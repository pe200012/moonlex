# moonlex

## Basic types

commit: 87f0aa60c9c52dbecc436ade8f446c67d542030e

根据 proposal 的内容，将 moonlex 数据结构定义为:
```haskell
data Spec {
  pragma: [(String      --  moonlex 命令选项
          , String)]    --  moonlex 命令参数
  rules: [([String]     --  语法规则规定的 start condition
          , Regex
          , Action)]    --  用户代码动作
  userCode: String      --  全局代码定义
}
```

Regex 定义如下
```haskell
data Regex
    = Char Word8
    | Concatenation [Regex]
    | Choice [Regex]
    | WordSet [Word8]
    | Complement [Word8]
    | ZeroOrMore Regex
    | Option Regex
    | Any
    | Group Regex
    | EndOfString
    | EmptySet
    | Epsilon
```

`p{3,5}` 特定次数重复被解糖为 concat，`x+` 解为 `xx*`

## Parsing

commit: be23a5f964c5cc39907a28bbfbc04e1b6e7bbb8e

对 moonlex .l 文件的解析借鉴了 [flex](https://www.cs.princeton.edu/~appel/modern/c/software/flex/flex.html#SEC6) 的文档和 SO 上对 Regex BNF 的[回答](https://stackoverflow.com/questions/265457/regex-bnf-grammar)

我们规定：应用 %% 分割不同的区域，总共有*定义区，规则区，全局定义区*三个区域

在所有区域中都可以使用注释 `// ...`

定义区有两种语法：

- 以 % 开头的命令: `%somedir somearg` 这个阶段命令只有两个有用(%s, %x 详细的下面代码生成的时候解析)
- 以 ASCII 标识符+空格+Regex 表达式组成的规则缩写，仅可以用在之后的规则中，像下面这种是不允许的：
  ```
  DIGIT [0-9]+
  FLOAT {DIGIT}"."{DIGIT}    -- 未识别的 regex
  ```

规则区由若干规则和与其对应的用户动作组成。规则的构成如下：
```
(<SC0, SC1...>)?REGEX
```
start condition 是可选的。当用户不写明时默认为 INITIAL。sc 可以用来实现 modal lexer，在不同自动机间切换。切换的函数这样调用 `yybegin(SC)`

start condition 分四种：
- INITIAL: 无 SC 时默认
- inclusive: 指定 SC 时默认为 inclusive sc，也可用 `%s SC` 手动指定。 inclusive sc 自动**包含**了 INITIAL 中的所有规则
- exclusive: `%x SC` 指定。不包含 INITIAL 的规则
- star: 特殊规则 `<*>`。被 star 标记的规则被所有 sc 包含

用户动作解析：为了简化解析，我们规定用户动作之前需要有缩进。当没有缩进时进行下一个规则的解析。例：
```flex
<STRING>"\\\""  stringstore.val = stringstore.val + "\""

<STRING>"\""
    yybegin(NORMAL)
    let v = stringstore.val
    println("STRING: \(v)")

<STRING>.      stringstore.val = stringstore.val + yytext
```

全局代码：遇到 %% 后直接取所有剩下的字符串

## Automata generation

commit: 3d71c5ae792cb98edf258bd484370d09ce9c904c

简单的 Thompson 法构造 NFA，然后转换为 DFA

commit: a8552d24607bd51d13ed44a4539a8b9eada3628b

DFA 简化，减少状态数量。~本来想用 10.1016/j.ipl.2011.12.004 写的，但是 Haskell 写 stateful program 太痛苦了，就用简单的 iteratively deepening 方法了~

## Code generation

commit: e644b3f536eb1d869e28eb58c0635e2ea6cf1aad

初步代码生成。按照 proposal 实现朴素的代码转移。

lexer 使用贪心策略，多个规则同时匹配成功时选择长度最长的，长度相当时选择靠前的规则。每个规则生成的代码如下：
```rs
// 表示当前状态被第几个规则接受，-1 表示 nonfinal
let yyaccepted0 : Array[Int] = [0,1,2,-1,-1...]

// int yytable[INPUT][STATE] 状态转移表，-1 表示没有对应的边
let yytable0 : Array[Array[Int]] = #{yytable}

// 回溯用的栈
let yyfailed0 : Array[List[Int]] = Array::make(#{numStates}, List::Nil)
...
// yyfinal 最后一个到达的终态
// yymark 用于回溯保存 buffer offset 用的标记
// yytext 被匹配的文本
let (yyfinal, yymark, yytext) = yyloop(yybuf, yytable0, yyaccepted0, yyfailed0, #{startId})
              if yyfinal != (-1) {
                yyoffset.val = yymark
                match yyfinal {
                  (-1) => { yyerror() }
                  #{T.intercalate "\n                " matches}
                  _ => abort("impossible")
                }
              } else {
                yyerror()
              }
```

预定义的全局函数如下
```rs
// buffer offset
let yyoffset : Ref[Int] = { val : 0 }

// 在每个状态里面，我们检查之前是否经过当前的 offset，如果有
// 说明我们之前匹配失败了，这里不需要继续进行 lexing
fn failed_previously(yyfailedinput : List[Int]) -> Bool {
  match yyfailedinput {
    List::Nil => false
    List::Cons(x, xs) => {
      if x == yyoffset.val {
        true
      } else {
        failed_previously(xs)
      }
    }
  }
}

// main lexing loop
fn yyloop0(yybuf : Array[Int],
           yytable : Array[Array[Int]],
           yyaccepted: Array[Int],
           yyfailed: Array[List[Int]],
           yystart : Int) -> (Int, Int, String) {
  fn go(yycur : Int,
        yyacc : String,
        yyfinal : List[Int],
        yymark : List[Int],
        yytext : String) -> (List[Int], List[Int], String) {
    if yyoffset.val >= yybuf.length() {
      return (yyfinal, yymark, yytext)
    }

    // 检查之前是否匹配失败
    if failed_previously(yyfailed[yycur]) {
      return (yyfinal, yymark, yytext)
    }
    let __yy_input__ = yybuf[yyoffset.val]
    let yynext = yytable[__yy_input__][yycur]

    // 有没有对应的转移边
    if yynext != (-1) {
      let curoffset = yyoffset.val
      yyoffset.val = yyoffset.val + 1
      let __yy_inputchar__ = Char::from_int(__yy_input__)
      let newyyacc = yyacc + "\\(__yy_inputchar__)"
      let act = yyaccepted[yynext]
      if act != -1 {
        // 只保留最后一个接受的状态
        go(yynext, newyyacc, List::Cons(yynext, List::Nil), List::Cons(yyoffset.val, List::Nil), newyyacc)
      } else {
        go(yynext, newyyacc, List::Cons(yycur, yyfinal), List::Cons(curoffset, yymark), yytext)
      }
    } else {
      return (yyfinal, yymark, yytext)
    }
  }
  let (finals, marks, yytext) = go(yystart, "", List::Cons(-1, List::Nil), List::Cons(-1, List::Nil), "")

  // backtracking 函数，将栈中的所有元素标记为失败。将栈底的成功的状态返回
  fn backtracking(fs : List[Int],
                  ms : List[Int]) -> (Int, Int) {
      match (fs, ms) {
        (List::Nil, List::Nil) => (-1, -1)
        (List::Cons(-1, List::Nil), List::Cons(-1, List::Nil)) => (-1, -1)
        (List::Cons(f, List::Nil), List::Cons(m, List::Nil)) => (yyaccepted[f], m)
        (List::Cons(nf, rfs), List::Cons(nm, rms)) => {
          yyfailed[nf] = List::Cons(nm, yyfailed[nf])
          backtracking(rfs, rms)
        }
        _ => abort("yyloop backtracking")
      }
  }
  let (final, mark) = backtracking(finals, marks)
  (final, mark, yytext)
}
```

commit: dcb1be1aadac71c874ecbbc43d90d5e7d8688d34

tabulation on need. 根据 10.1145/276393.276394，只在能被 final 访问的 nonfinal 状态上回溯

commit: 2ee7066dcac95e878f143910389781609f9452be

shift DFA。基本前提：1）better locality when fit in cache 2) break loop dependency chain 3) do index with ALU operation

根据 [shift_dfa.md](https://gist.github.com/pervognsen/218ea17743e1442e59bb60d29b1aa725) 将小于 10 状态的自动机其转移表压缩成 64 位整数。减少指令开销。

生成代码如下：
```rs
...
// uint64_t yytable[INPUT] 状态转移表，每个状态占 uint64 中的 6 位。0b111111 表示无对应边
let yytable0 : Array[Int64] = #{yytable}
...
fn yyloop1(yybuf : Array[Int],
           yytable : Array[Int64],
           yyaccepted: Array[Int],
           yyfailed: Array[List[Int]],
           yybounded : Array[Bool],
           yystart : Int) -> (Int, Int, String) {
...
// uint8_t run(const uint8_t *start, const uint8_t *end, uint8_t state) {
//    for (const uint8_t *s = start; s != end; s++) {
//        uint64_t row = table[*s];
//        state = (row >> state) & ((1 << BITS_PER_STATE) - 1);
//    }
//    return state;
// }
 let yynext = yytable[__yy_input__].lsr(yycur.land(63).to_int64()).land(63L).to_int()
...
}
```

## Some examples

commit: ff9aa5ab0833cbefc50827c9930b124687ce8428