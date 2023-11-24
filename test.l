// scanner for a toy Pascal-like language
%option nowrap

DIGIT    [0-9]
ID       [a-z][a-z0-9]*

%%

{DIGIT}+    {
                println("An integer: \(yytext)")
            }

{DIGIT}+"."{DIGIT}*        {
                                println("A float: \(yytext)")
                           }

if|then|begin|end|procedure|function        {
                                                println("A keyword: \(yytext)")
                                            }

{ID}       println("An identifier: \(yytext)")

"+"|"-"|"*"|"/" println("An operator: \(yytext)")

"{"[^}\n]*"}"     () // eat up one-line comments

[ \t\n]+          () // eat up whitespace

.              println("Unrecognized character: \(yytext)")

%%

fn yyinput() -> Array[Int] {
  let x : String = "if then else 1 1.234 1+2+3 aabb   "
  let arr = Array::make(x.length(), 0)
  var idx = 0
  while idx < x.length() {
    arr[idx] = x[idx].to_int()
    idx = idx + 1
  }
  return arr
}

fn init {
    yylex()
}