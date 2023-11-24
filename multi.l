// scanner for a toy Pascal-like language

INT     [0-9]+

%x STRING
%x NORMAL
%%

<STRING>"\\\""  stringstore.val = stringstore.val + "\""

<STRING>"\""
    yybegin(NORMAL)
    let v = stringstore.val
    println("STRING: \(v)")

<STRING>.      stringstore.val = stringstore.val + yytext

<NORMAL>"\""  println("entering STRING"); stringstore.val = ""; yybegin(STRING)

<NORMAL>{INT}   println("INT: \(yytext)")

.              println("Unrecognized character: \(yytext)")

%%

let stringstore : Ref[String] = { val : "" }

fn yyinput() -> Array[Int] {
  let x : String = "123\"bbb\"456"
  let arr = Array::make(x.length(), 0)
  var idx = 0
  while idx < x.length() {
    arr[idx] = x[idx].to_int()
    idx = idx + 1
  }
  return arr
}

fn init {
    yybegin(NORMAL)
    yylex()
}