// scanner for a toy Pascal-like language

LONG    (abc)*d
SHORT   abc

%%

{LONG}           println("LONG: \(yytext)")

{SHORT}          println("SHORT: \(yytext)")

%%

fn yyinput() -> Array[Int] {
  let x : String = "abcabcabc"
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