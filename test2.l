
// scanner for a toy Pascal-like language

DIGIT [0-9]

%%

[1-9]{DIGIT}+       println("Number: \(yytext)")

.              println("Unrecognized character: \(yytext)")

%%

fn yyinput() -> Array[Int] {
    return [49,50,51,52]
}

fn init {
    yylex()
}