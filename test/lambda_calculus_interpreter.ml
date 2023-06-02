open OUnit2
open Lambda_calculus_interpreter

module MyKeywords: Lexer.KEYWORD = struct
    let alphas = ["if"; "cons"; "add"]
    let symbols = ["("; ")"; "["; "]"]
end

module MyLexer = Lexer.Lexical(MyKeywords)


let tests = "test suite for lexer" >::: [
    "Empty string" >:: (fun _ -> 
        assert_equal [] (MyLexer.scan ""));
]
