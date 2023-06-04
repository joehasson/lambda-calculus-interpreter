(* For declaring these operators it turns out that ocaml has weird rules about how
   the precedence of user-defined operators is determined: unlike in standard ML where
   we are allowed to declare the level ourselves, in ocaml it is determined by the
   characters in the operator:
       - operator beginning with ** have highest precedence, then * or /, then + or -
         , then @ or ^, then anything else
*)

exception SyntaxErr of string

module Parser (Lex: Lexer.LEXICAL) = struct
    let id = function
        | Lex.Id x :: xs -> x, xs
        | Lex.Key x :: _ -> SyntaxErr (x ^ " is not an identifier.") |> raise
        | [] -> raise (SyntaxErr "No more tokens")
    
    let key a = function
        | Lex.Key x :: xs when x=a -> x, xs
        | _ :: _ -> SyntaxErr ("not keyword " ^ a) |> raise
        | [] -> raise (SyntaxErr "No more tokens")
    
    let empty tokens = [], tokens
    
    let ( ||| ) ph1 ph2 tokens = 
        try ph1 tokens with SyntaxErr _ -> ph2 tokens
    
    exception Aborted of string
    let ( !! ) ph = fun tokens -> 
        try ph tokens  with SyntaxErr s -> raise (Aborted s)
    
    let ( -- ) ph1 ph2 = fun tokens ->
        let x, tokens' = ph1 tokens in
        let y, tokens'' = ph2 tokens' in
        (x, y), tokens''
    
    let ( *$-- ) a ph2 = fun tokens ->
        let (_, m), tokens = (key a -- !!ph2) tokens in m, tokens
    
    let ( @>>) ph f = fun tokens ->
        let (x, rest_tokens) = ph tokens in (f x, rest_tokens)
    
    let rec repeat ph = fun tokens ->
        (((ph -- repeat ph) @>> (fun (x, xs) -> x :: xs)) ||| empty) tokens

    (** [infixes ph prec_of apply] is a parser for infix operators, when supplied
        with arguments:
            - [ph], a parser which accepts the atomic phrases to be combined by
              the operators
            - [prec_of], a function which gives the precedence of operator tokens,
              returning a negative value for all keywords which are not infix
              operators.
            - [apply], a function which combines the meaning of phrases; [apply a x y]
              is the result of applying operator a to arguments x and y. *)
    let infixes ph prec_of apply =
        let rec over k toks = next k (ph toks)
        and next k = function
            | x, Lex.Key a :: toks when prec_of a >= k -> 
                    next k ((over (prec_of a) @>> apply a x) toks)
            | x, Lex.Key a :: toks -> 
                    x, Lex.Key a :: toks 
            | x, toks -> 
                    x, toks
        in over 0

    let reader ph a = 
        match a |> Lex.scan |> ph with
        | (x, []) -> x
        | (_, _) -> raise (SyntaxErr "Extra tokens in phrase.")
end


