module type LEXICAL = sig
    type token = Id of string | Key of string
    val scan: string -> token list
end

(** the list [alphas] defines the alphanumeric keywords like [if] and [let] while
    symbols list symbolic keywords like [(] and [)]. The two kinds of keyword are treated
    differently: 
        - A string of alphanumerics is scanned as far as possible until it is no longer
          followed by a letter or digit. It is classified as a keyword iff it belongs to alphas.
        - A string of symbolic characters is scanned until it matches some element of [symbols],
          or until it is not followed by another symbolic character. It is always classified as
          a keyword. For instance if [(] is in symbols then "((" is scanned as two "(" tokens,
          else as one "((" token. *)
module type KEYWORD = sig
    val alphas: string list
    val symbols: string list
end


(** [split f sq] is a pair of sequence s1, s2 such that s1 is the initial segment of sq for which
    f is true of all elements, and s2 is the remainder of sq. *)
let rec seq_split f sq =
    match sq () with
    | Seq.Nil -> (Seq.empty, Seq.empty)
    | Seq.Cons (x, sq) when f x -> let left, right = seq_split f sq in (Seq.cons x left, right)
    | Seq.Cons _ -> (Seq.empty, sq)
                          

module Lexical (Keyword : KEYWORD) : LEXICAL = struct
    type token = Id of string | Key of string

    let is_digit = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false

    let is_alpha char = 
        let code = Char.code char in 
        (65 <= code && code <= 90) (*Uppercase letters *)
        || (97 <= code && code <= 122) (*Lowercase letters *)
        || (48 <= code && code <= 57) (* Digits *)

    let is_punct char = 
        let code = Char.code char in 
        33 <= code && code <= 126 && not (is_alpha char)

    let rec scan_symbol sym chars =
        if List.mem sym Keyword.symbols 
        then (Key sym, chars)
        else match chars() with
            | Seq.Cons (c, chars') when is_punct c -> 
                    scan_symbol (sym ^ String.make 1 c) chars'
            | Seq.Nil | Seq.Cons _ -> (Key sym, chars)


    (** [scanning [] chars] is a list of all the tokens in chars *)
    let rec scanning toks chars =
        match chars() with
        | Seq.Nil -> List.rev toks
        | Seq.Cons (c, rest) ->
                if is_digit c
                then
                    let tok_seq, chars' = seq_split is_digit chars in
                    let tok = String.of_seq tok_seq in
                    scanning (Key tok :: toks) chars'
                else if is_alpha c
                then 
                    let tok_seq, chars' = seq_split is_alpha chars in
                    let tok = String.of_seq tok_seq in
                    if List.mem tok Keyword.alphas 
                    then scanning (Key tok :: toks) chars'
                    else scanning (Id tok :: toks) chars'
                else if is_punct c
                then 
                    let (tok, chars') = scan_symbol (String.make 1 c) rest in
                    scanning (tok :: toks) chars'
                else scanning toks rest (* Ignore eg whitespace *)
                     

    (** [scan s] is a list of all the tokens in s *)
    let scan s = scanning [] (String.to_seq s)
end

