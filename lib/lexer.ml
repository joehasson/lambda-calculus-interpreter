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
          else as one "((" token. 

    Invariant I made up for exercises: comment_start and comment_end cannot begin with alphanumeric
    characters. *)
module type KEYWORD = sig
    val alphas: string list
    val symbols: string list
    val comment_start: string
    val comment_end: string
end


(** [split f sq] is a pair of sequence s1, s2 such that s1 is the initial segment of sq for which
    f is true of all elements, and s2 is the remainder of sq. *)
let rec seq_split f sq =
    match sq () with
    | Seq.Nil -> (Seq.empty, Seq.empty)
    | Seq.Cons (x, sq) when f x -> let left, right = seq_split f sq in (Seq.cons x left, right)
    | Seq.Cons _ -> (Seq.empty, sq)

let rec seq_take n sq = 
    let open Seq in
    match n, sq() with
    | 0, _ -> fun () -> Nil 
    | _, Nil -> failwith "Out of bounds"
    | n, Cons (x, xs) -> fun () -> Cons(x, seq_take (n-1) xs)
                          
let rec seq_drop n sq = 
    let open Seq in
    match n, sq() with
    | 0, _ -> sq
    | _, Nil -> failwith "Out of bounds"
    | n, Cons (_, xs) -> seq_drop (n-1) xs

module Lexical (Keyword : KEYWORD) = struct
    type token = Id of string | Key of string

    (** [ignore comments sq] is the sequence which is the result of ignoring every 
        character in [sq] which falls between the comment delimiters 
        [Keyword.comment_start] and [Keyword.comment_end] *)
    let ignore_comments sq =
        let open Seq in
        let c_start = Keyword.comment_start.[0] in
        let c_end = Keyword.comment_end.[0] in
        let rec accumulate_until_open sq =
            match sq () with
            | Cons (x, xs) when x=c_start ->
                    let to_compare = seq_take (String.length Keyword.comment_start) sq in
                    if String.of_seq to_compare = Keyword.comment_start
                    then drop_until_close sq
                    else fun () -> Cons (x, accumulate_until_open xs)
            | Cons (x, xs) -> fun () -> Cons(x, accumulate_until_open xs)
            | Nil -> fun () -> Nil
        and drop_until_close sq =
            match sq () with
            | Cons (x, xs) when x=c_end ->
                    let delim_length = String.length Keyword.comment_end in
                    let to_compare = seq_take delim_length sq in
                    if String.of_seq to_compare = Keyword.comment_end
                    then accumulate_until_open (seq_drop delim_length sq)
                    else drop_until_close xs
            | Cons (_, xs) -> drop_until_close xs
            | Nil -> failwith "Comment opened but never closed."
        in accumulate_until_open sq


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
        match (ignore_comments chars) () with
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

