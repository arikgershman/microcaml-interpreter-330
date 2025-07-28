open Types
open Utils

(* Provided functions - did not modify *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* MY CODE *)
(* Part 2: Parsing expressions *)
let rec parse_expr toks = let ret = 

  let rec parse_exp toks = 
    match lookahead toks with
    | Some Tok_Let -> parse_let toks
    | Some Tok_If -> parse_if toks
    | Some Tok_Fun -> parse_fun toks
    | _ -> parse_or toks

  and parse_let toks = 
    let rectok = match (lookahead_many toks 1) with Some Tok_Rec -> true | _ -> false in
    match (if rectok then (lookahead_many toks 2) else (lookahead_many toks 1)) with
    | Some Tok_ID(s) -> (let e1, t1 = parse_exp (if rectok 
                                                then (match_many toks ([Tok_Let; Tok_Rec; Tok_ID(s); Tok_Equal]))
                                                else (match_many toks (([Tok_Let; Tok_ID(s); Tok_Equal])))) in
                        match lookahead t1 with
                        | Some Tok_In -> let e2,t2 = parse_exp (match_token t1 Tok_In) in
                                         (Let(s, rectok, e1, e2), t2)
                        | _ -> raise(InvalidInputException ("let (in) failure")))
    | _ -> raise(InvalidInputException ("let failure"))

  and parse_if toks =
    let t = match_token toks Tok_If in
    let e1,t1 = parse_exp t in
    match lookahead t1 with
    | Some Tok_Then -> let t2 = match_token t1 Tok_Then in
                       let e2,t3 = parse_exp t2 in
                       match lookahead t3 with
                       | Some Tok_Else -> let t4 = match_token t3 Tok_Else in
                                          let e3,t5 = parse_exp t4 in
                                          (If(e1, e2, e3), t5)
                       | _ -> raise(InvalidInputException ("else failure"))
    | _ -> raise(InvalidInputException ("then failure"))

  and parse_fun toks =
    let var = lookahead_many toks 1 in 
    match var with 
    | Some Tok_ID(s) -> let t1 = (match_many toks ([Tok_Fun; Tok_ID(s); Tok_Arrow])) in
                        let e1,t2 = parse_exp t1 in
                        (Fun(s, e1), t2)
    | _ -> raise(InvalidInputException ("Function failure"))
      
  and parse_or toks =
    let e1,t1 = parse_and toks in 
    match lookahead t1 with 
    | Some Tok_Or -> let t2 = match_token t1 Tok_Or in
                let e2,t3 = parse_or t2 in
                (Binop(Or, e1, e2), t3)
    | _ -> (e1, t1)

  and parse_and toks = 
    let e1,t1 = parse_eqexp toks in 
    match lookahead t1 with 
    | Some Tok_And -> let t2 = match_token t1 Tok_And in
                 let e2,t3 = parse_and t2 in
                 (Binop(And, e1, e2), t3)
    | _ -> (e1, t1)

  and parse_eqexp toks = 
    let e1,t1 = parse_relexp toks in 
    match lookahead t1 with
    | Some Tok_Equal -> let t2 = match_token t1 Tok_Equal in
                   let e2,t3 = parse_eqexp t2 in
                   (Binop(Equal, e1, e2), t3)
    | Some Tok_NotEqual -> let t2 = match_token t1 Tok_NotEqual in
                      let e2,t3 = parse_eqexp t2 in
                      (Binop(NotEqual, e1, e2), t3)
    | _ -> (e1, t1)

  and parse_relexp toks = 
    let e1,t1 = parse_additive toks in 
    match lookahead t1 with
    | Some Tok_Less -> let t2 = match_token t1 Tok_Less in
                  let e2,t3 = parse_relexp t2 in
                  (Binop(Less, e1, e2), t3)
    | Some Tok_Greater -> let t2 = match_token t1 Tok_Greater in
                     let e2,t3 = parse_relexp t2 in
                     (Binop(Greater, e1, e2), t3)
    | Some Tok_LessEqual -> let t2 = match_token t1 Tok_LessEqual in
                       let e2,t3 = parse_relexp t2 in
                       (Binop(LessEqual, e1, e2), t3)
    | Some Tok_GreaterEqual -> let t2 = match_token t1 Tok_GreaterEqual in
                          let e2,t3 = parse_relexp t2 in
                          (Binop(GreaterEqual, e1, e2), t3)
    | _ -> (e1, t1)

  and parse_additive toks = 
    let e1,t1 = parse_mult toks in 
    match lookahead t1 with
    | Some Tok_Add -> let t2 = match_token t1 Tok_Add in
                      let e2,t3 = parse_additive t2 in
                      (Binop(Add, e1, e2), t3)
    | Some Tok_Sub -> let t2 = match_token t1 Tok_Sub in
                      let e2,t3 = parse_additive t2 in
                      (Binop(Sub, e1, e2), t3)
    | _ -> (e1, t1)

  and parse_mult toks = 
    let e1,t1 = parse_con toks in 
    match lookahead t1 with
    | Some Tok_Mult -> let t2 = match_token t1 Tok_Mult in
                   let e2,t3 = parse_mult t2 in
                   (Binop(Mult, e1, e2), t3)
    | Some Tok_Div-> let t2 = match_token t1 Tok_Div in
                      let e2,t3 = parse_mult t2 in
                      (Binop(Div, e1, e2), t3)
    | _ -> (e1, t1)

  and parse_con toks =
    let e1,t1 = parse_unary toks in
    match lookahead t1 with
    | Some Tok_Concat -> let t2 = match_token t1 Tok_Concat in
                    let e2,t3 = parse_con t2 in
                    (Binop(Concat, e1, e2), t3)
    | _ -> (e1,t1)

  and parse_unary toks =
    match lookahead toks with
    | Some Tok_Not -> let e1,t1 = parse_unary (match_token toks Tok_Not) in
                 (Not(e1), t1)
    | _ -> (parse_app toks)

  and parse_app toks =
    let e1,t1 = parse_select toks in 
    match lookahead t1 with
    | Some Tok_Int(c) -> let e2,t2 = parse_prim t1 in 
                         (App(e1, e2), t2)
    | Some Tok_Bool(b) -> let e2,t2 = parse_prim t1 in 
                          (App(e1, e2), t2)
    | Some Tok_String(s) -> let e2,t2 = parse_prim t1 in 
                            (App(e1, e2), t2)
    | Some Tok_ID(s) -> let e2,t2 = parse_prim t1 in 
                        (App(e1, e2), t2)
    | Some Tok_LParen -> let e2,t2 = parse_prim t1 in 
                         (App(e1, e2), t2)
    | Some Tok_LCurly -> let e2,t2 = parse_prim t1 in 
                         (App(e1, e2), t2)
    | _ -> (e1,t1)

  and parse_select toks = 
    let e1,t1 = parse_prim toks in
    match lookahead t1 with
    | Some Tok_Dot -> (let t2 = match_token t1 Tok_Dot in
                      match lookahead t2 with
                      | Some Tok_ID(lb) -> let t3 = match_token t2 (Tok_ID(lb)) in
                                           (Select(Lab(lb), e1), t3)
                      | _ -> raise(InvalidInputException ("select failure")))
    | _ -> (e1,t1)
    
  and parse_prim toks = 
    match lookahead toks with
    | Some Tok_Int(c) -> let t = match_token toks (Tok_Int(c)) in 
                         (Int(c), t)
    | Some Tok_Bool(b) -> let t = match_token toks (Tok_Bool(b)) in  
                          (Bool(b), t)
    | Some Tok_String(s) -> let t = match_token toks (Tok_String(s)) in  
                            (String(s), t)
    | Some Tok_ID(s) -> let t = match_token toks (Tok_ID(s)) in  
                        (ID(s), t)
    | Some Tok_LParen -> (let e1,t1 = parse_exp (match_token toks Tok_LParen) in
                         match lookahead t1 with
                         | Some Tok_RParen -> let t2 = match_token t1 Tok_RParen in
                                              (e1, t2)
                         | _ -> raise(InvalidInputException ("parentheses failure")))
    | _ -> (parse_record toks)

  and parse_record toks =
    let t = match_token toks Tok_LCurly in
    match (lookahead t) with
    | Some Tok_RCurly -> let t1 = match_token t Tok_RCurly in (Record([]), t1)
    | _ -> let e1,t1 = parse_recordbody t in
                       match lookahead t1 with
                       | Some Tok_RCurly -> let t2 = match_token t1 Tok_RCurly in
                                            (Record(e1), t2)
                       | _ -> raise(InvalidInputException ("record brackets failure"))

  and parse_recordbody toks =
    match (lookahead toks) with
    | Some Tok_ID(s) -> (let e1,t1 = parse_exp (match_many toks ([Tok_ID(s); Tok_Equal])) in 
                        match (lookahead t1) with
                        | Some Tok_Semi -> let t2 = match_token t1 Tok_Semi in 
                        let e2, t3 = parse_recordbody t2 in ((Lab(s),e1)::e2, t3)
                        | _ -> ([(Lab(s),e1)], t1))

    | _ -> raise(InvalidInputException ("record body failure"))

  in parse_exp toks
  in match ret with 
  | (a,b) -> match b with 
             [] -> (b,a)
             | h::_ -> match h with 
                       | Tok_DoubleSemi -> (b,a)
                       | _ -> raise(InvalidInputException ("invalid"))

(* Part 3: Parsing mutop *)
let rec parse_mutop toks = 
    match lookahead toks with
    | Some Tok_Def -> (match (lookahead_many toks 1) with
                      | Some Tok_ID(s) -> (let t = match_many toks ([Tok_Def; Tok_ID(s); Tok_Equal]) in
                                          let t1,e1 = parse_expr t in
                                          match lookahead t1 with
                                          | Some Tok_DoubleSemi -> let t2 = match_token t1 Tok_DoubleSemi in 
                                                                   match t2 with [] -> (t2, Def(s,e1)) | h::_ -> raise(InvalidInputException ("leftover"))
                                          | _ -> raise(InvalidInputException ("doesn't end with ;;")))
                      | _ -> raise(InvalidInputException ("no ID after def")))
    | Some Tok_DoubleSemi -> (let t = match_token toks Tok_DoubleSemi in
                             (t, NoOp))
    | _ -> (let t1,e1 = parse_expr toks in
           match lookahead t1 with
           | Some Tok_DoubleSemi -> let t2 = match_token t1 Tok_DoubleSemi in
                               (t2, Expr(e1))
           | _ -> raise(InvalidInputException ("doesn't end with ;;")))