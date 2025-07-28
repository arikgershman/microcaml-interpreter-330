open Types

(* Provided functions - did not modify *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* MY CODE *)
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  | Int(c) -> Int(c)

  | Bool(b) -> Bool(b)

  | String(s) -> String(s)

  | ID(x) -> lookup env x

  | Not(e0) -> (let e1 = (eval_expr env e0) in
               match e1 with 
               | Bool(b) -> if b then Bool(false) else Bool(true)
               | _ -> raise(TypeError "Expected type bool"))

  | Binop(op,e0,e00) -> (let e1 = (eval_expr env e0) in
                        let e2 = (eval_expr env e00) in
                        match op with
                        | Equal -> (match e1,e2 with 
                                    | Closure(env,var,ex),_ -> raise(TypeError "Cannot compare closures") 
                                    | _,Closure(env,var,ex) -> raise(TypeError "Cannot compare closures") 
                                    | _ -> Bool(e1=e2))
                        | NotEqual -> (match e1,e2 with 
                                      | Closure(env,var,ex),_ -> raise(TypeError "Cannot compare closures") 
                                      | _,Closure(env,var,ex) -> raise(TypeError "Cannot compare closures") 
                                      | _ -> Bool(e1<>e2))
                        | _ -> (match e1,e2 with
                               | Int(a),Int(b) -> (match op with
                                                  | Add -> Int(a+b)
                                                  | Sub -> Int(a-b)
                                                  | Mult -> Int(a*b)
                                                  | Div -> if b=0 then raise(DivByZeroError) else Int(a/b)
                                                  | Greater -> Bool(a>b)
                                                  | Less -> Bool(a<b)
                                                  | GreaterEqual -> Bool(a>=b)
                                                  | LessEqual -> Bool(a<=b)
                                                  | _ -> raise(TypeError "Invalid op for type Int")
                                                  )
                                | String(a),String(b) -> (match op with
                                                         | Concat -> String(a^b)
                                                         | _ -> raise(TypeError "Invalid op for type String")
                                                         )
                                | Bool(a),Bool(b) -> (match op with
                                                     | Or -> Bool(a||b)
                                                     | And -> Bool(a&&b)
                                                     | _ -> raise(TypeError "Invalid op for type Bool")
                                                     )
                                | _ -> raise(TypeError "Invalid type")))

    | If(e0,e2,e3) -> (let e1 = (eval_expr env e0) in
                      match e1 with
                      | Bool(g) -> if g then (eval_expr env e2) else (eval_expr env e3)
                      | _ -> raise(TypeError "Expected type bool"))

    | Let(x,recc,e1,e2) -> (if not recc then let ex = (eval_expr env e1) in
                                            (eval_expr (extend env x ex) e2)
                                        else let nenv = (extend_tmp env x) in let v = (eval_expr nenv e1) in 
                                             let r = (update nenv x v) in (eval_expr nenv e2))

    | Fun(x,e1) -> (Closure(env, x, e1))

    | App(e1,e2) -> (match (eval_expr env e1) with
                    | Closure(a,x,e) -> let v = (eval_expr env e2) in (eval_expr (extend a x v) e)
                    | _ -> raise(TypeError "Not a function"))

    | Record(lst) -> (Record(lst))

    | Select(lb,ex) -> (match (eval_expr env ex) with
                       | Record(lst) -> (let (l,e) = (let rec helper lst = 
                                                 match lst with 
                                                 | [] -> raise(SelectError "Label not found")
                                                 | x::xs -> if (match x with (a,b) -> a=lb) then x else (helper xs)
                                                 in helper lst) in 
                                        (eval_expr env e))
                       | _ -> raise(TypeError "Not a record")
                       )
                  
                       
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def(var,e) -> (let nenv = extend_tmp env var in 
                  let ex = (eval_expr nenv e) in let r = (update nenv var ex) in
                  (nenv, Some ex))

  | Expr(e) -> (env, Some (eval_expr env e))

  | NoOp -> (env, None)