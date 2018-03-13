open Form

exception UnequalLengths of int * int

(* A list of substitions giving l1 when applied to l2 *)
let get_substs l1 l2 =
  let rec aux l1' l2' sts =
    let len1 = List.length l1' in
    let len2 = List.length l2' in
    if len1 <> len2 then
      raise (UnequalLengths (len1, len2))
    else
      (match (l1', l2') with
       | ([], []) -> sts
       | (h1::t1, h2::t2) ->
         if h1 = h2 then
           aux t1 t2 sts
         else
           aux t1 t2 sts@[(h1, h2)]
       | _ -> [])
  in aux l1 l2 []

(* Whether list l1 can be obtained from list l2 by exactly 1 substitution *)
let ( $= ) l1 l2 =
  (List.length (get_substs l1 l2)) = 1

(* Whether formula phi' can be obtained from formula phi by exactly 1 substitution *)
let rec ( $=$ ) phi' phi =
  match (phi', phi) with
  | (Pred (pred', vars'), Pred (pred, vars)) ->
    pred' = pred && vars' $= vars
  | (ZOp op', ZOp op) ->
    op' = op
  | (UnaryForm (op', psi'), UnaryForm (op, psi)) ->
    op' = op && psi' $=$ psi
  | (BinaryForm (psi', op', chi'), BinaryForm (psi, op, chi)) ->
    op' = op && psi' $=$ psi && chi' $=$ chi
  | (QuantForm (qt', var', psi'), QuantForm (qt, var, psi)) ->
    qt' = qt && var' = var && psi' $=$ psi
  | _ -> false

(* List l with element n substituted to occurrences of o *)
let ( $< ) l (n, o) =
  List.map (fun e -> if e = o then n else e) l

(* Formula phi with term t substituted to variable var *)
let rec ( $<$ ) phi (t, var) =
  match phi with
  | Pred (pred, vars) -> Pred (pred, vars $< (t, var))
  | ZOp op -> phi
  | UnaryForm (op, psi) -> UnaryForm (op, psi $<$ (t, var))
  | BinaryForm (psi, op, chi) -> BinaryForm (psi $<$ (t, var), op, chi $<$ (t, var))
  | QuantForm (qt, var', psi) ->
    if t = var' || var = var' then
      phi
    else
      QuantForm (qt, var', psi $<$ (t, var))
