open Printf

type pred = string
type var = string

type zop = [ `False ]
type unop = [ `Neg ]
type binop = [ `And | `Or | `Imply | `Equiv ]
type quant = [ `Exists | `Forall ]
type logcst = [ zop | unop | binop | quant ]

type form =
  | Pred of pred * var list
  | ZOp of zop
  | UnaryForm of unop * form
  | BinaryForm of form * binop * form
  | QuantForm of quant * var * form

let string_of_logcst = function
  | `False  -> "bottom"
  | `Neg    -> "not"
  | `And    -> "and"
  | `Or     -> "or"
  | `Imply  -> "imply"
  | `Equiv  -> "equiv"
  | `Exists -> "exists"
  | `Forall -> "forall"

let rec string_of_form = function
  | Pred (prop, [])            -> sprintf "%s" prop
  | Pred (pred, vars)          -> sprintf "(%s %s)" pred (String.concat " " vars)
  | ZOp op                     -> sprintf "%s" (string_of_logcst op)
  | UnaryForm (op, phi)        -> sprintf "(%s %s)" (string_of_logcst op) (string_of_form phi)
  | BinaryForm (phi, op, psi)  -> sprintf "(%s %s %s)" (string_of_form phi) (string_of_logcst op) (string_of_form psi)
  | QuantForm (qt, var, phi)   -> sprintf "(%s %s %s)" (string_of_logcst qt) var (string_of_form phi)

let rec height_of_form = function
  | Pred (pred, vars) -> 1
  | ZOp op -> 1
  | UnaryForm (op, phi) -> (height_of_form phi) + 1
  | BinaryForm (phi, op, psi) -> (max (height_of_form phi) (height_of_form psi)) + 1
  | QuantForm (qt, var, phi) -> (height_of_form phi) + 1

module Ordered =
struct
  type t = form
  let compare phi psi =
    match compare (height_of_form phi) (height_of_form psi) with
    | 0 -> compare (string_of_form phi) (string_of_form psi)
    | c -> c
end
