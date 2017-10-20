open Printf

type atom = string
type pred = string
type var = string

type const = [ `False ]
type unop = [ `Neg ]
type binop = [ `And | `Or | `Imply | `Equiv ]
type quant = [ `Exists | `Forall ]
type logconst = [ const | unop | binop | quant ]

type form =
  | Atom of atom
  | Pred of pred * var list
  | Const of const
  | UnaryForm of unop * form
  | BinaryForm of form * binop * form
  | QuantForm of quant * var * form

let string_of_logconst = function
  | `False  -> "bottom"
  | `Neg    -> "not"
  | `And    -> "and"
  | `Or     -> "or"
  | `Imply  -> "imply"
  | `Equiv  -> "equiv"
  | `Exists -> "exists"
  | `Forall -> "forall"

let rec string_of_form = function
  | Atom atom                  -> sprintf "%s" atom
  | Pred (pred, vars)          -> sprintf "(%s %s)" pred (String.concat " " vars)
  | Const cst                  -> sprintf "%s" (string_of_logconst cst)
  | UnaryForm (op, phi)        -> sprintf "(%s %s)" (string_of_logconst op) (string_of_form phi)
  | BinaryForm (phi, op, psi)  -> sprintf "(%s %s %s)" (string_of_form phi) (string_of_logconst op) (string_of_form psi)
  | QuantForm (qt, var, phi)   -> sprintf "(%s %s %s)" (string_of_logconst qt) var (string_of_form phi)

let rec height_of_form = function
  | Atom atom -> 1
  | Pred (pred, vars) -> 1
  | Const cst -> 1
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
