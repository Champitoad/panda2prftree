open Printf
open Form

type rule_name_token = LogCst of logcst | Str of string
type rule_name = rule_name_token list
type free_rule =
  | IRule of rule_name
  | ERule of rule_name
type rule =
  | UnknownRule
  | FreeRule of free_rule
  | BoundRule of free_rule * int list
type assum =
  | FreeAssum of form
  | BoundAssum of form * int
type proof =
  | EmptyPrf
  | Assum of assum
  | Infer of proof list * rule * form

let string_of_proof prf =
  let rec aux n = function
    | EmptyPrf -> ""
    | Assum assum ->
      (match assum with
       | FreeAssum phi -> (string_of_form phi) ^ "\n"
       | BoundAssum (phi, ref) -> sprintf "[%s]^%d\n" (string_of_form phi) ref)
    | Infer (subprfs, rule, concl) ->
      sprintf "%s\n%s"
        (string_of_form concl)
        (String.concat ""
           (List.map
              (fun s -> (BatString.repeat "    " n) ^ s)
              (List.map (aux (n+1)) subprfs)))
  in aux 1 prf
