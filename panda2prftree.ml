open Printf
open Form
open Proof

module FormMap = Map.Make(Ordered)
type assum_binder = { mutable refs: int FormMap.t; ref: int ref }

let infer_rule concl prems assum_binder =
  let bind_assum phi =
    let ref = !(assum_binder.ref) + 1 in
    assum_binder.refs <- FormMap.add phi ref assum_binder.refs;
    assum_binder.ref := ref;
    ref
  in
  let irule =
    match concl with
    | ZOp op ->
      (match op with
       | `False ->
         (match prems with
          | [phi; UnaryForm (`Neg, phi')] when phi = phi' ->
            FreeRule (IRule [LogCst `False])
          | _ -> UnknownRule))
    | UnaryForm (op, phi) ->
      (match op with
       | `Neg ->
         (match prems with
          | [ZOp `False] ->
            let ref_phi = bind_assum phi in
            BoundRule (IRule [LogCst `Neg], [ref_phi])
          | _ -> UnknownRule))
    | BinaryForm (phi, op, psi) ->
      (match op with
       | `And ->
         (match prems with
          | [phi'; psi'] when phi' = phi && psi' = psi ->
            FreeRule (IRule [LogCst `And])
          | _ -> UnknownRule)
       | `Or ->
         (match prems with
          | [phi'] when phi' = phi ->
            FreeRule (IRule [Str "l"; LogCst `Or])
          | [psi'] when psi' = psi ->
            FreeRule (IRule [Str "r"; LogCst `Or])
          | _ -> UnknownRule)
       | `Imply ->
         (match prems with
          | [psi'] when psi' = psi ->
            let ref_phi = bind_assum phi in
            BoundRule (IRule [LogCst `Imply], [ref_phi])
          | _ -> UnknownRule)
       | `Equiv ->
         (match prems with
          | [BinaryForm (phi', `Imply, psi'); BinaryForm (psi'', `Imply, phi'')]
            when phi' = phi && psi' = psi && phi' = phi'' && psi' = psi'' ->
            FreeRule (IRule [LogCst `And])
          | _ -> UnknownRule))
    | _ -> UnknownRule
  in match irule with
  | UnknownRule ->
    (match prems with
     | [ZOp `False] ->
       FreeRule (ERule [LogCst `False])
     | [UnaryForm (`Neg, UnaryForm (`Neg, phi))] when phi = concl ->
       FreeRule (ERule [LogCst `Neg; LogCst `Neg])
     | [BinaryForm (phi, `And, psi)] when phi = concl ->
       FreeRule (ERule [Str "l"; LogCst `And])
     | [BinaryForm (phi, `And, psi)] when psi = concl ->
       FreeRule (ERule [Str "r"; LogCst `And])
     | [BinaryForm (phi, `Or, psi); chi; chi'] when chi = concl && chi = chi' ->
       let ref_phi = bind_assum phi in
       let ref_psi = bind_assum psi in
       BoundRule (ERule [LogCst `Or], [ref_phi; ref_psi])
     | [BinaryForm (phi, `Imply, psi); phi'] when psi = concl && phi = phi' ->
       FreeRule (ERule [LogCst `Imply])
     | _ -> UnknownRule)
  | r -> r

exception Malformed_formula of string
exception Malformed_proofnode
exception Invalid_tag_name

let form_of_string str =
  let lexbuf = Lexing.from_string str in
  match (FormParser.prog FormLexer.read lexbuf) with
  | Some phi -> phi
  | None -> raise (Malformed_formula str)

let get_prems xml_list =
  let aux = function
    | Xml.Element ("proofnode", [], chlds) ->
      (match chlds with
       | [Xml.Element ("formula", [], [Xml.PCData formula]); _; _] -> form_of_string formula
       | _ -> raise Malformed_proofnode)
    | _ -> raise Invalid_tag_name
  in List.map aux xml_list

let proof_of_panda xml =
  let rec aux assum_binder = function
    | Xml.Element ("proofnode", [], chlds) ->
      (match chlds with
       | [Xml.Element ("formula", [], [Xml.PCData formula]);
          Xml.Element ("graphicalposition", [], _);
          Xml.Element ("children", [], children)] ->
         (let concl = form_of_string formula in
          match children with
          | [] ->
            (let ref =
               try FormMap.find concl assum_binder.refs with
               | Not_found -> 0
             in match ref with
             | 0 -> Assum (FreeAssum concl)
             | n -> Assum (BoundAssum (concl, n)))
          | _ ->
            (let rule = infer_rule concl (get_prems children) assum_binder in
             Infer (List.map
                      (aux { refs = assum_binder.refs;
                             ref = assum_binder.ref })
                      children,
                    rule, concl)))
       | _ -> raise Malformed_proofnode)
    | _ -> raise Invalid_tag_name
  in aux { refs = FormMap.empty; ref = ref 0 } xml

let proofs_of_panda = function 
  | Xml.Element ("proofs", [], chlds) -> List.map proof_of_panda chlds
  | _ -> raise Invalid_tag_name

let prftree_of_proof prf =
  let string_of_logcst = function
    | `False  -> "\\abs"
    | `Neg    -> "\\neg"
    | `And    -> "\\land"
    | `Or     -> "\\lor"
    | `Imply  -> "\\lto"
    | `Equiv  -> "\\lequiv"
    | `Exists -> "\\exists"
    | `Forall -> "\\forall"
  in 
  let string_of_form form =
    let rec aux n = function
      | Pred (prop, []) ->
        (match Str.string_match (Str.regexp "[a-z]") prop 0 with
         | true -> sprintf "\\mathsf{%s}" prop
         | false -> prop)
      | Pred (pred, vars)          -> sprintf "%s(%s)" pred (String.concat "," vars)
      | ZOp op                     -> sprintf "%s" (string_of_logcst op)
      | _ as phi                   -> if n = 0 then unparen n phi else "(" ^ (unparen n phi) ^ ")"
    and unparen n = function
      | UnaryForm (op, phi)        -> sprintf "%s %s" (string_of_logcst op) (aux (n+1) phi)
      | BinaryForm (phi, op, psi)  -> sprintf "%s %s %s" (aux (n+1) phi) (string_of_logcst op) (aux (n+1) psi)
      | QuantForm (qt, var, phi)   -> sprintf "%s %s (%s)" (string_of_logcst qt) var (aux (n+1) phi)
      | _ as phi                   -> aux n phi
    in aux 0 form
  in
  let string_of_rule_name rn =
    String.concat ""
      (List.map
         (fun token ->
            match token with
            | LogCst cst -> string_of_logcst cst
            | Str s -> s)
         rn)
  in
  let string_of_rule r =
    let rec str refs = String.concat "," (List.map string_of_int refs) in
    (match r with
     | UnknownRule -> "?"
     | FreeRule (IRule rn) -> sprintf "\\iruleI{%s}" (string_of_rule_name rn)
     | FreeRule (ERule rn) -> sprintf "\\iruleE{%s}" (string_of_rule_name rn)
     | BoundRule (IRule rn, refs) -> sprintf "\\iruleI[%s]{%s}" (str refs) (string_of_rule_name rn)
     | BoundRule (ERule rn, refs) -> sprintf "\\iruleE[%s]{%s}" (str refs) (string_of_rule_name rn))
  in
  let rec aux n = function
    | EmptyPrf -> ""
    | Assum assum ->
      (match assum with
       | FreeAssum phi -> sprintf "\\prfassumption{%s}" (string_of_form phi)
       | BoundAssum (phi, ref) -> sprintf "\\prfboundedassumption<%d>{%s}" ref (string_of_form phi))
    | Infer (subprfs, rule, concl) ->
      let indent = (BatString.repeat "    " n) in
      sprintf "\\prftree[r]{%s}\n%s\n%s"
        (string_of_rule rule)
        (String.concat "\n"
           (List.map
              (fun s -> sprintf "%s{%s}" indent s)
              (List.map (aux (n+1)) subprfs)))
        (sprintf "%s{%s}" indent (string_of_form concl))
  in sprintf "\\begin{displaymath}\n    %s\n\\end{displaymath}\n" (aux 2 prf)

let prftrees_of_proofs prfs = String.concat "\n" (List.map prftree_of_proof prfs)

let () =
  let xml = Xml.parse_in stdin in
  let prfs = proofs_of_panda xml in
  let header = BatFile.with_file_in "header.tex" BatIO.read_all in
  let footer = BatFile.with_file_in "footer.tex" BatIO.read_all in
  print_string (String.concat "\n" [header; prftrees_of_proofs prfs; footer])
