%token FALSE
%token NEG
%token AND OR IMPLY EQUIV
%token EXISTS FORALL
%token <string> NAME
%token LPAREN RPAREN
%token SPACE EOF
%{
    open Form
%}
%start <Form.form option> prog
%%

prog:
  | phi = form { Some phi }
  | EOF { None }
  ;

form:
  | atom = NAME { Atom atom }
  | cst = const { Const cst }
  | LPAREN; phi = unparen_form; RPAREN { phi }
  ;

unparen_form:
  | atom = NAME { Atom atom }
  | pred = NAME; SPACE; vars = separated_nonempty_list(SPACE, NAME) { Pred (pred, vars) }
  | cst = const { Const cst }
  | op = unop; phi = form { UnaryForm (op, phi) }
  | phi = form; op = binop; psi = form { BinaryForm (phi, op, psi) }
  | qt = quant; var = NAME; SPACE; phi = form { QuantForm (qt, var, phi) }
  ;

const:
  | FALSE { `False }

unop:
  | NEG { `Neg }
  ;
binop:
  | AND     { `And }
  | OR      { `Or }
  | IMPLY   { `Imply }
  | EQUIV   { `Equiv }
  ;
quant:
  | EXISTS  { `Exists }
  | FORALL  { `Forall }
  ;
