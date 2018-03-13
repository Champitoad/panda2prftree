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
  | prop = NAME { Pred (prop, []) }
  | op = zop { ZOp op }
  | LPAREN; phi = unparen_form; RPAREN { phi }
  ;

unparen_form:
  | prop = NAME { Pred (prop, []) }
  | pred = NAME; SPACE; vars = separated_nonempty_list(SPACE, NAME) { Pred (pred, vars) }
  | op = zop { ZOp op }
  | op = unop; phi = form { UnaryForm (op, phi) }
  | phi = form; op = binop; psi = form { BinaryForm (phi, op, psi) }
  | qt = quant; var = NAME; SPACE; phi = form { QuantForm (qt, var, phi) }
  ;

zop:
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
