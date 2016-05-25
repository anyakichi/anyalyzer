%{

%}

%token INFO
%token DEPTH
%token SELDEPTH
%token SCORE CP MATE UPPERBOUND LOWERBOUND
%token NODES
%token NPS
%token TIME
%token MULTIPV
%token PV
%token <int> INT
%token <Pmove.t> MOVE
%token EOL EOF

%start <((string * int) list * Pmove.t list) list option> main

%%

main:
  | v = value; EOF
    { Some v }
  | EOF
    { None }
  ;

value:
  | info = info
    { [info] }
  | info = info; l = value
    { info :: l }
  ;

info:
  | INFO; vs = info_values; PV; pv = info_pv; EOL
    { (vs, pv) }
  | INFO; vs = info_values; EOL
    { (vs, []) }
  | INFO; vs = info_values; PV; pv = info_pv; EOF
    { (vs, pv) }
  | INFO; vs = info_values; EOF
    { (vs, []) }
  ;

info_values:
  | v = info_value
    { [v] }
  | v = info_value; vs = info_values
    { v :: vs }
  ;

info_value:
  | DEPTH; i = INT
    { ("depth", i) }
  | SELDEPTH; i = INT
    { ("seldepth", i) }
  | SCORE; CP; i = INT
    { ("score cp", i) }
  | SCORE; CP; i = INT; UPPERBOUND
    { ("score cp", i) }
  | SCORE; CP; i = INT; LOWERBOUND
    { ("score cp", i) }
  | SCORE; MATE; i = INT
    { ("score mate", i) }
  | NODES; i = INT
    { ("nodes", i) }
  | NPS; i = INT
    { ("nps", i) }
  | TIME; i = INT
    { ("time", i) }
  | MULTIPV; i = INT
    { ("multipv", i) }
  ;

info_pv:
  | move = MOVE
    { [move] }
  | move = MOVE; moves = info_pv
    { move :: moves }
