%{

open StdLabels

open ShogiKifu
open Pmove

let make tags position moves =
  let pos = Position.copy position in
  let moves =
    List.fold_left moves ~init:[] ~f:(fun a x ->
      let sq0opt, sq, koma, time = x in
      let m =
        match sq0opt with
        | None -> Drop(sq, koma)
        | Some sq0 -> Move' (sq0, sq, koma)
      in
      let move = Position.move pos m in
      (Move.update move ~time ()) :: a)
    |> List.rev
  in
  { tags; position; moves }
;;

%}

%token <Koma.t> KOMA
%token <Square.t> SQUARE
%token KOMADAI
%token <Color.t> COLOR
%token <int> TIME
%token <string * string> TAG
%token PI
%token P1 P2 P3 P4 P5 P6 P7 P8 P9
%token PEMPTY
%token COMMA
%token EOL
%token EOF

%start <ShogiKifu.t> main

%%

main:
  | tags = tags; position = position; moves = moves; EOF
    { make tags position moves }
  ;

delimiter:
  | EOL                         {}
  | COMMA                       {}
  ;

tags:
  |                             { [] }
  | tag = TAG; tags = tags      { tag :: tags }
  ;

position:
  | p = position0; color = COLOR; delimiter
    { p.Position.teban <- color; p }
  ;

position0:
  | p = pi      { p }
  | p = pmap    { p }
  ;

pi:
  | PI
    { Position.make () }
  | p = pi; sq = SQUARE; KOMA
    { Position.clear p sq; p }
  | p = pi; delimiter
    { p }
  ;

pmap:
  | P1; r1 = prow; P2; r2 = prow; P3; r3 = prow; P4; r4 = prow; P5; r5 = prow;
    P6; r6 = prow; P7; r7 = prow; P8; r8 = prow; P9; r9 = prow
    {
      let p = Position.make () in
      let pmap = [r1; r2; r3; r4; r5; r6; r7; r8; r9] in
      List.iteri pmap ~f:(fun i row ->
        List.iteri row ~f:(fun j sq ->
          match sq with
          | None -> ()
          | Some v ->
              Position.set p (Square.make (9 - j) (i + 1)) v));
      p
    }
  ;

prow:
  | psq; psq; psq; psq; psq; psq; psq; psq; psq; delimiter
    { [$1; $2; $3; $4; $5; $6; $7; $8; $9] }
  ;

psq:
  | PEMPTY                      { None }
  | color = COLOR; koma = KOMA  { Some (color, koma) }
  ;

moves:
  |                             { [] }
  | m = move; ms = moves        { m :: ms }
  ;

move:
  | COLOR; sq0 = SQUARE; sq = SQUARE; koma = KOMA; delimiter; time = time
    { (Some sq0, sq, koma, time) }
  | COLOR; KOMADAI; sq = SQUARE; koma = KOMA; delimiter; time = time
    { (None, sq, koma, time) }
  ;

time:
  |                             { 0 }
  | t = TIME; delimiter         { t }
  ;
