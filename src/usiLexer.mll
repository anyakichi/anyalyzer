{

open Lexing
open UsiParser

open ShogiKifu
open Koma
open Pmove

exception Error of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = lexbuf.lex_curr_pos;
                                  pos_lnum = pos.pos_lnum + 1 }
;;

let square_of_string s =
  if (String.length s) <> 2 then
    raise (Error "Invalid square");
  let i = int_of_char s.[0] - int_of_char '0' in
  let j = int_of_char s.[1] - int_of_char 'a' + 1 in
  if not (1 <= i && i <= 9 && 1 <= j && j <= 9) then
    raise (Error "Invalid square range");
  Square.make i j
;;

let koma_of_char s =
  match s with
  | 'R' -> HI
  | 'B' -> KA
  | 'G' -> KI
  | 'S' -> GI
  | 'N' -> KE
  | 'L' -> KY
  | 'P' -> FU
  | s -> raise (Error "Invalid piece")
;;

}

let int = '-'? ['0'-'9']+
let square = ['1'-'9'] ['a'-'i']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let piece = ['K' 'R' 'B' 'G' 'S' 'N' 'L' 'P']

rule token = parse
  | "info"              { INFO }
  | "depth"             { DEPTH }
  | "seldepth"          { SELDEPTH }
  | "score"             { SCORE }
  | "cp"                { CP }
  | "mate"              { MATE }
  | "upperbound"        { UPPERBOUND }
  | "lowerbound"        { LOWERBOUND }
  | "nodes"             { NODES }
  | "nps"               { NPS }
  | "time"              { TIME }
  | "multipv"           { MULTIPV }
  | "pv"                { PV }

  | int as i            { INT (int_of_string i) }

  | (square as sq0) (square as sq) ('+'? as p)
    {
      MOVE (Move (square_of_string sq0, square_of_string sq, p = "+"))
    }

  | (['R' 'B' 'G' 'S' 'N' 'L' 'P'] as koma) '*' (square as sq)
      { MOVE (Drop (square_of_string sq, koma_of_char koma)) }

  | white               { token lexbuf }
  | newline             { next_line lexbuf; EOL }
  | eof                 { EOF }

  | _
    { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
