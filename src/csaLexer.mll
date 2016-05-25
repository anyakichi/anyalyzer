{

open StdLabels

open ShogiKifu
open Koma
open CsaParser

exception Error of string

let digit_of_char c = int_of_char c - int_of_char '0'

}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | "'" [^ '\r' '\n']* newline?
    {
      token lexbuf
    }

  | '+'         { COLOR Color.Black }
  | '-'         { COLOR Color.White }

  | 'N' (['+' '-'] as c) ([^ '\r' '\n']+ as s) newline
                { TAG ((if c = '+' then "Black" else "White"), s) }

  | "PI"        { PI }
  | "P1"        { P1 }
  | "P2"        { P2 }
  | "P3"        { P3 }
  | "P4"        { P4 }
  | "P5"        { P5 }
  | "P6"        { P6 }
  | "P7"        { P7 }
  | "P8"        { P8 }
  | "P9"        { P9 }
  | " * "       { PEMPTY }

  | "00"        { KOMADAI }

  | (['1'-'9'] as i) (['1'-'9'] as j)
                { SQUARE (Square.make (digit_of_char i) (digit_of_char j)) }

  | "OU"        { KOMA OU }
  | "HI"        { KOMA HI }
  | "KA"        { KOMA KA }
  | "KI"        { KOMA KI }
  | "GI"        { KOMA GI }
  | "KE"        { KOMA KE }
  | "KY"        { KOMA KY }
  | "FU"        { KOMA FU }
  | "RY"        { KOMA RY }
  | "UM"        { KOMA UM }
  | "NG"        { KOMA NG }
  | "NK"        { KOMA NK }
  | "NY"        { KOMA NY }
  | "TO"        { KOMA TO }

  | 'T' (['0'-'9']+ as i)
                { TIME (int_of_string i) }

  | ','         { COMMA }
  | newline     { EOL }
  | eof         { EOF }

  | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
