type t = Black | White

let inv = function
  | Black -> White
  | White -> Black
;;

let to_int = function
  | Black -> 0
  | White -> 1
;;

let to_kif = function
  | Black -> "▲"
  | White -> "△"
;;

let to_sfen = function
  | Black -> "b"
  | White -> "w"
;;
