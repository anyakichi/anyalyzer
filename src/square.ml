open Color

type t = int * int

let is_valid (s, d) = 1 <= s && s <= 9 && 1 <= d && d <= 9

let make suji dan =
  match suji, dan with
  | sq when is_valid sq -> sq
  | s, d -> invalid_arg (Printf.sprintf "Square.make %d %d" s d)
;;

let is_promotion_zone (suji, dan) ~color =
  (color = Black && 1 <= dan && dan <= 3) ||
  (color = White && 7 <= dan && dan <= 9)
;;

let validate t = if is_valid t then Some t else None

let add (suji, dan) (x, y) = validate (suji + x, dan + y)
let sub (suji, dan) (x, y) = (suji - x, dan - y)

let of_tuple (suji, dan) = make suji dan

let to_tuple t = t

let to_string (suji, dan) =
  Printf.sprintf "%d%d" suji dan
;;

let suji_to_kif = function
  | 1 -> "１" | 2 -> "２" | 3 -> "３" | 4 -> "４" | 5 -> "５"
  | 6 -> "６" | 7 -> "７" | 8 -> "８" | 9 -> "９" | _ -> "？"
;;

let dan_to_kif = function
  | 1 -> "一" | 2 -> "二" | 3 -> "三" | 4 -> "四" | 5 -> "五"
  | 6 -> "六" | 7 -> "七" | 8 -> "八" | 9 -> "九" | _ -> "？"
;;

let to_kif (suji, dan) =
  (suji_to_kif suji) ^ (dan_to_kif dan)
;;

let dan_to_usi = function
  | 1 -> "a" | 2 -> "b" | 3 -> "c" | 4 -> "d" | 5 -> "e"
  | 6 -> "f" | 7 -> "g" | 8 -> "h" | 9 -> "i" | _ -> "?"
;;

let to_usi (suji, dan) =
  Printf.sprintf "%d%s" suji (dan_to_usi dan)
;;
