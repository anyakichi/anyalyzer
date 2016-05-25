open StdLabels

module Color = Color
module Koma = Koma
module Square = Square
module Move = Move
module Position = Position

type t = {
  tags : (string * string) list;
  position : Position.t;
  moves : Move.t list;
}

let make ?(tags=[]) ?(moves=[]) position =
  { tags; position; moves }
;;

let to_kif t =
  let header =
    Printf.sprintf "先手：%s\n後手：%s\n手数----指手---------消費時間--\n"
      (try List.assoc "Black" t.tags with Not_found -> "先手")
      (try List.assoc "White" t.tags with Not_found -> "後手")
  in
  let s =
    List.mapi t.moves ~f:(fun i m ->
      Move.to_kif m ~num:(i + 1))
    |> String.concat ~sep:"\n"
  in
  header ^ s
;;

let to_moves ?(sq0=None) t =
  let rec loop sq0 color acc moves =
    match moves, sq0 with
    | [],      _ -> acc
    | x :: xs, None ->
        loop (Some x.Move.sq) (Color.inv color) (Move.to_string x ~color :: acc) xs
    | x :: xs, Some sq0 ->
        loop (Some x.Move.sq) (Color.inv color) (Move.to_string x ~color ~sq0 :: acc) xs
  in
  loop sq0 (Position.teban t.position) [] t.moves
  |> List.rev
  |> String.concat ~sep:""
;;
