open StdLabels

open Color

type t =
  | OU
  | HI | RY
  | KA | UM
  | KI
  | GI | NG
  | KE | NK
  | KY | NY
  | FU | TO

let promote = function
  | HI -> RY
  | KA -> UM
  | GI -> NG
  | KE -> NK
  | KY -> NY
  | FU -> TO
  | _  -> invalid_arg "Koma.promote"
;;

let demote = function
  | RY -> HI
  | UM -> KA
  | NG -> GI
  | NK -> KE
  | NY -> KY
  | TO -> FU
  | _  -> invalid_arg "Koma.demote"
;;

let can_promote = function
  | HI | KA | GI | KE | KY | FU -> true
  | _ -> false
;;

let is_promoted = function
  | RY | UM | NG | NK | NY | TO -> true
  | _ -> false
;;

let rec can_move ?(color=Black) t =
  let nums = [1; 2; 3; 4; 5; 6; 7; 8] in
  match t, color with
  | OU, _ ->
      [[(-1, -1)]; [(0, -1)]; [(1, -1)];
       [(-1,  0)];            [(1,  0)];
       [(-1,  1)]; [(0,  1)]; [(1,  1)]]
  | HI, _ ->
      [List.map nums ~f:(fun n -> (  0,   n));
       List.map nums ~f:(fun n -> (  0, - n));
       List.map nums ~f:(fun n -> (  n,   0));
       List.map nums ~f:(fun n -> (- n,   0))]
  | RY, _ ->
      (can_move HI) @ [[(-1, -1)]; [(1, -1)]; [(-1,  1)]; [(1,  1)]]
  | KA, _ ->
      [List.map nums ~f:(fun n -> (  n,   n));
       List.map nums ~f:(fun n -> (  n, - n));
       List.map nums ~f:(fun n -> (- n,   n));
       List.map nums ~f:(fun n -> (- n, - n))]
  | UM, _ ->
      (can_move KA) @ [[(0, -1)]; [(-1, 0)]; [(1, 0)]; [(0, 1)]]
  | (KI | NG | NK | NY | TO), Black ->
      [[(0, -1)]; [(-1, 0)]; [(1, 0)]; [(0, 1)]; [(-1, -1)]; [(1, -1)]]
  | (KI | NG | NK | NY | TO), White ->
      [[(0, -1)]; [(-1, 0)]; [(1, 0)]; [(0, 1)]; [(-1,  1)]; [(1,  1)]]
  | GI, Black -> [[(-1, -1)]; [(1, -1)]; [(-1, 1)]; [(1, 1)]; [(0, -1)]]
  | GI, White -> [[(-1, -1)]; [(1, -1)]; [(-1, 1)]; [(1, 1)]; [(0,  1)]]
  | KE, Black -> [[(-1, -2)]; [(1, -2)]]
  | KE, White -> [[(-1,  2)]; [(1,  2)]]
  | KY, Black -> [List.map nums ~f:(fun n -> (0, - n))]
  | KY, White -> [List.map nums ~f:(fun n -> (0,   n))]
  | FU, Black -> [[(0, -1)]]
  | FU, White -> [[(0,  1)]]
;;

let to_string = function
  | OU -> "OU"
  | HI -> "HI" | RY -> "RY"
  | KA -> "KA" | UM -> "UM"
  | KI -> "KI"
  | GI -> "GI" | NG -> "NG"
  | KE -> "KE" | NK -> "NK"
  | KY -> "KY" | NY -> "NY"
  | FU -> "FU" | TO -> "TO"
;;

let to_kif = function
  | OU -> "玉"
  | HI -> "飛" | RY -> "龍"
  | KA -> "角" | UM -> "馬"
  | KI -> "金"
  | GI -> "銀" | NG -> "成銀"
  | KE -> "桂" | NK -> "成桂"
  | KY -> "香" | NY -> "成香"
  | FU -> "歩" | TO -> "と"
;;

let to_sfen = function
  | OU -> "K"
  | HI -> "R" | RY -> "+R"
  | KA -> "B" | UM -> "+B"
  | KI -> "G"
  | GI -> "S" | NG -> "+S"
  | KE -> "N" | NK -> "+N"
  | KY -> "L" | NY -> "+L"
  | FU -> "P" | TO -> "+P"
;;
