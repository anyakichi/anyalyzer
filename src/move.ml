open Core.Std

open Color
open Pmove

type annotation =
  | Forward
  | Backward
  | Sideward
  | Left
  | LeftForward
  | LeftBackward
  | LeftSideward
  | Right
  | RightForward
  | RightBackward
  | RightSideward
  | Straight
  | Drop

type t = {
  color : Color.t option;
  sq0 : Square.t option;
  sq : Square.t;
  koma : Koma.t option;
  annot : annotation option;
  promote : bool option;
  same : bool option;
  time : int;
  comment : string;
}

let make ?color ?sq0 ?koma ?annot ?promote ?same ?(time=0) ?(comment="") sq =
  { color; sq0; sq; koma; annot; promote; same; time; comment }
;;

let update t ?color ?sq0 ?sq ?koma ?annot ?promote ?same ?time ?comment () =
  {
    color = Option.value_map color ~default:t.color ~f:Option.some;
    sq0 = Option.value_map sq0 ~default:t.sq0 ~f:Option.some;
    sq = Option.value sq ~default:t.sq;
    koma = Option.value_map koma ~default:t.koma ~f:Option.some;
    annot = Option.value_map annot ~default:t.annot ~f:Option.some;
    promote = Option.value_map promote ~default:t.promote ~f:Option.some;
    same = Option.value_map same ~default:t.same ~f:Option.some;
    time = Option.value time ~default:t.time;
    comment = Option.value comment ~default:t.comment;
  }
;;

let to_pmove t =
  match t.sq0, t.koma, t.promote with
  | Some sq0, _        , Some promote -> Move (sq0, t.sq, promote)
  | Some sq0, Some koma, None         -> Move' (sq0, t.sq, koma)
  | None,     Some koma, _            -> Drop (t.sq, koma)
  | _                                 -> invalid_arg "Move.to_move"
;;

let promote_to_string = function
  | true -> "成"
  | false -> "不成"
;;

let annotation_to_string = function
  | Forward       -> "上"
  | Backward      -> "引"
  | Sideward      -> "寄"
  | Left          -> "左"
  | LeftForward   -> "左上"
  | LeftBackward  -> "左引"
  | LeftSideward  -> "左寄"
  | Right         -> "右"
  | RightForward  -> "右上"
  | RightBackward -> "右引"
  | RightSideward -> "右寄"
  | Straight      -> "直"
  | Drop          -> "打"
;;

let to_string ?sq0 t ~color =
  Printf.sprintf "%s%s%s%s%s"
    (Color.to_kif color)
    (if sq0 = Some t.sq then "同" else Square.to_kif t.sq)
    (Option.value_map t.koma ~default:"" ~f:Koma.to_kif)
    (Option.value_map t.annot ~default:"" ~f:annotation_to_string)
    (Option.value_map t.promote ~default:"" ~f:promote_to_string)
;;

let to_kif ?sq0 ?(num=1) t =
  let sq0_to_string = function
    | None -> "打  "
    | Some sq -> Printf.sprintf "(%s)" (Square.to_string sq)
  in

  let s = t.time mod 60 in
  let m = t.time / 60 in
  let kif = Printf.sprintf "%3d %s%s%s%s%s (%2d:%02d/00:00:00)"
              num
              (if sq0 = Some t.sq then "同　" else Square.to_kif t.sq)
              (Option.value_map t.koma ~default:"" ~f:Koma.to_kif)
              (if t.promote = Some true then "成" else "")
              (sq0_to_string t.sq0)
              (if t.promote <> Some true then "" else "  ")
              m s
  in
  if t.comment = "" then kif
  else
    let comment =
      String.split t.comment ~on:'\n'
      |> String.concat ~sep:"\n*"
    in
    kif ^ "\n*" ^ comment
;;

let to_usi t =
  match t.sq0 with
  | None ->
      Printf.sprintf "%s*%s"
        (Option.value_map t.koma ~default:"" ~f:Koma.to_sfen)
        (Square.to_usi t.sq)
  | Some sq0 ->
      Printf.sprintf "%s%s%s" (Square.to_usi sq0) (Square.to_usi t.sq)
        (if t.promote = Some true then "+" else "")
;;
