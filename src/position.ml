open Core.Std
open Option.Monad_infix

open Color
open Move
open Koma
open Pmove

module Board = struct
  type 'a t = 'a option array array

  let make = Array.make_matrix

  let copy t = Array.map ~f:Array.copy t

  let iteri t ~f =
    Array.iteri t ~f:(fun i row ->
      Array.iteri row ~f:(fun j x ->
        f (i, j) x))
  ;;

  let get t (i, j) =
    t.(i).(j)
  ;;

  let set t (i, j) v =
    t.(i).(j) <- Some v
  ;;

  let clear t (i, j) =
    t.(i).(j) <- None
  ;;

  let take t (i, j) =
    let v = t.(i).(j) in
    t.(i).(j) <- None;
    v
  ;;

  let put t (i, j) v =
    let old = t.(i).(j) in
    t.(i).(j) <- Some v;
    old
  ;;
end

type t = {
  ban : (Color.t * Koma.t) Board.t;
  mutable komadai : (Color.t * Koma.t) list;
  mutable teban : Color.t;
  mutable last_sq : Square.t option;
}

type preset =
  | Hirate
  | Custom

let point_of_tuple (suji, dan) =
  (dan - 1, 9 - suji)
;;

let point_of_square sq =
  point_of_tuple (Square.to_tuple sq)
;;

let get t sq =
  Board.get t.ban (point_of_square sq)
;;

let get_exn t sq =
  match get t sq with
  | None -> raise Not_found
  | Some v -> v
;;

let set t sq v =
  Board.set t.ban (point_of_square sq) v
;;

let clear t sq =
  Board.clear t.ban (point_of_square sq)
;;

let take t sq =
  Board.take t.ban (point_of_square sq)
;;

let take_exn t sq =
  match take t sq with
  | None -> raise Not_found
  | Some v -> v
;;

let put t sq (color, koma) =
  match Board.put t.ban (point_of_square sq) (color, koma) with
  | None -> None
  | Some (color', koma') as v->
      if color = color' then
        invalid_arg "Position.put";
      let koma' = try Koma.demote koma' with _ -> koma' in
      t.komadai <- (color, koma') :: t.komadai;
      v
;;

let make ?(preset=Hirate) () =
  let set0 t sqt v = set t (Square.of_tuple sqt) v in
  let hirate t =
    set0 t (5, 9) (Black, OU); set0 t (5, 1) (White, OU);
    set0 t (6, 9) (Black, KI); set0 t (4, 1) (White, KI);
    set0 t (4, 9) (Black, KI); set0 t (6, 1) (White, KI);
    set0 t (7, 9) (Black, GI); set0 t (3, 1) (White, GI);
    set0 t (3, 9) (Black, GI); set0 t (7, 1) (White, GI);
    set0 t (8, 9) (Black, KE); set0 t (2, 1) (White, KE);
    set0 t (2, 9) (Black, KE); set0 t (8, 1) (White, KE);
    set0 t (9, 9) (Black, KY); set0 t (1, 1) (White, KY);
    set0 t (1, 9) (Black, KY); set0 t (9, 1) (White, KY);
    set0 t (8, 8) (Black, KA); set0 t (2, 2) (White, KA);
    set0 t (2, 8) (Black, HI); set0 t (8, 2) (White, HI);
    set0 t (5, 7) (Black, FU); set0 t (5, 3) (White, FU);
    set0 t (6, 7) (Black, FU); set0 t (4, 3) (White, FU);
    set0 t (4, 7) (Black, FU); set0 t (6, 3) (White, FU);
    set0 t (7, 7) (Black, FU); set0 t (3, 3) (White, FU);
    set0 t (3, 7) (Black, FU); set0 t (7, 3) (White, FU);
    set0 t (8, 7) (Black, FU); set0 t (2, 3) (White, FU);
    set0 t (2, 7) (Black, FU); set0 t (8, 3) (White, FU);
    set0 t (9, 7) (Black, FU); set0 t (1, 3) (White, FU);
    set0 t (1, 7) (Black, FU); set0 t (9, 3) (White, FU);
    t
  in
  let t = {
    ban = Board.make 9 9 None;
    komadai = [];
    teban = Black;
    last_sq = None
  } in
  match preset with
  | Hirate -> hirate t
  | Custom -> t
;;

let copy t =
  { t with ban = Board.copy t.ban }
;;

let dirpos ~color sq0 sq =
  let sdiff, ddiff = Square.sub sq sq0 in
  let dir =
    if ddiff = 0 then
      Move.Sideward
    else if ddiff < 0 && color = Black || ddiff > 0 && color = White then
      Move.Forward
    else
      Move.Backward
  in
  let pos =
    if sdiff < 0 && color = Black || sdiff > 0 && color = White then
      Move.Left
    else
      Move.Right
  in
  (dir, pos)
;;

let annot2 ?(color=Black) sq sq1 sq2 =
  let dir1, pos1 = dirpos ~color sq1 sq in
  let dir2, pos2 = dirpos ~color sq2 sq in
  if dir1 <> dir2
  then dir1
  else pos1
;;

(* annotations for general pieces (KI and GI) *)
let annot_gen ~color sq sq1 sqs =
  let relsqs = List.map sqs ~f:(fun x -> Square.sub x sq) in
  match Square.sub sq1 sq with
  | (0, -1) ->
      (* Koma is KI and this is only one backward move *)
      Move.Backward
  | (1, 0) ->
      (* Koma is KI *)
      begin match List.mem relsqs (-1, 0), List.mem relsqs (1, 1) with
      | false, _     -> Move.Sideward
      | true,  false -> Move.Left
      | true,  true  -> Move.LeftSideward
      end
  | (-1, 0) ->
      (* Koma is KI *)
      begin match List.mem relsqs (1, 0), List.mem relsqs (-1, 1) with
      | false, _     -> Move.Sideward
      | true,  false -> Move.Right
      | true,  true  -> Move.RightSideward
      end
  | (1, -1) ->
      (* Koma is GI *)
      begin match List.mem relsqs (-1, -1), List.mem relsqs (1, 1) with
      | false, _     -> Move.Backward
      | true,  false -> Move.Left
      | true,  true  -> Move.LeftBackward
      end
  | (-1, -1) ->
      (* Koma is GI *)
      begin match List.mem relsqs (1, -1), List.mem relsqs (-1, 1) with
      | false, _     -> Move.Backward
      | true,  false -> Move.Right
      | true,  true  -> Move.RightBackward
      end
  | (0, 1) ->
      begin match List.mem relsqs (1, 1), List.mem relsqs (-1, 1) with
      | false, false -> Move.Forward
      | _            -> Move.Straight
      end
  | (1, 1) ->
      begin match List.mem relsqs (0, 1) || List.mem relsqs (-1, 1),
                  List.mem relsqs (1, 0) || List.mem relsqs (1, -1) with
      | false, _     -> Move.Forward
      | true,  false -> Move.Left
      | true,  true  -> Move.LeftForward
      end
  | (-1, 1) ->
      begin match List.mem relsqs (0, 1) || List.mem relsqs (1, 1),
                  List.mem relsqs (-1, 0) || List.mem relsqs (-1, -1) with
      | false, _     -> Move.Forward
      | true,  false -> Move.Right
      | true,  true  -> Move.RightForward
      end
  | _ -> invalid_arg "can't resolve relation"
;;

let resolve_annot ~color ~koma sq sq0 sq0s =
  match sq0s, koma with
  | [], _ -> None
  | xs,  (KI | GI | NG | NK | NY | TO) -> Some (annot_gen ~color sq sq0 xs)
  | [x], _ -> Some (annot2 ~color sq sq0 x)
  | _ -> invalid_arg "Position.resolve_annot"
;;

let drop t sq v =
  match put t sq v with
  | Some _ -> failwith "Position.drop"
  | None   -> ()
;;

let move t mv =
  let color = t.teban in

  (* Find squares of the komas that can move to sq *)
  let get_sqs sq color koma =
    List.filter_map (Koma.can_move ~color:(Color.inv color) koma) ~f:(fun l ->
      List.find_map l ~f:(fun x ->
        Square.add sq x >>= fun sq' ->
        get t sq' >>= fun pair ->
        Some (sq', pair)) >>= fun (sq', pair) ->
      if (color, koma) = pair then Some sq' else None)
  in

  let rec move0 = function
    | Move (sq0, sq, promo) ->
        let color', koma = get_exn t sq0 in
        let sq0s =
          get_sqs sq color koma
          |> List.filter ~f:(fun x -> x <> sq0)
        in
        let annot =
          resolve_annot ~color ~koma sq sq0 sq0s in
        let color', koma = take_exn t sq0 in
        if color <> color' then
          failwith "Position.move";
        ignore @@ put t sq (color, (if promo then Koma.promote koma else koma));
        let promote =
          if promo then
            Some true
          else if (Koma.can_promote koma) &&
                  (Square.is_promotion_zone ~color sq ||
                   Square.is_promotion_zone ~color sq0) then
            Some false
          else
            None
        in
        let same = t.last_sq = Some sq in
        t.last_sq <- Some sq;
        Move.make ~sq0 sq ~koma ?promote ?annot ~same
    | Drop (sq, koma) ->
        drop t sq (color, koma);
        let annot =
          if (get_sqs sq color koma) = [] then None else Some Move.Drop in
        t.last_sq <- Some sq;
        Move.make sq ~koma ?annot ~same:false
    | Move' (sq0, sq, koma) ->
        let promo =
          match get t sq0 with
          | Some (_, okoma) when koma <> okoma -> true
          | _ -> false
        in
        move0 (Move (sq0, sq, promo))
  in

  let m = move0 mv in
  t.teban <- Color.inv t.teban;
  m
;;

let teban t = t.teban

let to_sfen t =
  let count = ref 0 in
  let buf = Buffer.create 81 in
  Board.iteri ~f:(fun (i, j) x ->
    if i <> 0 && j = 0 then begin
      if !count <> 0 then begin
        Buffer.add_string buf @@ string_of_int !count;
        count := 0
      end;
      Buffer.add_char buf '/';
    end;
    match x with
    | None -> incr count
    | Some (color, koma) ->
        if !count <> 0 then begin
          Buffer.add_string buf @@ string_of_int !count;
          count := 0
        end;
        if color = Black then
          Buffer.add_string buf @@ Koma.to_sfen koma
        else
          Buffer.add_string buf @@ String.lowercase @@ Koma.to_sfen koma
  ) t.ban;
  let ban = Buffer.contents buf in
  let komadai =
    if t.komadai = [] then
      "-"
    else
      List.fold_left t.komadai ~init:[] ~f:(fun a (color, koma) ->
        let sfen = Koma.to_sfen koma in
        let sfen = if color = Black then sfen else String.lowercase sfen in
        match List.Assoc.find a sfen with
        | None -> (sfen, 1) :: a
        | Some count -> (sfen, count + 1) :: (List.Assoc.remove a sfen))
      |> List.map ~f:(fun (sfen, count) -> Printf.sprintf "%d%s" count sfen)
      |> String.concat ~sep:""
  in
  Printf.sprintf "%s %s %s 1" ban (Color.to_sfen t.teban) komadai
;;
