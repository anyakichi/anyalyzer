open Core.Std
open ShogiKifu
open Color
open Move

let depth = ref 13
let multipv = ref 3

let usage =
  "usage:\n" ^
  "  anyalyzer [options] ENGINE FILE\n"

let rec speclist = [
  "-help",      Arg.Unit help, "";
  "--help",     Arg.Unit help, "";
  "-h",         Arg.Unit help,
                " show help message";

  "-d",         Arg.Set_int depth,
                "depth";

  "-m",         Arg.Set_int multipv,
                "multipv";
]
and help () = raise (Arg.Help (Arg.usage_string (Arg.align speclist) usage))

let rev_argv = ref []

let print_position oc lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  fprintf oc "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
  try UsiParser.main UsiLexer.token lexbuf with
  | UsiLexer.Error msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | UsiParser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)
;;

let read_while0 ic token =
  let rec loop acc =
    let line = input_line ic in
    if String.is_prefix line token then
      List.rev (line :: acc)
    else
      loop (line :: acc)
  in
  if token = "" then [] else loop []
;;

let read_while ic token =
  String.concat ~sep:"\n" @@ read_while0 ic token
;;

let usi_request0 (ic, oc) req tok =
  output_string oc (req ^ "\n");
  flush oc;
  read_while0 ic tok
;;

let usi_request (ic, oc) req tok =
  output_string oc (req ^ "\n");
  flush oc;
  read_while ic tok
;;

let moves_list moves =
  let rec loop acc l =
    match l with
    | [] -> acc
    | x :: xs ->
        loop ((String.concat ~sep:" " @@ List.rev l) :: acc) xs
  in
  loop [] (List.rev @@ List.map ~f:(fun x -> Move.to_usi x) moves)
;;

let pv_string pos values sq0 =
  let curpos = Position.copy pos in
  let moves =
    List.fold_left values ~init:[] ~f:(fun a v ->
      (Position.move curpos v) :: a)
    |> List.rev
  in
  let kifu = ShogiKifu.make pos ~moves:(List.take moves 13) in
  let s = ShogiKifu.to_moves ~sq0:(Some sq0) kifu in
  s
;;

let main engine ic =
  let engine_dir = Filename.dirname engine in
  let engine_bin = Filename.basename engine in
  let filebuf = Lexing.from_channel ic in
  try
    let kifu = CsaParser.main CsaLexer.token filebuf in
    let curpos = Position.copy kifu.position in
    Unix.chdir engine_dir;
    let ic, oc = Unix.open_process ("./" ^ engine_bin) in
    let usireq0 = usi_request0 (ic, oc) in
    let usireq = usi_request (ic, oc) in
    ignore @@ usireq "usi" "usiok";
    ignore @@ usireq "setoption name USI_Hash value 2048" "";
    ignore @@ usireq
                (Printf.sprintf "setoption name MultiPV value %d" !multipv) "";
    ignore @@ usireq "setoption name Max_Random_Score_Diff value 0" "";
    ignore @@ usireq "setoption name Max_Random_Score_Diff_Ply value 32767" "";
    ignore @@ usireq "isready" "readyok";
    ignore @@ usireq "usinewgame" "";
    let sfen = Position.to_sfen kifu.position in
    let moves' =
      List.fold2_exn kifu.moves (moves_list kifu.moves) ~init:([], 0) ~f:(fun (a, pre_cp) x y ->
        let req = Printf.sprintf "position sfen %s moves %s\ngo depth %d"
                    sfen y !depth in
        let resp =
          usireq0 req "bestmove"
        in
        let resp =
          resp
          |> List.rev
          |> List.drop_while ~f:(fun x -> x <> "")
          |> Fn.flip List.drop 1
          |> List.take_while ~f:(fun x -> x <> "")
          |> String.concat ~sep:"\n"
        in
        ignore @@ Position.move curpos (Move.to_pmove x);
        let cp = ref 0 in
        match parse_with_error (Lexing.from_string resp) with
        | None -> (x :: a, !cp)
        | Some l ->
            let comment =
              List.sort l ~cmp:(fun (vals0, _) (vals1, _) ->
                compare (List.Assoc.find vals0 "multipv")
                        (List.Assoc.find vals1 "multipv"))
              |> List.map ~f:(fun (vals, moves) ->
                let s = pv_string curpos moves x.Move.sq in
                match List.Assoc.find vals "score cp", List.Assoc.find vals "score mate" with
                | None  , None   -> s
                | Some v, None   ->
                    let v = if curpos.Position.teban = Black then v else - v in
                    if !cp = 0 then
                      cp := v;
                    Printf.sprintf "評価値: %+d (%+d)\n%s" (v) (v - pre_cp) s
                | None  , Some v ->
                    if !cp = 0 then
                      cp := if Position.teban curpos = Black then 10000 else -10000;
                    Printf.sprintf "詰み: %d\n%s"
                      (if curpos.Position.teban = Black then v else - v) s
                | _ -> failwith "Invalid format")
              |> String.concat ~sep:"\n\n"
            in
            ({ x with Move.comment = comment } :: a, !cp))
      |> fst
      |> List.rev
    in
    let kifu = { kifu with moves = moves' } in
    print_endline @@ ShogiKifu.to_kif kifu;
  with
  | CsaLexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | CsaParser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
;;

let () =
  Arg.parse (Arg.align speclist)
    (fun arg -> rev_argv := arg :: !rev_argv) usage;
  match !rev_argv with
  | [engine] -> main engine stdin
  | [file; engine] -> main engine (open_in file)
  | _ -> help(); exit 1
;;
