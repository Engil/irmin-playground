(* Find the last commit that updated the file given as a paramter. *)
(* Syntax is ./find_last_updated_commit.byte file or ./find_last_updated_commit dir dir file *)

open Irmin_unix
open Irmin
open Lwt.Infix

module S = Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module Topological = Graph.Topological.Make(S.History)

let config = Irmin_git.config ~root:"/Users/engil/Dev/ghc/" ()

let upstream = Irmin.remote_uri "https://github.com/Engil/Canopy.git"

let repo = S.Repo.create config

let keys_argv =
  if Array.length Sys.argv > 1 then Array.to_list Sys.argv |> List.tl
  else (Printf.eprintf "Usage: sync [uri]\n%!"; exit 1)

let last_updated repo keys =
  S.master task repo >>= fun t  ->
  S.head_exn (t "Finding head") >>= fun head ->
  S.read_exn (t "Reading file") keys >>= fun current_file ->
  S.history (t "Reading history") >>= fun history ->
  let rec aux ucommit visited to_visit =
    match to_visit with
    | [] -> Lwt.return ucommit
    | commit::to_visit ->
       S.of_commit_id (Irmin.Task.none) commit repo >>= fun store ->
       S.read (store ()) keys >>= fun readed_file ->
       let visited = commit::visited in
       (match readed_file with
	 | Some readed_file ->
	    let matched = (String.compare readed_file current_file) = 0 in
	    let to_visit =
	      if matched then
		(
		  match S.History.pred history commit with
		  | [] -> to_visit
		  | pred::pred2::[] ->
		     let to_visit = if ((List.mem pred visited) = false) then pred::to_visit else to_visit in
		     let to_visit = if ((List.mem pred2 visited) = false) then pred2::to_visit else to_visit in
		     to_visit
		  | pred::[] ->
		     let to_visit = if ((List.mem pred visited) = false) then pred::to_visit else to_visit in
		     to_visit
		  | q -> print_endline "weird"; List.append (List.rev q) to_visit
		) else to_visit
	    in
	    if matched then
	      aux commit visited to_visit
	    else
	      aux ucommit visited []
	 | None -> aux ucommit visited to_visit)
  in
  aux head [] [head]

let last_updated_commit_id repo keys =
  S.master task repo >>= fun t  ->
  S.read_exn (t "Reading file") keys >>= fun current_file ->
  let aux commit_id acc =
    acc >>= fun (acc, matched) ->
    S.of_commit_id (Irmin.Task.none) commit_id repo >>= fun store ->
    S.read (store ()) keys >>= fun readed_file ->
    match readed_file with
    | Some readed_file ->
      let matching = current_file = readed_file in
      let res =
        if current_file = readed_file
        then if matched then acc else commit_id
        else commit_id in
      Lwt.return (res, matching)
    | None -> Lwt.return (commit_id, true) in
  S.history (t "Reading history") >>= fun history ->
  S.head (t "Finding head") >>= function
  | Some head ->
    Topological.fold aux history (Lwt.return (head, false))
    >>= fun (c, _) -> Lwt.return c
  | None ->
    Lwt.fail_with "HEAD doesn't point to anything :("

let test () =
  repo >>= fun repo -> S.master task repo >>= fun t  ->
  let t = Sys.time() in
  last_updated_commit_id repo keys_argv >>= fun c ->
  Printf.printf "execution time old: %fs\n" (Sys.time() -. t);
  let t = Sys.time() in
  last_updated repo keys_argv >>= fun g ->
  Printf.printf "execution time new: %fs\n" (Sys.time() -. t);
  let hashg = Irmin.Hash.SHA1.to_hum g in
  let hash = Irmin.Hash.SHA1.to_hum c in

  Printf.printf "File last edited at commit %s %s\n" hash hashg;
  Lwt.return_unit

let () =
  Lwt_main.run (test ())
