#load "str.cma"

let walk_directory_tree dir pattern =
  let select str = Str.string_match (Str.regexp pattern) str 0 in
  let rec walk acc = function
    | [] -> (acc)
    | dir :: dirs ->
       let contents = Array.map (Filename.concat dir) (Sys.readdir dir) in
       let xdirs, files =
         Array.fold_left
           (fun (xdirs,files) f ->
            if Sys.is_directory f then (f :: xdirs, files) else (xdirs, f :: files)
           ) ([], []) contents
       in
       let matched = List.filter (select) files in
       walk (matched @ acc) (xdirs @ dirs)
  in
  walk [] [dir]
;;

let algpattern = Str.regexp "algoritmo[ ]+\"\\([A-Za-z0-9 _]+\\)\"" ;;

let get_algname file =
  let ic = open_in_bin file in
  let iclen = in_channel_length ic in
  let s = String.make iclen ' ' in
  let n = input ic s 0 iclen in
  close_in ic;
  assert(n = iclen);
  try
    ignore (Str.search_forward algpattern s 0);
    Str.matched_group 1 s;
  with
  | Not_found ->
     Format.printf "No name found for %s@." file;
     exit 1;
;;



let main () =
  let cdir = Sys.getcwd () in
  let files = walk_directory_tree (Filename.concat cdir "tests/programs") ".*\\.alg" in
  let oc = open_out (Filename.concat "www" "filemap.txt") in
  let fmt = Format.formatter_of_out_channel oc in
  let b = Buffer.create 2048 in
  let rec maxlen max = function
    | [] -> max
    | file :: files ->
       let max' =
         let slen = String.length file in if slen > max then slen else max in
       maxlen max' files
  in
  let maxl = maxlen 0 (List.map Filename.basename files) in
  List.iter
    (fun f ->
     let bname = Filename.basename f in
     let nlen = String.length f in
     Format.fprintf fmt "\"%s\"  %*s@."
                    bname
                    (maxl - nlen)
                    (Format.sprintf "\"%s\"" (get_algname f));
     Buffer.add_string b (f^" ");
    )
    files;
  close_out oc;
  let oc = open_out (Filename.concat "src" "Makefile.jsfiles") in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "FILES=\"%s\"" (Buffer.contents b);
  close_out oc;
;;

main () ;;
