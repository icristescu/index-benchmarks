module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

type attrs = float StringMap.t

type bench_result = (string * float) list [@@deriving yojson]

let attrs_to_yojson m = StringMap.bindings m |> [%to_yojson: bench_result]

let parse_into_stringmap x =
  let string_map = StringMap.empty in
  let string_map =
    List.fold_left
      (fun string_map (key, value) ->
        StringMap.add key
          (float_of_string (Yojson.Safe.to_string value))
          string_map)
      string_map x
  in
  string_map

let attrs_of_yojson input =
  match input with
  | `Assoc x -> Result.Ok (parse_into_stringmap x)
  | _ -> Result.Error "Couldn't parse the json :("

type bench_results = (string * attrs) list [@@deriving yojson]

let benchmark_list =
  [
    "replace_random";
    "replace_random_sync";
    "replace_increasing_keys";
    "replace_increasing_hash";
    "replace_decreasing_hash";
    "iter_rw";
    "find_random_ro";
    "find_random_rw";
    "find_absent_ro";
    "find_absent_rw";
  ]

open Yojson.Safe.Util

let get_bench_results json bench_name =
  [ json ] |> filter_member "results" |> filter_member bench_name |> List.hd

let put_benchmarks_into_a_list acc json =
  List.fold_left
    (fun acc name -> (name, get_bench_results json name) :: acc)
    acc benchmark_list

let read_file path = Yojson.Safe.from_file path |> put_benchmarks_into_a_list []

let diff (map1 : (float StringMap.t, string) Result.result)
    (map2 : (float StringMap.t, string) Result.result) =
  match (map1, map2) with
  | Ok map1, Ok map2 ->
      let new_map =
        StringMap.merge
          (fun _ xo yo ->
            match (xo, yo) with Some x, Some y -> Some (x -. y) | _ -> None)
          map1 map2
      in
      new_map
  | _, _ -> StringMap.empty

let generate_diff filepath1 filepath2 =
  let json1 = read_file filepath1 in
  let json2 = read_file filepath2 in
  let list_of_map1 =
    List.fold_left
      (fun acc value -> (fst value, attrs_of_yojson (snd value)) :: acc)
      [] json1
  in
  let list_of_map2 =
    List.fold_left
      (fun acc value -> (fst value, attrs_of_yojson (snd value)) :: acc)
      [] json2
  in
  let empty_list =
    List.fold_left2
      (fun empty_list a b -> (fst a, diff (snd a) (snd b)) :: empty_list)
      [] list_of_map1 list_of_map2
  in
  empty_list |> bench_results_to_yojson
