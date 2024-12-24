type value =
  [ `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string ]

let rec output_value out = function
  | `Null -> Printf.fprintf out "null"
  | `Assoc attrs ->
    Printf.fprintf out "{";
    let print_attrs = fun idx (k, v) ->
      Printf.fprintf out "\"%s\":" k;
      output_value out v;
      let maxlen = List.length attrs - 1 in
      if idx < maxlen then Printf.fprintf out ", ";
    in
    List.iteri print_attrs attrs;
    Printf.fprintf out "}";
  | `Bool b -> Printf.fprintf out "%b" b
  | `Float f -> Printf.fprintf out "%f" f
  | `Int i -> Printf.fprintf out "%d" i
  | `String s -> Printf.fprintf out "\"%s\"" s
  | `List values ->
    let maxlen = List.length values - 1 in
    Printf.fprintf out "[";
    let print' = fun idx v ->
      output_value out v;
      if idx < maxlen then Printf.fprintf out ", "
    in
    List.iteri print' values;
    Printf.fprintf out "]"
