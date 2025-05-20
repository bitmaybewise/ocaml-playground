type 'a value =
  | String of string
  | List of 'a list
  | Whatever

let func1 v =
  match v with
  | String s ->
    let len = String.length s in
    let char = if len >= 1 then String.get s 0 else '.' in
    String (Char.escaped char)
  | List a ->
    let len = List.length a in
    let item = if len >= 1 then List.nth a 0 else 42 in
    List [item]
  | Whatever -> failwith "🤷‍♂️"

module Enumerable = struct
  let length v =
    match v with
    | String s -> String.length s
    | List a -> List.length a
    | Whatever -> failwith "🤷‍♂️"

  let nth v n =
    match v with
    | String s ->
      if n >= 0 && n < String.length s then
        Some (String (Char.escaped (String.get s n)))
      else None
    | List a ->
      if n >= 0 && n < List.length a then
        Some (List [List.nth a n])
      else None
    | Whatever -> failwith "🤷‍♂️"
end

let _ = Enumerable.length (String "asdf")
let _ = Enumerable.nth (String "asdf") 0
let _ = Enumerable.length (List [1, 2, 3])
let _ = Enumerable.nth (List [1; 2; 3]) 0

let func2 v =
  let len = Enumerable.length v in
  if len >= 1
    then Enumerable.nth v 0
    else failwith "🤷‍♂️"

let _ = func2 (String "aha!")
let _ = func2 (List [1, 2, 3])
let _ = func2 (List [1; 2; 3])

class type ['a] enumerable = object
  method length : int
  method nth : int -> 'a value option
end

class ['a] string_enumerable (s : string) : ['a] enumerable = object
  method length = String.length s
  method nth n =
    if n >= 0 && n < String.length s then
      Some (String (Char.escaped (String.get s n)))
    else None
end

class ['a] list_enumerable (l : 'a list) : ['a] enumerable = object
  method length = List.length l
  method nth n =
    if n >= 0 && n < List.length l then
      Some (List [List.nth l n])
    else None
end

let s = new string_enumerable "hello"
let l = new list_enumerable [1; 2; 3]
let s_len = s#length  (* returns 5 *)
let l_len = l#length  (* returns 3 *)
let s_first = s#nth 0 (* returns Some (String "h") *)
let l_first = l#nth 0 (* returns Some (List [1]) *)

let main () = print_endline "functors example"
