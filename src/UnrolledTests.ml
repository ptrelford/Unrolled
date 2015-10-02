open Unrolled

let run_tests tests =
  Printf.printf "1..%d\n" (List.length tests);
  List.iteri (fun i (name, f) ->
    let ok = try f () with _ -> false in
    if ok then Printf.printf "ok %d - %s\n" (i+1) name
    else Printf.printf "not ok %d - %s\n" (i+1) name
  ) tests

let add_10 () =
  let xs = make ~node_capacity:4 in
  for i = 1 to 10 do add xs 1 done;
  length xs = 10

let insert_10 () =
  let xs = make ~node_capacity:4 in
  for i = 1 to 10 do insert xs 0 1 done;
  length xs = 10

let insert_10_remove_9 () =
  let xs = make ~node_capacity:4 in
  for i = 1 to 10 do insert xs 0 1 done;
  for i = 9 downto 1 do remove_at xs i done;
  length xs = 1

let to_array_from_empty () =
  [||] = (to_array (make ~node_capacity:4))

let to_array_from_items () =
  let xs = make ~node_capacity:2 in
  add xs 1;
  add xs 2;
  add xs 3;
  [|1;2;3|] = to_array xs

let tests = [
  "empty", (fun () -> let xs = make ~node_capacity:4 in length xs = 0);
  "add 1", (fun () -> let xs = make ~node_capacity:4 in add xs 1; length xs = 1);
  "add 10", add_10;
  "insert 10", insert_10;
  "insert 10 - remove 9", insert_10_remove_9;
  "to array from empty", to_array_from_empty;
  "to array from items", to_array_from_items
]

let () = run_tests tests
