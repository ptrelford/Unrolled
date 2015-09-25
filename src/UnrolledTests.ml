open Unrolled

let run_tests tests =
  Printf.printf "1..%d\n" (List.length tests);
  List.iteri (fun i (name, f) ->
    let ok = try f () with _ -> false in
    if ok then Printf.printf "ok %d - %s\n" (i+1) name
    else Printf.printf "not ok %d - %s\n" (i+1) name
  ) tests

let tests = [
  "2 + 2 should equal 4", fun () -> 2 + 2 = 4
]

let add_10 () =
  let xs = make 4 in
  for i = 1 to 10 do add xs 1 done;
  length xs = 10

let insert_10 () =
  let xs = make 4 in
  for i = 1 to 10 do insert xs 0 1 done;
  length xs = 10

let insert_10_remove_9 () =
  let xs = make 4 in
  for i = 1 to 10 do insert xs 0 1 done;
  for i = 9 downto 1 do remove_at xs i done;
  length xs = 1

let tests = [
  "empty", (fun () -> let xs = make 4 in length xs = 0);
  "add 1", (fun () -> let xs = make 4 in add xs 1; length xs = 1);
  "add 10", add_10;
  "insert 10", insert_10;
  "insert 10 - remove 9", insert_10_remove_9
]

let () = run_tests tests
