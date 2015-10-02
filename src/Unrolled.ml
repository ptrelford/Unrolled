type 'a node = {
  mutable next : 'a node option;
  mutable count : int;
  mutable items : 'a array
}

type 'a unrolled_linked_list = {
  node_capacity : int;
  mutable length : int;
  mutable first : 'a node option;
  mutable last : 'a node option
}

type 'a t = 'a unrolled_linked_list

let make ~node_capacity =
  { node_capacity;
    length = 0;
    first = None;
    last = None
  }

let length (xs:'a t) = xs.length

let iter (f:'a -> unit) (xs:'a t) =
  let rec next = function
    | Some node ->
      for i = 0 to node.count-1 do
        f(node.items.(i))
      done;
      next (node.next)
    | None -> ()
  in
  next xs.first

let get (xs:'a t) (index:int) =
  let rec find count = function
    | None -> failwith "Outside bounds"
    | Some node ->
      if index < count + node.count then node.items.(count-index)
      else find (count+node.count) node.next
  in
  find 0 xs.first

let set (xs:'a t) (index:int) (value:'a) =
  let rec find count = function
    | None -> failwith "Outside bounds"
    | Some node ->
      if index < count + node.count then node.items.(count-index) <- value
      else find (count+node.count) node.next
  in
  find 0 xs.first

let to_array (xs:'a t) =
  let len = length xs in
  if len = 0 then [||]
  else (
    let ar = Array.make len (get xs 0) in
    let rec copy index = function
      | None -> ()
      | Some node ->
        Array.blit node.items 0 ar index node.count;
        copy (index + node.count) node.next
    in
    copy 0 xs.first;
    ar
  )

let add (xs:'a t) (x:'a) =
  let new_node (x:'a) =
    let items = Array.make xs.node_capacity x in
    { next = None;
      count = 1;
      items = items }
  in
  begin
    match xs.last with
    | None ->
      let node = new_node x in
      xs.first <- Some node;
      xs.last <- Some node
    | Some node ->
      if node.count = xs.node_capacity then begin
          let next = new_node x in
          node.next <- Some next;
          xs.last <- Some next
        end else begin
          node.items.(node.count) <- x;
          node.count <- node.count + 1
        end
  end;
  xs.length <- xs.length + 1

let split (xs:'a t) node offset =
  let split = xs.node_capacity / 2 in
  match node.next with
  | Some next when next.count = split && offset < split ->
    (* push excess to next node *)
    Array.blit next.items 0 next.items next.count split;
    Array.blit node.items split next.items 0 split;
    next.count <- xs.node_capacity;
    node.count <- node.count - split;
    node, offset
  | Some _ | None ->
    (* insert new node to right *)
    let items = Array.make xs.node_capacity node.items.(0) in
    let new_node =
      { next = node.next;
        count = split;
        items = items } in
    Array.blit node.items split new_node.items 0 split;
    node.next <- Some new_node;
    node.count <- split;
    begin
      match new_node.next with
      | None -> xs.last <- Some new_node
      | Some _ -> ()
    end;
    if offset < node.count then node, offset
    else new_node, offset - node.count

let insert_left (xs:'a t) (index:int) (x:'a) =
  let rec find count = function
    | None -> failwith "Outside bounds"
    | Some node ->
      if (index - count) <= node.count then node, index - count
      else find (count+node.count) node.next
  in
  let node, offset = find 0 xs.first in
  let node, offset =
    if node.count = xs.node_capacity then
      split xs node offset
    else
      node, offset
  in
  (* insert item in node *)
  Array.blit node.items offset node.items (offset+1) (node.count-offset);
  node.items.(offset) <- x;
  node.count <- node.count + 1;
  xs.length <- xs.length + 1

let insert (xs:'a t) (index:int) (x:'a) =
  if index = xs.length then add xs x
  else insert_left xs index x

let delete (xs:'a t) (index:int) =
  let rec find count previous node' =
    match node' with
    | None -> failwith "Outside bounds"
    | Some node ->
      if index - count < node.count then previous, node, index - count
      else find (count+node.count) node' node.next
  in
  let previous, node, offset = find 0 None xs.first in
  Array.blit node.items (offset+1) node.items offset (node.count-offset-1);
  node.count <- node.count - 1;
  xs.length <- xs.length - 1;
  let split = xs.node_capacity / 2 in
  (* Pull from right *)
  if node.count < split then begin
    match node.next with
    | None ->
      if node.count = 0 then begin
        match previous with
        | None ->
            xs.first <- None;
            xs.last <- None
        | Some previous' ->
            previous'.next <- None;
            xs.last <- previous
      end
    | Some next ->
      if next.count > split then
        let excess = next.count - split in
        Array.blit next.items 0 node.items node.count excess;
        Array.blit next.items excess next.items 0 excess;
        node.count <- node.count + excess;
        next.count <- next.count - excess
      else
        Array.blit next.items 0 node.items node.count next.count;
        node.count <- node.count + next.count;
        node.next <- next.next;
        match node.next with
        | None -> xs.last <- Some node
        | Some _ -> ()
  end
