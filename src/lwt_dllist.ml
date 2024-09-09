(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



exception Empty

type 'a node = {
  mutable prev : 'a node;
  mutable next : 'a node;
  mutable node_data : 'a;
  mutable node_active : bool;
}

type 'a t = 'a node

(* +-----------------------------------------------------------------+
   | Operations on nodes                                             |
   +-----------------------------------------------------------------+ *)

let get node =
  node.node_data

let set node data =
  node.node_data <- data

(*
external seq_of_node : 'a node -> 'a t = "%identity"
external node_of_seq : 'a t -> 'a node = "%identity"
 *)

let remove node =
  if node.node_active then begin
    node.node_active <- false;
    node.prev.next <- node.next;
    node.next.prev <- node.prev
  end

(* +-----------------------------------------------------------------+
   | Operations on sequences                                         |
   +-----------------------------------------------------------------+ *)

let create () =
  let rec seq =
    { prev = seq
    ; next = seq
    ; node_data = Obj.magic ()
    ; node_active = false } in
  seq

let is_empty seq = seq.next == seq

let length seq =
  let rec loop curr len =
    if curr == seq then
      len
    else
      let node = curr in
      loop node.next (len + 1)
  in
  loop seq.next 0

let add_l data seq =
  let node =
    { prev = seq
    ; next = seq.next
    ; node_data = data
    ; node_active = true }
  in
  seq.next.prev <- node;
  seq.next <- node;
  node

let add_r data seq =
  let node =
    { prev = seq.prev
    ; next = seq
    ; node_data = data
    ; node_active = true
    }
  in
  seq.prev.next <- node;
  seq.prev <- node;
  node

let take_l seq =
  if is_empty seq then
    raise Empty
  else begin
    let node = seq.next in
    remove node;
    node.node_data
  end

let take_r seq =
  if is_empty seq then
    raise Empty
  else begin
    let node = seq.prev in
    remove node;
    node.node_data
  end

let take_opt_l seq =
  if is_empty seq then
    None
  else begin
    let node = seq.next in
    remove node;
    Some node.node_data
  end

let take_opt_r seq =
  if is_empty seq then
    None
  else begin
    let node = seq.prev in
    remove node;
    Some node.node_data
  end

let transfer_l s1 s2 =
  s2.next.prev <- s1.prev;
  s1.prev.next <- s2.next;
  s2.next <- s1.next;
  s1.next.prev <- s2;
  s1.prev <- s1;
  s1.next <- s1

let transfer_r s1 s2 =
  s2.prev.next <- s1.next;
  s1.next.prev <- s2.prev;
  s2.prev <- s1.prev;
  s1.prev.next <- s2;
  s1.prev <- s1;
  s1.next <- s1

let iter_l f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = curr in
      if node.node_active then f node.node_data;
      loop node.next
    end
  in
  loop seq.next

let iter_r f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = curr in
      if node.node_active then f node.node_data;
      loop node.prev
    end
  in
  loop seq.prev

let iter_node_l f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = curr in
      if node.node_active then f node;
      loop node.next
    end
  in
  loop seq.next

let iter_node_r f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = curr in
      if node.node_active then f node;
      loop node.prev
    end
  in
  loop seq.prev

let fold_l f seq acc =
  let rec loop curr acc =
    if curr == seq then
      acc
    else
      let node = curr in
      if node.node_active then
        loop node.next (f node.node_data acc)
      else
        loop node.next acc
  in
  loop seq.next acc

let fold_r f seq acc =
  let rec loop curr acc =
    if curr == seq then
      acc
    else
      let node = curr in
      if node.node_active then
        loop node.prev (f node.node_data acc)
      else
        loop node.prev acc
  in
  loop seq.prev acc

let find_node_l f seq =
  let rec loop curr =
    if curr != seq then
      let node = curr in
      if node.node_active then
        if f node.node_data then
          node
        else
          loop node.next
      else
        loop node.next
    else
      raise Not_found
  in
  loop seq.next

let find_node_r f seq =
  let rec loop curr =
    if curr != seq then
      let node = curr in
      if node.node_active then
        if f node.node_data then
          node
        else
          loop node.prev
      else
        loop node.prev
    else
      raise Not_found
  in
  loop seq.prev

let find_node_opt_l f seq =
  try Some (find_node_l f seq) with Not_found -> None

let find_node_opt_r f seq =
  try Some (find_node_r f seq) with Not_found -> None
