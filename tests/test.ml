open Refolder

type 'a bin = Leaf of 'a | Node of 'a bin * 'a bin

let rec flip = function Leaf n -> Leaf n | Node (l, r) -> Node (flip r, flip l)

let leaf x = Leaf x

let node l r = Node (l, r)

let example_tree =
  node
    (node (leaf 0) (leaf 1))
    (node (node (leaf 2) (leaf 3)) (node (leaf 4) (leaf 5)))

module IntBinF = BinF.Make (struct
  type t = int
end)

module Refold = Refold.Make (IntBinF.Functor)

let () =
  let open IntBinF.Def in
  let unfolder = function
    | Leaf v -> `Continue (LeafF v)
    | Node (l, r) -> `Continue (NodeF (l, r))
  in
  let copy_folder = function
    | LeafF v -> Leaf v
    | NodeF (l, r) -> Node (l, r)
  in
  assert (Refold.refold unfolder copy_folder example_tree = example_tree);
  let sum_folder = function LeafF v -> v | NodeF (l, r) -> l + r in
  assert (Refold.refold unfolder sum_folder example_tree = 15)

module Log = struct
  type 'a t = string list -> string list * 'a

  let pure : 'a -> 'a t = fun x ss -> (ss, x)

  let bind m k ss =
    let ss, x = m ss in
    k x ss

  let log : int -> string -> unit t =
   fun n s ss -> ((String.make n ' ' ^ s) :: ss, ())

  (*
  module Monad : Monad.S = struct

    type 'a t = 'a log
    let pure = pure
    let bind = bind
  end
     *)
end

module WithInt = PairF.Make (struct
  type t = int
end)

module FM = FunctorM.Compose (IntBinF.FunctorM) (WithInt.FunctorM) (Log)
module RM = RefoldM.Make (FM)

let () =
  let open IntBinF.Def in
  let unfolder : (int * int bin, 'a) RM.unfolder =
   fun (n, t) ->
    match t with
    | Leaf v ->
        Log.bind (Log.log n (Printf.sprintf "%d" v)) @@ fun () ->
        Log.pure (`Continue (n, LeafF v))
    | Node (l, r) ->
        Log.bind (Log.log n "+") @@ fun () ->
        Log.pure (`Continue (n, NodeF ((n + 1, l), (n + 1, r))))
  in
  let folder (_, t) =
    Log.pure (match t with LeafF v -> Leaf v | NodeF (l, r) -> Node (r, l))
  in
  let log, t = RM.refoldM unfolder folder (0, example_tree) [] in
  let log = List.rev log in
  assert (t = flip example_tree);
  List.iter print_endline log;
  assert (
    log
    = [
        "+";
        " +";
        "  0";
        "  1";
        " +";
        "  +";
        "   2";
        "   3";
        "  +";
        "   4";
        "   5";
      ] )
