open Refolder

module Make (T : sig
  type t
end) =
struct
  module Def = struct
    type 'a f = LeafF of T.t | NodeF of 'a * 'a
  end

  module Functor : Functor.S with type 'a f = 'a Def.f = struct
    include Def

    let map_cps f m k =
      match m with
      | LeafF v -> k (LeafF v)
      | NodeF (l, r) ->
          f l @@ fun l ->
          f r @@ fun r -> k (NodeF (l, r))
  end

  module FunctorM (M : Monad.S) :
    FunctorM.S with type 'a f = 'a Def.f and module M = M = struct
    include Def
    module M = M

    type 'a f = 'a Def.f

    let mapM_cps f m k =
      let ( let* ) = M.bind in
      match m with
      | LeafF v -> k @@ M.pure (LeafF v)
      | NodeF (l, r) ->
          f l @@ fun l ->
          f r @@ fun r ->
          k
          @@ let* l = l in
             let* r = r in
             M.pure @@ NodeF (l, r)
  end

  include Def
end
