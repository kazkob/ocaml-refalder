open Refolder

module Make (T : sig
  type t
end) =
struct
  module Def = struct
    type 'a f = Nil | Cons of T.t * 'a
  end

  module Functor : Functor.S with type 'a f = 'a Def.f = struct
    include Def

    let map_cps f m k =
      match m with
      | Nil -> k Nil
      | Cons (n, a) -> f a @@ fun b -> k (Cons (n, b))
  end

  module FunctorM (M : Monad.S) : FunctorM.S with type 'a f = 'a Def.f = struct
    include Def
    module M = M

    type 'a f = 'a Def.f

    let mapM_cps f m k =
      let ( let* ) = M.bind in
      match m with
      | Nil -> k (M.pure Nil)
      | Cons (n, a) ->
          f a @@ fun mb ->
          k
          @@ let* b = mb in
             M.pure @@ Cons (n, b)
  end

  include Def
end
