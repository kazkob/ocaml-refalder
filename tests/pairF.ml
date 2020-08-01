open Refolder

module Make (T : sig
  type t
end) =
struct
  module Def = struct
    type 'a f = T.t * 'a
  end

  module Functor : Functor.S with type 'a f = 'a Def.f = struct
    include Def

    type 'a f = 'a Def.f

    let map_cps f (n, a) k = f a @@ fun b -> k (n, b)
  end

  module FunctorM (M : Monad.S) :
    FunctorM.S with type 'a f = 'a Def.f and module M = M = struct
    include Def
    module M = M

    type 'a f = 'a Def.f

    let mapM_cps f (n, a) k =
      let ( let* ) = M.bind in
      f a @@ fun mb ->
      k
      @@ let* b = mb in
         M.pure @@ (n, b)
  end

  include Def
end
