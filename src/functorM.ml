module type S = sig
  module M : Monad.S

  type 'a f

  val mapM_cps : ('a -> ('b M.t, 'r) Cps.t) -> 'a f -> ('b f M.t, 'r) Cps.t
end

module type Maker = functor (M : Monad.S) -> S with module M = M

module Compose (F1 : Maker) (F2 : Maker) (M : Monad.S) :
  S with module M = M and type 'a f = 'a F1(M).f F2(M).f = struct
  module F1 = F1 (M)
  module F2 = F2 (M)
  module M = M

  type 'a f = 'a F1.f F2.f

  let mapM_cps f m k = F2.mapM_cps (F1.mapM_cps f) m k
end
