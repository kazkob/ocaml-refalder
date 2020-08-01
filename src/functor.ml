module type S = sig
  type 'a f

  val map_cps : ('a -> ('b, 'r) Cps.t) -> 'a f -> ('b f, 'r) Cps.t
end

module Compose (F1 : S) (F2 : S) = struct
  module Functor : S = struct
    type 'a f = 'a F1.f F2.f

    let map_cps f m k = F2.map_cps (F1.map_cps f) m k
  end
end
