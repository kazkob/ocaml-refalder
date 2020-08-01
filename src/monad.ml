module type S = sig
  type 'a t

  val pure : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end
