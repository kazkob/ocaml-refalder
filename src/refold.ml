module type S = sig
  module F : Functor.S

  type ('a, 'b) unfolder = 'a -> [ `Return of 'b | `Continue of 'a F.f ]

  type 'b folder = 'b F.f -> 'b

  val refold : ('a, 'b) unfolder -> 'b folder -> 'a -> 'b
end

module Make (F : Functor.S) : S with module F = F = struct
  module F = F

  type ('a, 'b) unfolder = 'a -> [ `Return of 'b | `Continue of 'a F.f ]

  type 'b folder = 'b F.f -> 'b

  let refold f g x =
    let rec aux x k =
      match f x with
      | `Return v -> v
      | `Continue t -> F.map_cps aux t (fun t -> k @@ g t)
    in
    aux x (fun x -> x)
end
