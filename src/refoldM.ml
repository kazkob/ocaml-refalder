module type S = sig
  module F : FunctorM.S

  module M : Monad.S

  type ('a, 'b) unfolder = 'a -> [ `Return of 'b | `Continue of 'a F.f ] M.t

  type 'b folder = 'b F.f -> 'b M.t

  val refoldM : ('a, 'b) unfolder -> 'b folder -> 'a -> 'b M.t
end

module Make (F : FunctorM.S) : S with module F = F and module M = F.M = struct
  module F = F
  module M = F.M

  type ('a, 'b) unfolder = 'a -> [ `Return of 'b | `Continue of 'a F.f ] M.t

  type 'b folder = 'b F.f -> 'b M.t

  let refoldM f g x =
    let rec aux x k =
      M.bind (f x) @@ function
      | `Return v -> M.pure v
      | `Continue af -> F.mapM_cps aux af @@ fun bfm -> k @@ M.bind bfm g
    in
    aux x (fun x -> x)
end
