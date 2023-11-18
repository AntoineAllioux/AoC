open Function

module type S = sig
  type 'a t

  include Foldable.S with type 'a t := 'a t

  module App : functor (F : Applicative.S) -> sig 
    val traverse: 'a t -> f:('a -> 'b F.t) -> 'b t F.t
  end
end

module Functor (T : S) : Functor.S with type 'a t = 'a T.t = struct 
  type 'a t = 'a T.t
  module TI = T.App(Identity.S)

  let map (x : 'a t) ~(f : 'a -> 'b) : 'b t =
    TI.traverse ~f x 
end

module Sequence (T: S) (F: Applicative.S) = struct
  open T
  module TF = T.App (F)

  let sequence (x: 'a F.t t) : 'a t F.t =
    TF.traverse ~f:id x
end  

(* horrendous *)
module Map_accum (M : Monad.S) (T : S) (B : sig type t end) = struct
  open T

  module State = State.MakeT(M)(struct type t = B.t end)
  module TM = T.App(State)

  let map_accum (l : 'a t) ~(init : B.t) ~(f : B.t -> 'a -> ('c * B.t) M.t) : ('c t * B.t) M.t =
    TM.traverse ~f:(flip f) l init
end
