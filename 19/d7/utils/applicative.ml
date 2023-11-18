open Functor

module type S = sig
  include Functor.S

  val pure: 'a -> 'a t
  val app: f:('a -> 'b) t -> 'a t -> 'b t
end