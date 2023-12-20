module type S = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end
