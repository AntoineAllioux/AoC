type ('b, 'a) t = 
  | Left of 'b
  | Right of 'a

module Right_focused (B : sig type t end): Monad.S with type 'a t = (B.t, 'a) t = struct
  type nonrec 'a t = (B.t, 'a) t

  include Monad.Make(struct
    type nonrec 'a t = 'a t

    let pure x = Right(x)

    let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t = 
      match x with
      | Right(a) -> f a
      | Left(b) -> Left(b)
  end)
end
