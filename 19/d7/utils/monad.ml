open Function

module type S = sig
  include Applicative.S

  val bind: 'a t -> f:('a -> 'b t) -> 'b t
end

module type Basic = sig
  type 'a t
  val pure: 'a -> 'a t 
  val bind: 'a t -> f:('a -> 'b t) -> 'b t
end 

module Make(M : Basic) : S with type 'a t := 'a M.t = struct
  include M
  let map x ~f = bind x ~f:(pure $ f)
  let app ~f x = bind f ~f:(fun f -> map x ~f)
end

module Let_syntax (M : S) = struct
  let ( let* ) x f = M.bind x ~f 
end
