module type Functor = sig
  type 'a t
  val return : 'a -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module Monad = struct
  module type S = sig
    include Functor  
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Utils(M : S) = struct
    type 'a monad = 'a M.t

    let rec foldm init f l =
      match l with
      | [] -> M.return init
      | h :: t -> M.bind (f init h) (fun x -> foldm x f t) 
  end
end

module StateMonad = struct
  
  module type S = sig
    include Monad.S
    type s
    val get : s t
    val put : s -> unit t
    val modify : (s -> s) -> unit t
    val eval : 'a t -> s -> 'a
  end

  module Make (S : sig type t end) : S with type s = S.t = struct
    type s = S.t
    type 'a t = S.t -> ('a * S.t)
    
    let return a s = (a, s)
    
    let map x f s =
      let (a, s) = x s in
      (f a , s)
    
    let bind x f s =
      let (a, s) = x s in
      f a s
    
    let get s = (s, s)
    
    let put s _ = ((), s)

    let modify f s = ((), f s)

    let eval x s = fst (x s)  
  end
end

module ReaderMonad = struct
  
  module type S = sig
    include Monad.S
    type s
    val get : s t
    val local : (s -> s) -> 'a t -> 'a t
  end

  module Make (S : sig type t end) : S = struct
    type s = S.t
    type 'a t = S.t -> 'a
    
    let return a _ = a
    
    let map x f s = f (x s)
    
    let bind x f s = f (x s) s
    
    let get s = s
    
    let local f x s = x (f s)
  end
end

let rec int_fold (n : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if n <= 0 then
    init
  else
    int_fold (n - 1) ~init:(f init n) ~f

let range_fold_aux (start : int) (stop : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  let len = abs(stop - start) in
  let sign = if stop - start >= 0 then 1 else -1 in
  let f' acc x = f acc (stop - sign * (x - 1)) in
  int_fold (len + 1) ~init ~f:f'

let range_fold_incr (start : int) (stop : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if stop < start then
    init
  else
    range_fold_aux start stop ~init ~f

let range_fold_decr (start : int) (stop : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if stop > start then
    init
  else
    range_fold_aux start stop ~init ~f
