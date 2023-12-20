module type S = sig
  include Monad.S

  type s

  val get : s t
  val put : s -> unit t
  val modify : (s -> s) -> unit t
end

module Is_monad
    (M : Monad.S)
    (S : sig
      type t
    end) : Monad.S with type 'a t = S.t -> ('a * S.t) M.t = struct
  type 'a t = S.t -> ('a * S.t) M.t

  include Monad.Make (struct
    type 'a t = S.t -> ('a * S.t) M.t

    let bind x ~f s = M.bind (x s) ~f:(fun (y, s') -> f y s')
    let pure a s = M.pure (a, s)
  end)
end

module MakeT
    (M : Monad.S)
    (S : sig
      type t
    end) =
struct
  include Is_monad (M) (S)

  type s = S.t

  let get s = M.pure (s, s)
  let put s _ = M.pure ((), s)
  let modify f s = M.pure ((), f s)
  let run_state (x : 'a t) (s : s) = x s
  let eval_state x s = M.map (x s) ~f:fst
  let lift (x : 'a M.t) : 'a t = fun s -> M.bind x ~f:(fun y -> M.pure (y, s))

  let unlift_fun (f : ('a -> 'b) t) : 'a -> 'b t =
   fun x s -> M.map (f s) ~f:(fun (f', s') -> (f' x, s'))
end

module Make (S : sig
  type t
end) =
  MakeT (Identity.S) (S)
