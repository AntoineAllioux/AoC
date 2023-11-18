open Monad

module type S = sig
  type 'a t

  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module Fold_map (M : Monad.S) (F : S) = struct
  open Let_syntax (M)

  let fold_map (l : 'a F.t) ~(init : 'b) ~(f : 'b -> 'a -> 'b M.t) : 'b M.t =
    let aux acc x = 
      let* acc' = acc in
      f acc' x in
    F.fold l ~init:(M.pure init) ~f:aux
end
