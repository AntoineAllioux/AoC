module Make (F : Functor.S) = struct
  type 'a t =
    | Pure of 'a
    | Free of 'a t F.t

  include Monad.Make(struct
    type nonrec 'a t = 'a t

    let pure x = Pure(x)

    let rec bind (x : 'a t) ~(f : 'a -> 'b t): 'b t = 
      match x with
      | Pure a -> f a
      | Free y -> Free (F.map y ~f:(fun x -> bind x ~f))
  end)

  let lift (x : 'a F.t): 'a t =
    Free (F.map x ~f:pure)

  module Fold_free (M : Monad.S) = struct 
  
    (* annoying lack of higher-rank polymorphism *)
    type trans = { apply: 'a. 'a F.t -> 'a M.t }
  
    let rec fold_free (f : trans) (x : 'a t): 'a M.t =
      match x with
      | Pure a -> M.pure a
      | Free y -> M.bind (f.apply y) ~f:(fold_free f)
  end
end

module Free_foldable (F : Functor.S) (F_foldable : Foldable.S with type 'a t = 'a F.t) : Foldable.S 
with type 'a t := 'a Make(F).t = struct
  type 'a t = 'a Make(F).t 

  let rec fold (x : 'a t) ~(init : 'b) ~(f : 'b -> 'a -> 'b) : 'b = 
    match x with
    | Pure a -> f init a
    | Free x ->  F_foldable.fold x ~init ~f:(fun init x -> fold x ~init ~f)
end

module Free_traversable (T : Traversable.S) : Traversable.S = struct
    module F = Traversable.Functor(T)
    include Make(F)
    include Free_foldable (F) (T)
  
    module App (F : Applicative.S) = struct
      module TF = T.App (F)

      let traverse (x : 'a t) ~(f : 'a -> 'b F.t) : 'b t F.t =
        let rec aux (x : 'a t) = match x with
        | Pure a ->  F.map (f a) ~f:(fun x -> Pure x)
        | Free x ->  F.map (TF.traverse x ~f:aux) ~f:(fun x -> Free x)
      in aux x
    end
  end