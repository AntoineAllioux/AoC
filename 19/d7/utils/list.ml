include Base.List 

type 'a t = 'a list

module List_monad: Monad.S with type 'a t := 'a list = struct
  include Monad.Make(struct
    type 'a t = 'a list
    
    let pure x = [x]
    
    let rec bind l ~f = match l with
    | [] -> []
    | h :: t -> append (f h) (bind ~f t)
  end)
end

module List_foldable: Foldable.S with type 'a t := 'a list = struct
  type 'a t = 'a list

  let rec fold (l : 'a t) ~(init : 'b) ~(f : 'b -> 'a -> 'b): 'b =
    match l with
    | [] -> init
    | h :: t -> fold t ~init:(f init h) ~f
end 

module List_traversable (*: Traversable.S*) = struct
  type 'a t = 'a list

  include List_foldable
  include List_monad

  module App (F: Applicative.S) = struct
    let rec traverse (l : 'a t) ~(f : 'a -> 'b F.t) : 'b t F.t =
    match l with
    | [] -> F.pure []
    | h :: t -> F.app ~f:(F.app ~f:(F.pure cons) (f h)) (traverse t ~f) 
  end
end 

let rec range start stop = 
  match stop - start with 
  | 0 -> []
  | _ -> start :: range (start + 1) stop

let rec arrangements elements length = 
  match length with
  | 0 -> []
  | 1 -> Base.List.map elements ~f:(fun x -> [x])
  | _ -> 
    let hyp = arrangements elements (length - 1) in
    Base.List.bind elements ~f:(fun x -> Base.List.map hyp ~f:(fun l -> x :: l))

let rec permutations elements = 
  let open Base.List in
  match elements with
  | [] -> []
  | [x] -> [[x]]
  | h :: t -> 
    let hyp = permutations t in 
    let aux l i =
      let (left, right) = split_n l i in
      left @ (h :: right) in
    hyp >>= (fun l -> map (range 0 (length l + 1)) ~f:(aux l))