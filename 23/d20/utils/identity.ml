module S : Monad.S with type 'a t = 'a = struct
  type 'a t = 'a

  include Monad.Make (struct
    type 'a t = 'a

    let bind x ~f = f x
    let pure x = x
  end)
end
