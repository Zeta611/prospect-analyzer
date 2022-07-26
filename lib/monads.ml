module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module MonadSyntax (M : Monad) = struct
  let ( >>= ) = M.bind
  let ( let* ) = M.bind
end

module type Applicative = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
end

module ApplicativeSyntax (A : Applicative) = struct
  let ( >>| ) o f = A.map f o
  let ( let+ ) = ( >>| )
  let ( and+ ) = A.product
end

module OptionBaseMonad : Monad with type 'a t = 'a option = struct
  type 'a t = 'a option

  let return x = Some x
  let bind o f = match o with None -> None | Some x -> f x
end

module OptionBaseApplicative : Applicative with type 'a t = 'a option = struct
  type 'a t = 'a option

  let map f = function None -> None | Some x -> Some (f x)

  let product o1 o2 =
    match (o1, o2) with Some x, Some y -> Some (x, y) | _ -> None
end

module Option = struct
  include OptionBaseMonad
  include MonadSyntax (OptionBaseMonad)
  include OptionBaseApplicative
  include ApplicativeSyntax (OptionBaseApplicative)
end

module ListBaseMonad : Monad with type 'a t = 'a list = struct
  type 'a t = 'a list

  let return x = [ x ]
  let bind o f = List.concat_map f o
end

module ListBaseApplicative : Applicative with type 'a t = 'a list = struct
  type 'a t = 'a list

  let map = List.map
  let product = List.combine
end

module List = struct
  include ListBaseMonad
  include MonadSyntax (ListBaseMonad)
  include ListBaseApplicative
  include ApplicativeSyntax (ListBaseApplicative)
end
