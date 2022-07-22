module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Syntax (M : Monad) = struct
  let ( >>= ) = M.bind
  let ( let* ) = M.bind
end

module OptionBase : Monad with type 'a t = 'a option = struct
  type 'a t = 'a option

  let return x = Some x
  let bind o f = match o with Some x -> f x | None -> None
end

module OptionSyntax = Syntax (OptionBase)

module Option = struct
  include OptionBase
  include OptionSyntax
end
