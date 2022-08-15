module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module MonadSyntax : functor (M : Monad) -> sig
  val ( >>= ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
  val ( let* ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
end

module type Applicative = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
end

module ApplicativeSyntax : functor (A : Applicative) -> sig
  val ( >>| ) : 'a A.t -> ('a -> 'b) -> 'b A.t
  val ( let+ ) : 'a A.t -> ('a -> 'b) -> 'b A.t
  val ( and+ ) : 'a A.t -> 'b A.t -> ('a * 'b) A.t
end

module OptionBaseMonad : sig
  type 'a t = 'a option

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module OptionBaseApplicative : sig
  type 'a t = 'a option

  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
end

module Option : sig
  val return : 'a -> 'a option
  val bind : 'a option -> ('a -> 'b option) -> 'b option

  val ( >>= ) :
    'a OptionBaseMonad.t -> ('a -> 'b OptionBaseMonad.t) -> 'b OptionBaseMonad.t

  val ( let* ) :
    'a OptionBaseMonad.t -> ('a -> 'b OptionBaseMonad.t) -> 'b OptionBaseMonad.t

  type 'a t = 'a option

  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t

  val ( >>| ) :
    'a OptionBaseApplicative.t -> ('a -> 'b) -> 'b OptionBaseApplicative.t

  val ( let+ ) :
    'a OptionBaseApplicative.t -> ('a -> 'b) -> 'b OptionBaseApplicative.t

  val ( and+ ) :
    'a OptionBaseApplicative.t ->
    'b OptionBaseApplicative.t ->
    ('a * 'b) OptionBaseApplicative.t
end

module ListBaseMonad : sig
  type 'a t = 'a list

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module ListBaseApplicative : sig
  type 'a t = 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
end

module List : sig
  val return : 'a -> 'a list
  val bind : 'a list -> ('a -> 'b list) -> 'b list

  val ( >>= ) :
    'a ListBaseMonad.t -> ('a -> 'b ListBaseMonad.t) -> 'b ListBaseMonad.t

  val ( let* ) :
    'a ListBaseMonad.t -> ('a -> 'b ListBaseMonad.t) -> 'b ListBaseMonad.t

  type 'a t = 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t

  val ( >>| ) :
    'a ListBaseApplicative.t -> ('a -> 'b) -> 'b ListBaseApplicative.t

  val ( let+ ) :
    'a ListBaseApplicative.t -> ('a -> 'b) -> 'b ListBaseApplicative.t

  val ( and+ ) :
    'a ListBaseApplicative.t ->
    'b ListBaseApplicative.t ->
    ('a * 'b) ListBaseApplicative.t
end
