exception VersionError of string

val get_program : in_channel -> L.prog
val check_version : int -> L.expr -> unit
