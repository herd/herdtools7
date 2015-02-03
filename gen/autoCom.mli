type mach = Local | Distant of string

module type Config = sig
  val addpath : string list
  val target : mach
end

module type S =  sig
  val dist_sh : string -> unit
  val dist_sh_save : string -> string -> unit
  val dist_upload : string -> string -> unit
  val dist_download : string -> string -> unit
end

module Make(C:Config) : S
