(** Minimal shell library for Ocaml

    Feather is a minimal shell library for Ocaml with lightweight, posix-like
    syntax.

    You can create a command using the function {{!process} process}, or any
    {{!commands}basic commands}, and combined with {{!operators}operators} and/or
    {{!redirection}file redirections}. Then, use any {{!exec}Execute commands} to
    execute it, and {{!colsect} collect} status and/or stdout and/or stdree as
    you like.

    {[
      open Feather
      open Feather.Infix

      let stdout, status =
        echo "Hello world !" >! devnull |> collect stdout_and_status
      in
      Printf.printf "Echo returned %d and printed \"%s\" on stdout.\n" status stdout
      (* This should print "Echo returned 0 and printed "Hello world !" on stdout." *)
    ]}

*)

open Base

(** {1 Command constructors} *)

type cmd
(** {!type:cmd} is the main type used by feather to represent commands. *)

val process : string -> string list -> cmd
(** [process cmd args] constructs a new command [cmd] from a string, with [args]
    the list of options for this command. *)

(** {2 Operators} *)

val ( |. ) : cmd -> cmd -> cmd
(** [a |. b] is feather's version of a [a | b] in bash. [a] will be run in
    background and its [stdout] will be redirected to [b]'s [stdin] *)

val and_ : cmd -> cmd -> cmd
(** [and_] is feather's version of [&&] in bash. See {{!module:Infix}Infix}
    module for more. *)

val or_ : cmd -> cmd -> cmd
(** [or_] is feather's version of [||] in bash. See {{!module:Infix}Infix}
    module for more. *)

val sequence : cmd -> cmd -> cmd
(** [sequence] is feather's version of [;] in bash. See
    {{!module:Infix}Infix} module for more. *)

(** {2 Map functions} *)

val map_lines : f:(string -> string) -> cmd
(** [map_lines] within a sequence of pipes will be run with a thread.
    Same goes for [filter_lines], [mapi_lines], etc. *)

val filter_lines : f:(string -> bool) -> cmd

val mapi_lines : f:(string -> int -> string) -> cmd

val filteri_lines : f:(string -> int -> bool) -> cmd

val filter_map_lines : f:(string -> string option) -> cmd

val filter_mapi_lines : f:(string -> int -> string option) -> cmd


(** {2:redirection Redirection functions} *)

val write_stdout_to : string -> cmd -> cmd
(** Redirect stdout to a given file. See {{!module:Infix}Infix} module for
    more. *)

val append_stdout_to : string -> cmd -> cmd
(** Redirect and append stdout to a given file. See {{!module:Infix}Infix}
    module for more. *)

val write_stderr_to : string -> cmd -> cmd
(** Redirect stderr to a given file. See {{!module:Infix}Infix} module for
    more. *)

val append_stderr_to : string -> cmd -> cmd
(** Redirect and append stderr to a given file. See {{!module:Infix}Infix}
    module for more. *)

val read_stdin_from : string -> cmd -> cmd
(** Use a file as input. See {{!module:Infix}Infix} module for more. *)

val stdout_to_stderr : cmd -> cmd
(** Redirect stdout to stderr, not composable with {!stderr_to_stdout}.
    Applying both will result in no output to either stdout or stderr. *)

val stderr_to_stdout : cmd -> cmd
(** Redirect stderr to stdout, not composable with {!stdout_to_stderr}.
    Applying both will result in no output to either stdout or stderr. *)

(** {2:operators Infix operators and redirections} *)

module Infix : sig
  val ( &&. ) : cmd -> cmd -> cmd
  (** Same as {!and_} *)

  val ( ||. ) : cmd -> cmd -> cmd
  (** Same as {!or_} *)

  val ( ->. ) : cmd -> cmd -> cmd
  (** Same as {!sequence}

      [(->.)] binds more tightly than {!(|.)} so parentheses should be used when
      chaining the two.
  *)

  val ( > ) : cmd -> string -> cmd
  (** Same as {!write_stdout_to} *)

  val ( >> ) : cmd -> string -> cmd
  (** Same as {!append_stdout_to} *)

  val ( >! ) : cmd -> string -> cmd
  (** Same as {!write_stderr_to} *)

  val ( >>! ) : cmd -> string -> cmd
  (** Same as {!append_stderr_to} *)

  val ( < ) : cmd -> string -> cmd
  (** Same as {!read_stdin_from} *)
end

(** {1:colsect Collect Section} *)

type 'a what_to_collect
(** The type that determines what should be returned by {{!collect}collect} *)

val stdout : string what_to_collect

val stderr : string what_to_collect

val status : int what_to_collect

val stdout_and_stderr : (string * string) what_to_collect

val stdout_and_status : (string * int) what_to_collect

val stderr_and_status : (string * int) what_to_collect

type everything = { stdout : string; stderr : string; status : int }

val everything : everything what_to_collect

(** Various collection possibilities, to be used with {{!collect}collect} *)

(** {1:exec Execute a command} *)

val collect :
  ?cwd:string -> ?env:(string * string) list -> 'a what_to_collect -> cmd -> 'a
(** [collect col cmd] runs [cmd], collecting the outputs specified by [col]
    along the way and returning them. The return type depends on what is
    collected, see {{!colsect} Collect section} for more. *)

val run : ?cwd:string -> ?env:(string * string) list -> cmd -> unit
(** [run cmd] runs [cmd] without collecting anything. *)

val run_bg : ?cwd:string -> ?env:(string * string) list -> cmd -> unit
(** [run_bg cmd] runs [cmd] without collecting anything, in a thread.
    Use [wait] to ensure that the parent won't exit, subsequently killing the
    background process. *)

(** {1:commands Basic commands} *)

val ls : string -> cmd
(** Similar to bash [ls] command, without any option. [ls path] will list the
    content the directory [path] *)

val find :
  ?include_starting_dir:bool ->
  ?ignore_hidden:bool ->
  ?kind:[ `Files | `Directories ] ->
  ?name:string ->
  ?depth:int ->
  string ->
  cmd
(** [find [options] path] will lists the content of [path], filtering using
    given optionnal parameters. [?include_starting_dir] specifies whether to
    include the starting directory passed into [find] (defaults to [false],
    notably different than the unix find utility). [?ignore_hidden] can be used
    to ignore hidden files (default to [false]). You can also filter you result
    by [?name] and/or [?kind], as well as setting the maximum search depth using
    [?depth] (default to infinity).
*)

val sh : string -> cmd
(** Similar to bash [sh], without any option. [sh commands] will execute
    [commands] written in Shell Command Language. *)

val rg : ?in_:string -> string -> cmd
(** ripgrep command. [rg ~_in:path pattern] will filter the content of [path]
    to find everything which match [pattern]. If [?_in] is not
    specified, filter stdin instead. *)

val rg_v : ?in_:string -> string -> cmd
(** Same as {!rg}, but find everything that does {b not} match the [pattern]. *)

val grep : ?in_:string -> string -> cmd
(** Similar to bash [grep] command. [grep ~_in:file pattern] will find every
    line in [file] which match [pattern]. If [?_in] is not specified, filter
    stdin instead. *)

val cat : string -> cmd
(** [cat file] redirects [file] to standard output. *)

val less : cmd
(** [less] reads standard output. *)

val mkdir : string -> cmd
(** [mkdir dir] will create a new directory with path [dir]. *)

val mkdir_p : string -> cmd
(** Same as {!mkdir}, but create parent directories if needed. *)

val sort : cmd

val uniq : cmd

val shuf : cmd

val head : ?file:string -> int -> cmd

val tail : ?file:string -> int -> cmd

val tail_f : string -> cmd

val echo : string -> cmd

val cut' : ?complement:unit -> ?d:char -> int list -> cmd

val cut : ?d:char (** defaults to a space *) -> int -> cmd

val cp : string -> string -> cmd

val cp_r : string -> string -> cmd

val mv : string -> string -> cmd

val pwd : cmd

val sed :
  ?g:bool (** defaults to TRUE *) ->
  string (** pattern *) ->
  string (** replacement *) ->
  cmd

val tr : string -> string -> cmd

val tr_d : string -> cmd

(** {1 Utility functions} *)

val of_list : string list -> cmd
(** Redirect a list of string to standard output *)

val lines : string -> string list
(** Transforms a string into the list of its lines *)

val devnull : string
(** [devnull] is an alias for [/dev/null] *)

val fzf : ?cwd:string -> ?env:(string * string) list -> cmd -> string option
(** [fzf] runs the command, and fuzzy finds the stdout.
    Returns [None] if no item was chosen, [Some str] otherwise

    Note that [fzf] is a way to to run a [cmd] and does not in itself return a
    [cmd]. *)

val debug : bool ref
(** If true, each [command] will be printed to stdout along with current time *)
