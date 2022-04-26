(* Fairly hacky way of supporting CM files, call `export` in the repl to
 * generate a heap image, then you an run it:
 *    sml @SMLload cm-sources.amd64-darwin <your .cm files here>
 * and it will print all associated `sml` files.
 *)
structure CMSources :
  sig
    val main : string * string list -> int
    val export : unit -> unit
  end =
  struct

    infix |>
    fun x |> f = f x

    fun getSources f =
      Option.map (List.map # file o List.filter (fn x => # class x = "sml"))
        (CM.sources NONE f)

    fun isCM f = if String.isSuffix ".cm" f then SOME f else NONE

    fun printSources sources =
      sources
      |> List.mapPartial isCM
      |> List.mapPartial getSources
      |> List.concat
      |> String.concatWith " "
      |> print

    fun main (prog_name, args) =
      let
        val () = # set CM.Control.verbose false
        val () = printSources args
      in
        OS.Process.success
      end

    fun export () = SMLofNJ.exportFn ("cm-sources", main)

  end
