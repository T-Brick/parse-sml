(* Mostly taken from the following places:
 * https://github.com/MPLLang/mpl/blob/master/lib/stubs/common/mlton/process-via-fork-exec.sml
 * https://github.com/MLton/mlton/blob/master/basis-library/mlton/process.sml
 * with some modifications
 *)

(* Copyright (C) 2013,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
structure MLtonProcess =
struct
  type pid = Posix.Process.pid

  local
      fun mk (exec, args) =
        case Posix.Process.fork () of
            NONE => exec args
          | SOME pid => pid
  in
      fun spawne {path, args, env} =
        mk (Posix.Process.exece, (path, args, env))
      fun spawnp {file, args} =
        mk (Posix.Process.execp, (file, args))
  end

  fun spawn {path, args} =
      spawne {path = path, args = args,
              env = Posix.ProcEnv.environ ()}
end

(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2002-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
structure MLton =
struct
  val isMLton = false

  fun eq (a : 'a, b : 'a) = (Unsafe.cast a : word) = Unsafe.cast b
  (* fun equal (a : 'a, b : 'a) = (Unsafe.cast a : ''b) = Unsafe.cast b *)

  structure Exn =
    struct
      val history = SMLofNJ.exnHistory
    end

  structure GC =
    struct
      fun collect () = SMLofNJ.Internals.GC.doGC 8
      fun pack () = collect ()
    end

  structure Process =
  struct
    open MLtonProcess

    structure OldIO = IO
    local
      open Posix
    in
      structure FileSys = FileSys
      structure IO = IO
      structure ProcEnv = ProcEnv
      structure Process = Process
      (* structure FileDesc = PrePosix.FileDesc
      structure PId = PrePosix.PId *)
      structure Signal = Signal
    end

    exception MisuseOfForget
    exception DoublyRedirected

    type input = unit
    type output = unit

    type none = unit
    type chain = unit
    type any = unit

    val readWrite =
      let
          open FileSys.S
      in
          flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
      end

    structure Child =
    struct
      datatype 'use childt =
          FileDesc of IO.file_desc
        | Stream of 'use * ('use -> unit)
        | Term
      type ('use, 'dir) t = 'use childt ref

      (* This is _not_ the identity; by rebuilding it we get type
        * ('a, 'b) t -> ('c, 'd) t
        *)
      fun remember x =
          case !x of 
            FileDesc f =>
                (x := Stream ((), fn () => ())
                ; ref (FileDesc f))
          | Stream _ => raise MisuseOfForget (* remember twice = bad *)
          | Term => ref Term

      local
        fun convert (new, close) p =
          case !p of
              FileDesc fd =>
                let
                    val str = new (fd, "<process>")
                    val () = p := Stream (str, close)
                in
                    str
                end
            | Stream (str, _) => str
            | Term => raise MisuseOfForget

          val buf_size = 4096

          fun binio_newIn (fd, name) =
            let
                val reader = Posix.IO.mkBinReader {fd = fd, initBlkMode = true, name = name}
            in
                BinIO.StreamIO.mkInstream (reader, IO.readVec (fd, buf_size))
            end
          fun textio_newIn mk mkSIO vec (fd, name) =
            let
                val reader = Posix.IO.mkTextReader {fd = fd, initBlkMode = true, name = name}
            in
                TextIO.StreamIO.mkInstream (reader, "")
            end
          fun newOut mk mkSIO (fd, name) =
            let
                val writer = mk {appendMode = false, chunkSize = 4096, fd = fd, initBlkMode = true, name = name}
                val buffer_mode =
                  if Posix.ProcEnv.isatty fd
                  then OldIO.LINE_BUF
                  else OldIO.BLOCK_BUF
            in
                mkSIO (writer, buffer_mode)
            end
      in
          val binIn = convert (binio_newIn, Unsafe.cast BinIO.closeIn)
          val binOut = convert (newOut Posix.IO.mkBinWriter BinIO.StreamIO.mkOutstream, Unsafe.cast BinIO.closeOut)
          val textIn = convert (textio_newIn, Unsafe.cast TextIO.closeIn)
          val textOut = convert (newOut Posix.IO.mkTextWriter TextIO.StreamIO.mkOutstream, Unsafe.cast TextIO.closeOut)
      end

      fun fd p =
          case !p of
            FileDesc fd => fd
          | _ => raise MisuseOfForget

      fun close ch =
          case ch of
            FileDesc fd => IO.close fd
          | Stream (str, close) => close str
          | Term => ()

      val close =
          fn (stdin, stdout, stderr) =>
          (close stdin; close stdout; close stderr)
    end
  end
end
