(* --------------------------------------------------------------------------
   MIPS emulator (based on SML generated from an L3 specification)
   -------------------------------------------------------------------------- *)

(*
(* load library and model under Poly/ML *)

local
   fun path a s = OS.Path.toString {vol = "", isAbs = false, arcs = a @ [s]}
in
   fun useSigSml a s =
      ( use (path a (OS.Path.joinBaseExt {base = s, ext = SOME "sig"}))
      ; use (path a (OS.Path.joinBaseExt {base = s, ext = SOME "sml"}))
      )
end

val useLib = useSigSml (List.tabulate (3, fn _ => OS.Path.parentArc) @ ["lib"])

val () =
   ( List.app useLib
        ["IntExtra", "Nat", "L3", "Bitstring", "BitsN", "FP64", "Ptree",
         "MutableMap", "Runtime"]
   ; useSigSml [OS.Path.currentArc] "mips"
   );

*)

(* --------------------------------------------------------------------------
   Default Configuration
   -------------------------------------------------------------------------- *)

val be = ref true
val trace_level = ref 0
val time_run = ref true

(* --------------------------------------------------------------------------
   Loading code into memory from Hex file
   -------------------------------------------------------------------------- *)

fun hex s = L3.lowercase (BitsN.toHexString s)
fun phex n = fn s => StringCvt.padLeft #"0" (n div 4) (hex s)
fun word i = fn s => Option.valOf (BitsN.fromHexString (s, i))
val w8 = word 8
val w32 = word 32
val hex32 = phex 32
val hex64 = phex 64

fun failExit s = ( print (s ^ "\n"); OS.Process.exit OS.Process.failure )

local
   fun address s = Option.valOf (BitsN.fromHexString (s, 16))
   val endLine = ":00000001FF"
   fun inc16 a = BitsN.+ (a, BitsN.fromInt (1, 16))
   fun checkEnd s r = if s = endLine then r else raise Fail ("Bad line: " ^ s)
   fun readIntelHex s =
      if String.substring (s, 0, 3) <> ":04" orelse
         String.substring (s, 7, 2) <> "00" then NONE
      else let
              val a = address (String.substring (s, 3, 4))
              val b0 = w8 (String.substring (s, 9, 2))
              val b1 = w8 (String.substring (s, 11, 2))
              val b2 = w8 (String.substring (s, 13, 2))
              val b3 = w8 (String.substring (s, 15, 2))
              val l = [b0, b1, b2, b3]
           in
              SOME (a, BitsN.concat (if !be then List.rev l else l))
           end
in
   fun loadIntelHex s =
      let
         val strm = TextIO.openIn s
         val inp = TextIO.inputAll strm before TextIO.closeIn strm
         val l = String.tokens (fn c => c = #"\n") inp
      in
         case l of
            [] => raise Fail ("File empty: " ^ s)
          | h :: t =>
              (case readIntelHex h of
                  SOME (a0, w0) =>
                    let
                       val () = if a0 <> BitsN.fromInt (0, 16)
                                   then raise Fail
                                          ("Does not start at zero: " ^ s)
                                else ()
                       val (_, r) =
                          List.foldl
                             (fn (s, (pa, l)) =>
                                case readIntelHex s of
                                   SOME (a, w) =>
                                      if inc16 pa = a
                                         then (a, w :: l)
                                      else ( print (BitsN.toHexString pa)
                                           ; print "\n"
                                           ; print (BitsN.toHexString a)
                                           ; raise Fail ("Not sequential: " ^ s)
                                           )
                                 | NONE => checkEnd s (pa, l))
                             (a0, [w0]) t
                    in
                       Array.fromList (List.rev r)
                    end
                | NONE => checkEnd h (Array.fromList []))
      end
      handle IO.Io {function  = "TextIO.openIn", ...} =>
         failExit ("Failed to open file \"" ^ s ^ "\"")
end

(*
fun printHex a =
   let
      val problematic = ref ([] : BitsN.nbit list)
   in
      Array.appi
        (fn (i, w) =>
           let
              val s = mips.instructionToString (mips.Decode w)
              val () = if s = "???" then problematic := w :: !problematic
                       else ()
           in
              print (hex32 w ^ " " ^ s ^ "\n")
           end) a
    ; L3.mkSet (!problematic)
   end
*)

fun storeArrayInMem (base, marray) =
   let
      val b = base div 8
   in
      L3.for (0, Array.length marray div 2 - 1,
        fn i =>
           let
              val w1 = Array.sub (marray, 2 * i)
              val w2 = Array.sub (marray, 2 * i + 1)
              val dw = BitsN.@@ (w1, w2)
           in
              mips.MEM := mips.Map.update (!mips.MEM, b + i, dw)
           end
           handle Subscript => print (Int.toString i ^ "\n"))
    ; print (Int.toString (Array.length marray) ^ " words.\n")
   end

(* ------------------------------------------------------------------------
   Dump final state of registers
   ------------------------------------------------------------------------ *)

local
   fun readReg i = hex64 (mips.GPR (BitsN.fromNat (i, 5)))
in
   fun dumpRegisters () =
      ( print "======   Registers   ======\n"
      ; print ("PC     " ^ hex64 (!mips.PC) ^ "\n")
      ; L3.for
          (0, 31,
           fn i =>
              print ("Reg " ^ (if i < 10 then " " else "") ^
                     Int.toString i ^ " " ^ readReg i ^ "\n"))
      )
   fun dumpRegistersOnCP0_26 () =
      case !mips.log of
         [mips.w_c0 (BitsN.B (26, 5), _)] => dumpRegisters ()
       | _ => ()
end

local
   val sizeString =
      fn BitsN.B (0, 3) => "byte"
       | BitsN.B (1, 3) => "halfword"
       | BitsN.B (3, 3) => "word"
       | BitsN.B (7, 3) => "dword"
       | BitsN.B (n, _) => Int.toString (n + 1) ^ " bytes"
   val logString =
      fn mips.w_gpr (n, v) => "Reg " ^ BitsN.toString n ^ " <- 0x" ^ hex64 v
       | mips.w_hi v => "HI <- 0x" ^ hex64 v
       | mips.w_lo v => "LO <- 0x" ^ hex64 v
       | mips.w_c0 (n, v) => mips.cpr n ^ " <- 0x" ^ hex64 v
       | mips.w_mem (a, (mask, (sz, v))) =>
          "Address 0x" ^ hex64 a ^ " <- 0x" ^ hex64 v ^
          " [" ^ sizeString sz ^ "], mask " ^ hex64 mask
in
   fun printLog () = List.app (fn e => print (logString e ^ "\n\n")) (!mips.log)
end

(* ------------------------------------------------------------------------
   Run code
   ------------------------------------------------------------------------ *)

fun loop mx i =
   let
      val (h, a) =
         case mips.Fetch () of
            SOME w => (hex32 w, mips.instructionToString (mips.Decode w))
          | NONE => ("-", "unaligned instruction fetch")
      val exl0 = #EXL (#Status (!mips.CP0))
   in
      print (StringCvt.padRight #" " 6 (Int.toString i) ^ " " ^
             hex64 (!mips.PC) ^ " : " ^ h ^ "   " ^ a ^ "\n")
    ; mips.Next ()
    ; if 2 <= !trace_level then printLog () else ()
    ; dumpRegistersOnCP0_26 ()
    ; if mips.done () orelse i = mx
         then print ("Completed " ^ Int.toString i ^ " instructions.\n")
      else loop mx (if not exl0 andalso #EXL (#Status (!mips.CP0))
                       then (print "exception\n"; i)
                    else i + 1)
   end

fun decr i = if i <= 0 then i else i - 1

fun pureLoop mx =
   ( mips.Next ()
   ; dumpRegistersOnCP0_26 ()
   ; if mips.done () orelse mx = 1 then () else pureLoop (decr mx)
   )

local
   fun t f x = if !time_run then Runtime.time f x else f x
in
   fun run_mem mx =
      if 1 <= !trace_level then t (loop mx) 0 else t pureLoop mx
   fun run pc mx code =
      ( mips.initMips pc
      ; List.map
          (fn (a, s) =>
             ( print ("Loading " ^ s ^ "... ")
             ; storeArrayInMem (a, loadIntelHex s))) code
      ; run_mem mx
      )
end

(* ------------------------------------------------------------------------
   Command line interface
   ------------------------------------------------------------------------ *)

fun printUsage () =
   let
      val name = OS.Path.file (CommandLine.name ())
   in
      print
        ("\nMIPS emulator (based on an L3 specification).\n\
          \http://www.cl.cam.ac.uk/~acjf3/l3\n\n\
          \usage: " ^ name ^
         " [--cyles number] [--trace level] [--pc address]\n" ^
         StringCvt.padLeft #" " (String.size name + 8) " " ^
          "[--at address hex_file] ... hex_start_file\n\n" ^
         "       " ^ name ^ " --help\n\n")
   end

fun getNumber s =
   case IntExtra.fromString s of
      SOME n => n
    | NONE => failExit ("Bad number: " ^ s)

fun getArguments () =
   List.map
      (fn "-c" => "--cycles"
        | "-t" => "--trace"
        | "-h" => "--help"
        | "-p" => "--pc"
        | "-a" => "--at"
        | s => s)
      (CommandLine.arguments ())

fun processOption (s: string) =
   let
      fun loop acc =
         fn a :: b :: r =>
            if a = s
               then (SOME b, List.rev acc @ r)
            else loop (a :: acc) (b :: r)
          | r => (NONE, List.rev acc @ r)
   in
      loop []
   end

fun getCode p =
   let
      fun loop acc =
         fn "--at" :: a :: c :: r => loop ((getNumber a, c) :: acc) r
          | [r] => SOME (List.rev ((p, r) :: acc))
          | _ => NONE
   in
      loop []
   end

val () =
   case getArguments () of
      ["--help"] => printUsage ()
    | l =>
       let
          val (p, l) = processOption "--pc" l
          val p = Option.getOpt (Option.map getNumber p, 0x0000000040000000)
          val (c, l) = processOption "--cycles" l
          val c = Option.getOpt (Option.map getNumber c, ~1)
          val (t, l) = processOption "--trace" l
          val t = Option.getOpt (Option.map getNumber t, !trace_level)
          val () = trace_level := Int.max (0, Int.min (t, 2))
       in
          case getCode p l of
             SOME code => run p c code
           | NONE => printUsage ()
       end

(* ------------------------------------------------------------------------ *)

(* testing...

fun test s =
   run 0x40000000 10000 [(0x40000000, "/Users/acjf3/Work/mips/testTraces/" ^ s)]

val () = trace_level := 2
val () = trace_level := 1
val () = trace_level := 0

val () = test "test_addi_overflow_cached.hex";
val () = test "test_raw_b_cached.hex";
val () = test "test_raw_lb_cached.hex";
val () = test "test_raw_sb_cached.hex";
val () = test "test_teq_eq_cached.hex";
val () = test "test_raw_jalr.hex";
val () = test "test_ddiv_cached.hex";
val () = test "test_raw_sdr_cached.hex";

*)
