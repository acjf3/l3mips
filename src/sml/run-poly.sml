(* Poly/ML version of l3mips *)

val L3_LIB_PATH =
  let
    val tmp = OS.FileSys.tmpName ()
  in
    if OS.Process.isSuccess (OS.Process.system ("l3 --lib-path > " ^ tmp))
       then let
              val strm = TextIO.openIn tmp
              val s = TextIO.inputAll strm
                      before (TextIO.closeIn strm; OS.FileSys.remove tmp)
            in
              OS.Path.fromString (String.substring (s, 0, String.size s - 1))
            end
    else raise Fail "Failed to get L3_LIB_PATH"
  end

local
   fun path {vol, isAbs, arcs} s e =
     let
       val f = OS.Path.joinBaseExt {base = s, ext = SOME e}
       val s = OS.Path.toString {vol = vol, isAbs = isAbs, arcs = arcs @ [f]}
     in
       if OS.FileSys.access (s, [OS.FileSys.A_READ]) then
        (* print ("Using: " ^ s ^ "\n"); *) PolyML.use s
       else () (* print ("Not using: " ^ s ^ "\n") *)
     end
in
   fun useSigSml a s = List.app (path a s) ["sig", "sml"]
end

structure Real64 = LargeReal
structure PackReal64Little = PackRealLittle;

val () =
  List.app (useSigSml L3_LIB_PATH)
    ["IntExtra", "Nat", "Set", "L3", "Bitstring", "BitsN", "Ptree",
     "MutableMap", "Runtime", "FP", "FP64", "NO_FP"];

structure FP32 = NO_FP;

val () =
  ( useSigSml L3_LIB_PATH "FPConvert"
  ; List.app (useSigSml (OS.Path.fromString OS.Path.currentArc)) ["mips", "run"]
  )
