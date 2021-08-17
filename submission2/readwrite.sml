
fun readfile (infile : string) = let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => line :: loop ins
    | NONE      => []
   val strList =  map (Substring.string o (Substring.trimr 1) o Substring.full)
in
  strList(loop ins) before TextIO.closeIn ins
end

fun writetofile (L:string list,outfile) =
  let
    val outs = TextIO.openOut outfile

    fun loop [] = ()
      | loop (x::xs) = (TextIO.output(outs,x^"\n"); loop xs)
   in
     loop L before TextIO.closeOut outs
   end
