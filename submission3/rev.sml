(* The "real" program begins on line 57. *)
(* Note: unhandled exceptions might be raised in child processes even though
 * the correct result is computed. These may appear even after execution of rev
 * has ostensibly completed. Also, the REPL may display a prompt even when execution
 * is still ongoing. All of this is (obviously) perfectly normal behavior.
 *)

(* High-quality PRNG = PID Random Number Generator *)
structure input = struct fun length () = let val
  t = Posix.Process.fork () val
  p = valOf t in
  SysWord.toInt (Posix.Process.pidToWord p) before (Posix.Process.kill (Posix.Process.K_PROC p, Posix.Signal.kill) before ignore (Posix.Process.waitpid (Posix.Process.W_CHILD p, []))) end end
val In = input.length ()
(* A very meaningful value *)
val place = foldl (fn (x, y) => foldr op+ 0 (map Char.ord (String.explode x)) + Real.floor (Real.fromInt y * 1.5150)) 0 (SMLofNJ.getAllArgs ())
val mutation = 2
fun ds 0 = 0 | ds n = n mod 10 + ds (abs (n div 10))
fun r n = let val r = Random.rand (241, 251) (* if we really wanted randomness, we'd use Posix.Process.fork -- obviously *)
              fun s v [] = false | s v (x::xs) = v=x orelse s v xs
              val x = ref []
              fun f () = let val r = Random.randRange (0, n) r in if s r (!x) then f () else (x := r:: !x; r) end
              in f end
fun only i j = let val r = r (ds i div j - 1) in List.tabulate (ds i div j, (fn x => (ref (r (), (fn () => raise Div))))) end
fun // n m g f k v r = r ((Unsafe.cast k) (f o ref, g n m))
val void = Fn.uncurry map
fun (k * v) x = let
    val x = ref x
    (* Guess and check = optimal efficiency. 
     * Functions are values, so we can avoid pesky nuisances like well-typedness.
     * Also, everybody knows that reversing a list of length n should involve O(n) process forks. *)
    val _ = while List.length (!x) < List.length (Unsafe.cast v) do x := !x @ map (fn y => (y := ((#1 (!y)) + List.length (!x), (#2 (!y))); y)) (only (input.length ()) mutation)
    val x = !x
  in
    ((Unsafe.cast k) ((fn z => z := (#1 (!z), (fn () => List.nth (Unsafe.cast v, #1 (!z))))), x); x)
  end
datatype t = datatype Time.time
val new = TIME {usec=15312} (* much nicer than using the provided interface *)
structure UnsafeMutableRawPointer = struct fun allocate _ = (Posix.Process.sleep o Time.fromMilliseconds) 122 end (* allocating memory takes time! *)
infix var
fun v var b = if b then raise Fail "Principles of Imperative Programming Languages?" else v
val UnsafeMutableRawPointer = Subscript
fun sizeof x = (fn _ => raise x)
infix for
fun x for f = f x
(* naturally, we need to be able to sort a list to reverse it *)
fun is c [] = [] | is c (x::xs) = let fun i (y, []) = [y] | i (y, z::zs) = (case c (y, z) of GREATER => z::i (y, zs) | _ => y::z::zs) in i (x, is c xs) end
val i = 15150
fun int a = if true then 1 else if false then 0 else int a
val op / = op div
fun \ (a,b) = a
fun \\ (a,b) = b
fun ++ z a b = a (is (fn (q,r) => Int.compare (\(!r), \(!q))) b)
fun require or (f,u,n) x = map (fn x => \\(!x) ()) x handle _ => require or (f,u,n) (tl x)
fun rev (input : 'a list) : 'a list =

(* As we learned in 122, comments are vital to the correctness of a program. *)
// (In-place) mutation only!
void rev (void *input)
  var new = UnsafeMutableRawPointer.allocate(sizeof(UnsafeMutableRawPointer) * input.length())
  for (int i = 0; i <= input.length() / 2; ++i) (
    require("swap")(new, i, input.length() - i - 1)
  )
 
(* With options as great as C, Swift, and Java(Script), you can't settle for just one syntax... *)
