fun rev L =
  let

    fun len [] = 0
      | len (_::xs) = 1 + (len xs)

    fun varstring n i = case (Int.compare(n,i+1)) of
                          LESS => ""
                        | EQUAL => "x" ^ (Int.toString i)
                        | GREATER => "x" ^ (Int.toString i)
                                    ^ "," ^ (varstring n (i+1))
    fun varlist n = "[" ^ (varstring n 0) ^ "]"

    (* Lord forgive me *)
    fun revstring' 0 = "(raise Return [])"
      | revstring' n = "(" ^ (revstring' (n-1)) 
                       ^ " handle (Return res) => "
                       ^ "raise Return ("
                       ^"x" ^ (Int.toString (n-1))
                       ^ "::res))"
    fun revstring n =
       "let exception Return of 'a list in "
      ^ (revstring' n)
      ^ " handle (Return res) => res end"

    (* Check if the lookup table knows how to reverse a list of length |L|.
     * If not, insert it into the lookup table
     *)
    val _ = ignore(prerev L) 
      handle Match => (print ("Adding length " ^ (Int.toString (len L))
                              ^ " to memo\n");
                       let
                         val memo = readfile "memo.sml"
                         val memo' = memo @ ["| prerev " 
                                             ^ (varlist (len L))
                                             ^ " = "
                                             ^ (revstring (len L))]
                       in
                         writetofile(memo',"memo.sml")
                         before (use "memo.sml"; use "rev.sml")
                       end)

     (* AFAIK, the `use` function only dumps stuff into the global environment.
      * So, while the version of `prerev` bound globally will have the addition
      * we just made, and that'll get used for future calls, it isn't currently
      * in scope and IDK how to get it into scope. So just this once, we'll
      * actually do the quadratic reverse lol
      *)
     fun backuprev [] = []
       | backuprev (x::xs) = (backuprev xs)@[x]
  in
     prerev L handle Match => (print "Reverting to backup...\n";
                                backuprev L)
     
  end
