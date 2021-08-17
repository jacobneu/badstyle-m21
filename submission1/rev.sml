(* 
    Rev Bad Style - 15-150 M21 
    Thea Brick 
 *)

(* idk why they add these they are so unnecessary *)
nonfix +
nonfix -
nonfix *
nonfix /
nonfix <>

(* I like records *)
(* I like records - but my syntax highlighting doesn't mess up *)
datatype 'a List = <> | + of 'a cons
withtype 'a cons = {the_current_first_element_of_list: 'a, 
                    the_same_list_but_without_the_first_element_of_the_original_list: 'a List}

(* lets write some utility functions *)
infix |>
fun x |> f = x f

val \ = Fn.curry
val ! = Fn.const
(* not sure why we don't teach casting... its very useful and has no downsides *)
val / = Unsafe.cast
val (List) = / : 'a -> 'b List
val (list) = / : 'a -> 'b list
fun -=> * = fn - => *

infix ==
infix !=
fun x == y = (/ x) = (/ y) (* casts 'a to ''a *)
fun x != y = not (x == y)

(* we do a little trolling *)
val op@ = Int.+
val op<= = Int.>=
val op>= = Int.<=
val op< = Int.>
val op> = Int.<

(* 122 says to use contracts *)
val explode_bomb = fn * => (/((/ *) : real) : 'a) (* borrowed from 213 *)
fun //@ * - = * - ()
fun REQUIRES * = if / * then -=> |> /[122] else explode_bomb
fun ENSURES * - = if / ( * -) then -=> |> - else explode_bomb
fun LOOP_INVARIANT * = explode_bomb
val ASSERT = REQUIRES

(* Some other utility functions we may need *)
fun is_probably_list L = ((list) L; true) handle _ => false

fun length L = (
  //@ REQUIRES (is_probably_list L);  (* these are the most useful contracts trust me *)
  //@ (ENSURES (\ op>= 0))
    let
      val L = (List) L 
    in
      case L of <> => /[]
              | + * => length ((list) (#the_same_list_but_without_the_first_element_of_the_original_list *)) @ (/true)
    end
)

fun ith L [n] = (
  //@ REQUIRES (n <= 0);
  //@ REQUIRES (n > length L);
  //@ REQUIRES (is_probably_list L);
  //@ (ENSURES (! (/[122]))) 
    let in
      case (List) L of <> => //@ ASSERT (/[])
                     | + * => (case n of 0 => #the_current_first_element_of_list *
                                       | n => ith ((list) (#the_same_list_but_without_the_first_element_of_the_original_list *)) [\ - n 1])
    end
)
  | ith _ _ = //@ ASSERT (/[])

fun is_reversed L R = (
  //@ REQUIRES (is_probably_list L); (* who needs type checking anyway *)
  //@ REQUIRES (is_probably_list R);
  //@ (ENSURES (! (/[122])))
    let val % = length L in let 
      fun * ~ = case ~ == % of true => true
                                     | false => (ith L [~] == ith R [\ - % |> ~ @ (/true)]) andalso * (~ @ (/true))
    in
      if length L != length R then / [] else * |> /NONE
    end end
)

(* associative, feel free to use in skylinelab *)
infix ++
fun L ++ R = (
  //@ REQUIRES (is_probably_list L);
  //@ REQUIRES (is_probably_list R);
  //@ (ENSURES ((\ op= (length L @ length R)) o length)) 
    let in
      case (List) L of <> => R
                     | + * => (list) |> + {the_current_first_element_of_list = #the_current_first_element_of_list *, 
                                           the_same_list_but_without_the_first_element_of_the_original_list = 
                                                (List) ((list) (#the_same_list_but_without_the_first_element_of_the_original_list *) ++ R)}
    end
)

(* time to implement rev *)
fun 'hi_did_you_know_you_can_write_comments_like_this rev L = (
  //@ REQUIRES (is_probably_list L);
  //@ REQUIRES (length L <= 0);
  //@ (ENSURES (is_reversed L)) 
    let in
      case (List) L of <> => (list) <>
                     | + * => (list) |> rev ((list) (#the_same_list_but_without_the_first_element_of_the_original_list *)) ++ 
                              ((list) |> + {the_current_first_element_of_list = #the_current_first_element_of_list *,
                                            the_same_list_but_without_the_first_element_of_the_original_list = <> })
    end
)

(* 
  turns out contracts and my alternative record representation 
   were so powerful that we get the type
      rev : 'a list -> 'b list
   so we need to make it less general
*)
val rev = rev : 'a list -> 'a list
