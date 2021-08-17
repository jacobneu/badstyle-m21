local
  infix |>
  fun x |> f = f x

  fun rever [] = []
    | rever [A] = [A]
    | rever [A,B] = [B,A]
    | rever [A,B,C] = [C,B,A]
    | rever [A,B,C,D] = [D,C,B,A]
    | rever [A,B,C,D,E] = [E,D,C,B,A]
    | rever [A,B,C,D,E,F] = [F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G] = [G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H] = [H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I] = [I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J] = [J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K] = [K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L] = [L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M] = [M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N] = [N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = [O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = [P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = [Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = [R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = [S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = [T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] = [U,T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V] = [V,U,T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W] = [W,V,U,T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X] = [X,W,V,U,T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y] = [Y,X,W,V,U,T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]
    | rever [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z] = [Z,Y,X,W,V,U,T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A]

  fun chunk n L = if length L > n 
                    then List.take (L,n) :: chunk n (List.drop (L,n)) 
                    else [L]
                    
  val longest = 26
in
  fun rev L = case length L <= longest of
                true  => rever L
              | false =>
                  let
                    val l = length L
                    val n = if l mod longest = 0 
                              then l div longest 
                              else l div longest + 1
                  in
                    L |> chunk n
                      |> map rev
                      |> rever
                      |> List.concat
                  end
end
