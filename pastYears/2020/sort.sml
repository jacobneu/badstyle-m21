val sort = let
val(w,j,k)=(160,128,32)val v=TextIO.inputLine fun u T=case Time.compare(Time.now
(),T)of LESS=>u T|_=>()val O=Real.toString fun q p=let val T=TextIO.openIn p fun 
r(a,S)=case v S of NONE=>a|SOME l=>r(a^l,S)in r("",T)end val E=String.size fun g 
x c=List.filter(fn y=>(Real.compare(y,x))=c)fun c 0= #" "|c 6= #"@"|c 1= #","|c 
5= #"g"|c 2= #"("|c 4= #"#"|c 3= #"S"|c _= #"!"fun d x=(c(x div k),(x mod k)+1)
val P=List.map fun f D=let fun r(c,0)=""|r(c,i)=(String.str c)^(r(c,i-1))
fun N s=if E s<=w then s else String.substring(s,0,w)^"\n"^(N(String.extract(s,w
,NONE)))val C=P r D val F=String.concat C in N F end val X=String.explode fun Z 
S=let val i=case v S of NONE=>0|SOME N=>let val hB::lB::_=P Char.ord(X N)in j*(
hB-j)+(lB-j)end fun r(a,T,L)=if L>=0 then case v T of NONE=>a|SOME l=>r(a^l,T,L-
(E l))else a in r("",S,i)end fun m(p,n)=let fun M([],[])=[]|M(p::ps,n::ns)=if n=
#"!"then p::M(ps,ns)else n::M(ps,ns)in String.implode (M(X p,X n))end fun s[]=[]
|s[x]=[O x]|s(x::xs)=let val L=g x LESS(x::xs) val m=g x EQUAL(x::xs)val R=g x
GREATER(x::xs)in(s L)@(P O m)@(s R)end fun A[]=[]|A[x]=[]|A(x::y::xs)=let val M=
m(x,y)in M::A(M::xs)end fun R N=f(P(d o Char.ord)(X N))fun H p=let val T=
TextIO.openIn p fun h S=case Z S of ""=>[]|F=>F::(h S)in P R(h T)end fun play()=
let val F=H"Toxic.txt"val f::fs=F val t=f::A(F)fun d(x::xs,T)=let val n=Time.+(T
,Time.fromMilliseconds(1000 div 30))val()=print(x^"\n\n")val()=u n in d(xs,n)end
|d _=()in d(t,(Time.now()))end in(fn L=>let val()=play() in s L end)end