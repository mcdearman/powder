-- Haskell 2010 Report 2.2: lexemes follow the maximal munch rule; whitespace is only needed to separate lexemes that would merge
module Main (main) where
import Data.Char(toUpper)
data T=A|B Int|C{fld::Int}deriving(Show,Eq)
f::Int->Int
f x=x+1
g::[Int]->[Int]
g xs=[x*2|x<-xs,x>1]
h::Maybe Int->Int
h m=case m of{Just n->n;Nothing->0}
k::Int->Int
k=(\x->x*x)
l::Int
l=let{a=1;b=2}in a+b
p::[Int]->Int
p all'@(y:_)=y+length all'
p[]=0
q::Int->Bool
q n|n<0=False|otherwise=True
main::IO()
main=do{print("f",f 1,f(f 1))
;print("g",g[1,2,3])
;print("h",h(Just 5),h Nothing)
;print("k",k 4)
;print("l",l)
;print("p",p[10,20],p[])
;print("q",q(-1),q 1)
;print("data",A,B 1,C{fld=2},fld(C 3))
;print("range",[1..3],['a'..'c'],[1,3..9])
;print("ops",1+2*3-4,2^3^2,7`div`2,7`mod`2)
;print("cmp",1<2,2>=2,1/=2,1==1&&2==2||False)
;print("lambda",(\a b->a-b)5 3,map(\c->toUpper c)"ab")
;print("if",if 1<2then"y"else"n")
;print("neg",-1,(-1)+2,3-1,3-(-1))
;print("compose",(f.f.f)0,(f.k)3)
;print("tuple",(1,'a',"s"),fst(1,2),[(x,y)|x<-[1,2],y<-"ab"])
;print("cons",1:2:[],'a':"bc",[1]++[2])
;print("sig",(3::Int),(2::Double))
;print("unit",(),[()])
}
