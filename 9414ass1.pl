% Full name:Xingchen Wang
% Student number:5147730
% Assignment 1
%Q1:
neg([], []). %find negative number
neg([Head | Tail], [Head | N]) :-  
    Head <0,
    neg(Tail, N).
neg([Head | Tail], N) :-
    not(Head < 0),
    neg(Tail,N).
sumsq([], 0). %sum function
sumsq([Head | Tail], Sum) :-
    sumsq(Tail, Tail_Sum),
    Sum is Head * Head + Tail_Sum.
sumsq_neg(Numbers, Sum) :-
    neg(Numbers, Neg_List),
    sumsq(Neg_List, Sum).



%Q2:
one_like_all(_,[]).   %deal with one likes all
one_like_all(Person,[AHead|ATail]) :-
    likes(Person,AHead),
    one_like_all(Person,ATail).
all_like_all([],_).
all_like_all([OHead|OTail],[H|T]) :-
    likes(OHead,H),
    one_like_all(OHead,T),
    all_like_all(OTail,[H|T]).



%Q3:
sqrt1(Number,Sqrt) :- %Sqrt
    Sqrt is sqrt(Number).
cons([], L, L). %connect lists
cons([Head|Tail], List, [Head|TailResult]) :-
    cons(Tail, List, TailResult).
sqrt_table(N, M, [Head|[]]) :-
    N =:= M,
    cons([N],[Sqrt],Head),
    sqrt1(N,Sqrt).    
sqrt_table(N, M, [Head|Result]) :-
    N >  M,
    cons([N],[Sqrt],Head),
    sqrt1(N,Sqrt),
    Z is (N-1),
    sqrt_table(Z,M,Result).
    


%Q4:
succe(N,M) :-  %make sure the order
    M is N+1 .
rest([],_).  %get some numbers after increasing number
rest([H1|[H2|T]],Rest) :-
    succe(H1,H2),
    rest([H2|T],Rest).
rest([H1|[H2|[]]],[H2|[]]) :-
    succe(H1,H2),
    rest([],[]).
rest([H1|[H2|T]],[H1|[H2|T]]) :-
    not(succe(H1,H2)),
    rest([],[H1|[H2|T]]).
chop_up([],[]).
chop_up([H1|[H2|T]],[H1|NewList]) :-
    not(succe(H1,H2)),
    chop_up([H2|T],NewList).
chop_up([H],[H|[]]).
chop_up([H1|[H2|T]],[X|NewList]) :-
    succe(H1,H2),
    rest([H1|[H2|T]],[H3|T2]),
    cons([H1],[H3],X),
    chop_up(T2,NewList).    



%Q5:
tree_eval(Num, tree(empty,X,empty), Eval) :- %two base cases
    X=z, Eval is Num.  
tree_eval(_, tree(empty,X,empty), Eval) :- 
    number(X), Eval is X.
tree_eval(Num, tree(L,Op,R), Eval) :-   %four caculation rules
    tree_eval(Num, L, LEval), 
    tree_eval(Num, R, REval),
    Op = '+',
    Eval is LEval + REval.
tree_eval(Num, tree(L,Op,R), Eval) :-
    tree_eval(Num, L, LEval),
    tree_eval(Num, R, REval),
    Op = '-',
    Eval is LEval - REval.
tree_eval(Num, tree(L,Op,R), Eval) :-
    tree_eval(Num, L, LEval),
    tree_eval(Num, R, REval),
    Op = '*',
    Eval is LEval * REval.
tree_eval(Num, tree(L,Op,R), Eval) :-
    tree_eval(Num, L, LEval),
    tree_eval(Num, R, REval),
    Op = '/',
    Eval is LEval / REval.

