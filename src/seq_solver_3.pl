%===================File seq_solver_3.pl========================================
%===================Author : Nikolai Kosmatov (LIFC, Besancon)==================
%===============================================================================
%Description : CHR for the resolution of constraints for sequences.
%Propagation for first-last elements, front-tail, size, rev, ins_last/first,
%begin/endseq, concat

%Inspired by:
%% 931129 ECRC, 980312 LMU thom fruehwirth
%% 961106 Christian Holzbaur, SICStus mods

:- module(chr_list, [
                     concat/3,
                     eqlist/2,
                     lenlist/2
                    ]).

:- use_module(library(clpfd)).
:- use_module( library(chr)).
:- use_module( library(dialect/sicstus)).

:- chr_option(check_guard_bindings,on).

% handler list.
%option(debug_compile,on)

:- chr_constraint eqlist/2.
:- chr_constraint lenlist/2.
:- chr_constraint leneqlist/2.

:- op(700,xfx,eqlist).
:- op(700,xfx,lenlist).
:- op(700,xfx,leneqlist).

%===============================================================================
%====================Definition of operations===================================
%===============================================================================

:- op( 500, xfy, first ).
:- op( 500, xfy, last ).
:- op( 500, xfy, front ).
:- op( 500, xfy, tail ).
:- op( 500, xfy, size ).
:- op( 500, xfy, rev ).
:- op( 500, xfy, concat ).

:- chr_constraint
    first/2.       %Seq first El
:- chr_constraint
    last/2.       %Seq last El
%element of Seq number Num is El ( Num >=1 ).
:- chr_constraint
    seq_elt/3.     %seq_elt( Seq, Num, El ),
:- chr_constraint
    front/2.       %Seq front FrontSeq
:- chr_constraint
    tail/2.        %Seq tail TailSeq
:- chr_constraint
    ins_first/3. %ins_first( ResultSeq, TailSeq, FirstEl )
:- chr_constraint
    ins_last/3. %ins_last( ResultSeq, FrontSeq, LastEl )
:- chr_constraint
    size/2.        %size( Seq, Size )
:- chr_constraint
    rev/2.         %rev( RevSeq, Seq )
:- chr_constraint
    concat/3.      %concat( ResultSeq, SubSeq1, SubSeq2 )
%beginning subsequence : Subseq is the subsequence with Num first elts of Seq
:- chr_constraint
    begseq/3.      %begseq( Seq, Num, SubSeq )
%end subsequence : Subseq is the subsequence of Seq from position Num+1
:- chr_constraint
    endseq/3.      %endseq( Seq, Num, SubSeq )
:- chr_constraint
    seq_elts/2.     %seq_elts( Seq. [[Num1,El1],[Num2,El2],[Num3,El3]...] )


%--------------------first------------------------------------------------------
r01 @
Seq first El
    <=> [ [El], List1 ] eqlist Seq.
%--------------------last-------------------------------------------------------
r02 @
Seq last El
    <=> [ List1, [El] ] eqlist Seq.
%--------------------size-------------------------------------------------------
r03 @
Seq size X
    <=> Seq lenlist X.
%--------------------front------------------------------------------------------
r04 @
Seq front SubSeq
    <=> [ SubSeq, [ El ] ] eqlist Seq.
%--------------------tail-------------------------------------------------------
r05 @
Seq tail SubSeq
    <=> [ [ El ], SubSeq ] eqlist Seq.
%--------------------concat-----------------------------------------------------
r06 @
concat( ResultSeq, SubSeq1, SubSeq2 )
    <=> [ SubSeq1, SubSeq2 ] eqlist ResultSeq.
%--------------------begseq-----------------------------------------------------
r07 @
begseq( Seq, Num, SubSeq )
    <=> [ SubSeq, Seq2 ] eqlist Seq, SubSeq lenlist Num.
%--------------------endseq-----------------------------------------------------
r08 @
endseq( Seq, Num, SubSeq )
    <=> [ Seq2, SubSeq ] eqlist Seq, Seq2 lenlist Num.
%--------------------ins_first--------------------------------------------------
r09 @
ins_first( ResultSeq, TailSeq, FirstEl )
    <=> ResultSeq tail TailSeq, ResultSeq first FirstEl.
%--------------------ins_last---------------------------------------------------
r10 @
ins_last( ResultSeq, FrontSeq, LastEl )
    <=> ResultSeq front FrontSeq, ResultSeq last LastEl.
%--------------------rev--------------------------------------------------------
r11 @
rev( R, S )
    <=> reverse(R,S).
%-------------------------------------------------------------------------------
%% Rs eqlist L: Rs is a list of lists, whose concatentation is the single list L

r12 @
[] eqlist L <=> L=[].
r13 @
[R] eqlist L <=> R=L.
r14 @
[R|Rs] eqlist [] <=> R=[], Rs eqlist [].
r15 @
[[X|R]|Rs] eqlist L <=> L=[X|L1], [R|Rs] eqlist L1.
r16 @
Rs eqlist L <=> delete(R,Rs,Rs1),R==[] | Rs1 eqlist L.
r17 @
Rs eqlist L <=> delete(R,Rs,Rs1),R==L | Rs1 eqlist [].

R eqlist L ==> R leneqlist L.

[] leneqlist L <=> L=[].

[R|Rs] leneqlist L <=> R lenlist NR, Rs leneqlist L1, L lenlist NL,
  L1 lenlist NL1, NL #= NR + NL1.


:- chr_constraint labeling/0.
r18 @
labeling, ([R|Rs] eqlist L)#Ph <=> true |
( var(L) -> lngth(L,_) ; true),
(
  R=[], Rs eqlist L
;
  L=[X|L1], R=[X|R1], [R1|Rs] eqlist L1
),
labeling
pragma passive(Ph).


%% L lenlist N: The length of the list L is N
%% N can be an arithmetic expression

r19 @
[]    lenlist N <=> true | (var(N) -> N=0 ; N=:=0).
r20 @
[_|L] lenlist N <=> positive(N), pls(M,1,N),
%N #= M + 1,
L lenlist M.
r21 @
L     lenlist N <=> ground(N) | lngth(L,N).

( X lenlist N1 ) #Id \ X lenlist N2 <=> N1=N2 pragma passive(Id).


%% auxiliary predicates ---------------------------------------------------
genalternate1(R,0) :-
    R=[].
genalternate1(R,N) :-
    N1 is N - 1,
    ( N mod 2 =:= 0 ->
        genalternate1(FR,N1),
        R last N,
        R front FR
    ;
        genalternate1(TR,N1),
        R first N,
        R tail TR
    ).


genalternate(R,0) :-
    R=[].
genalternate(R,N) :-
    N1 is N - 1,
    ( N mod 2 =:= 0 ->
        R last N,
        R front FR,
        genalternate(FR,N1)
    ;
        R first N,
        R tail TR,
        genalternate(TR,N1)
    ).
gentail1(R,0) :-
    R=[].
gentail1(R,N) :-
    N1 is N - 1,
    gentail1(TR,N1),
    R first N,
    R tail TR.
genfront1(R,0) :-
    R=[].
genfront1(R,N) :-
    N1 is N - 1,
    genfront1(TR,N1),
    R last N,
    R front TR.
gentail(R,0) :-
    R=[].
gentail(R,N) :-
    R first N,
    R tail TR,
    N1 is N - 1,
    gentail(TR,N1).
genfront(R,0) :-
    R=[].
genfront(R,N) :-
    R last N,
    R front FR,
    N1 is N - 1,
    genfront(FR,N1).
cputime(Ts) :-
    statistics( runtime, [Tm,_]),
    Ts is Tm/1000.
test(Y) :-
    cputime(X),
    Y,
    cputime( Now),
    Time is Now-X,
    write(N-Time),
    nl.

delete( X, [X|L],  L).
delete( Y, [X|Xs], [X|Xt]) :-
	delete( Y, Xs, Xt).



lngth([],0).
lngth([_|L],N1):- lngth(L,N), N1 #= N+1.

% :- block pls(-,-,?), pls(-,?,-), pls(?,-,-).

pls(X, Y, Z) :- when((nonvar(X), nonvar(Y)
                     ;
                      nonvar(X), nonvar(Z)
                     ;
                      nonvar(Y), nonvar(Z)),
                     pls1(X, Y, Z)).
								%
pls1( A, B, C) :- var(C), !, C #= A+B.
pls1( A, B, C) :- var(B), !, B #= C-A.
pls1( A, B, C) :- var(A), !, A #= C-B.
pls1( A, B, C) :- C #= A+B.

:- block positive(-).


positive( X) :- X>0.


reverse([],[]).
reverse(R,L):-
	R lenlist N,
	L lenlist N,
	X lenlist 1,
	[X,R1] eqlist R,
	[L1,X] eqlist L,
	reverse(R1,L1).


%% Done by thom

permute([],[]).
permute(R,L):-
	R lenlist N,
	L lenlist N,
	X lenlist 1,
	[X,R1] eqlist R,
	[A,X,B] eqlist L,
	[A,B] eqlist L1,
	permute(R1,L1).

