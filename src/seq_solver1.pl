%===================File solver1.pl=============================================
%===================Author : Nikolai Kosmatov===================================
%===============================================================================
%Description : CHR for the resolution of constraints for sequences.
%Propagation for first-last elements, front-tail, size, rev, ins_last/first,
%begin/endseq, concat

%Inspired by:
%% 931129 ECRC, 980312 LMU thom fruehwirth
%% 961106 Christian Holzbaur, SICStus mods

:- module(seq_solver1,
          [conc/2,        %ListOfSeqs conc ResultSeq
           first/2,       %Seq first El
           last/2,        %Seq last El
           front/2,       %Seq front FrontSeq
           tail/2,        %Seq tail TailSeq
           ins_first/3,   %ins_first( ResultSeq, TailSeq, FirstEl )
           ins_last/3,    %ins_last( ResultSeq, FrontSeq, LastEl )
           size/2,        %size( Seq, Size )
           rev/2,         %rev( RevSeq, Seq )
           concat/3,      %concat( ResultSeq, SubSeq1, SubSeq2 )
                                %beginning subsequence : Subseq is the subsequence with Num first elts of Seq
           begseq/3,            %begseq( Seq, Num, SubSeq )
                                %end subsequence : Subseq is the subsequence of Seq from position Num+1
           endseq/3,            %endseq( Seq, Num, SubSeq )
           labeling/0, %to put once at the end of the list, if necessary
           op( 700, xfx, conc),
           op( 700, xfy, first ),
           op( 700, xfy, last ),
           op( 700, xfy, front ),
           op( 700, xfy, tail ),
           op( 700, xfy, size ),
           op( 700, xfy, rev ),
           op( 700, xfy, concat )
           
           ]).

:- use_module(library(clpfd)).
:- use_module(library(chr)).

:- chr_option(debug,on).
% :- chr_option(optimize,full).
% handler sequences.
%option(debug_compile,on).

%===============================================================================
%====================Definition of operations===================================
%===============================================================================

:- chr_constraint
    conc/2,        %ListOfSeqs conc ResultSeq
    first/2,       %Seq first El
    last/2,        %Seq last El
    front/2,       %Seq front FrontSeq
    tail/2,        %Seq tail TailSeq
    ins_first/3,   %ins_first( ResultSeq, TailSeq, FirstEl )
    ins_last/3,    %ins_last( ResultSeq, FrontSeq, LastEl )
    size/2,        %size( Seq, Size )
    rev/2,         %rev( RevSeq, Seq )
    concat/3,      %concat( ResultSeq, SubSeq1, SubSeq2 )
%beginning subsequence : Subseq is the subsequence with Num first elts of Seq
    begseq/3,      %begseq( Seq, Num, SubSeq )
%end subsequence : Subseq is the subsequence of Seq from position Num+1
    endseq/3,      %endseq( Seq, Num, SubSeq )
    labeling/0     %to put once at the end of the list, if necessary 
.

:- op( 700, xfx, conc).
:- op( 700, xfy, first ).
:- op( 700, xfy, last ).
:- op( 700, xfy, front ).
:- op( 700, xfy, tail ).
:- op( 700, xfy, size ).
:- op( 700, xfy, rev ).
:- op( 700, xfy, concat ).

%--------------------first------------------------------------------------------
Seq first El
    <=> [ [El], List1 ] conc Seq.
%--------------------last-------------------------------------------------------
Seq last El
    <=> [ List1, [El] ] conc Seq.
%--------------------front------------------------------------------------------
Seq front SubSeq
    <=> [ SubSeq, [ El ] ] conc Seq.
%--------------------tail-------------------------------------------------------
Seq tail SubSeq
    <=> [ [ El ], SubSeq ] conc Seq.
%--------------------concat-----------------------------------------------------
concat( ResultSeq, SubSeq1, SubSeq2 )
    <=> [ SubSeq1, SubSeq2 ] conc ResultSeq.
%--------------------begseq-----------------------------------------------------
begseq( Seq, Num, SubSeq )
    <=> [ SubSeq, Seq2 ] conc Seq, SubSeq size Num.
%--------------------endseq-----------------------------------------------------
endseq( Seq, Num, SubSeq )
    <=> [ Seq2, SubSeq ] conc Seq, Seq2 size Num.
%--------------------ins_first--------------------------------------------------
ins_first( ResultSeq, TailSeq, FirstEl )
    <=> ResultSeq tail TailSeq, ResultSeq first FirstEl.
%--------------------ins_last---------------------------------------------------
ins_last( ResultSeq, FrontSeq, LastEl )
    <=> ResultSeq front FrontSeq, ResultSeq last LastEl.
%--------------------rev--------------------------------------------------------
rev( R, S ) 
    <=> reverse(R,S).
%--------------------conc-------------------------------------------------------
%% Rs conc L: Rs is a list of lists, whose concatentation is the single list L

[] conc L                    <=>  L=[].
[R] conc L                   <=>  R=L.
[R|Rs] conc []               <=>  R=[], Rs conc [].
[[X|R]|Rs] conc L            <=>  L=[X|L1], [R|Rs] conc L1.
Rs conc L                    <=>  delete([],Rs,Rs1) | Rs1 conc L.
Rs conc L                    <=>  delete(L,Rs,Rs1) | Rs1 conc [].
R conc L                     ==>  lenPropagate(R,L).

%--------------------size-------------------------------------------------------
%% R size N: R is a list of size N, N can be an arithmetic expression

[] size N                    <=>  N#=0.
[_|L] size N                 <=>  N#=M+1, L size M.
L size N                     <=>  ground(N) | N1 is N, length(L,N1).
(X size N1)#Id \ X size N2   <=>  N1=N2 pragma passive(Id).

%--------------------labeling---------------------------------------------------

labeling, ([R|Rs] conc L)#Id <=>  true |
    ( var(L) -> length(L,_) ; true),
    ( R=[], Rs conc L ; L=[X|L1], R=[X|R1], [R1|Rs] conc L1 ),
    labeling pragma passive(Id).
		 
%--------------prolog predicates------------------------------------------------		 

reverse([],[]).
reverse(R,L):- R size N, L size N, X size 1,
     [X,R1] conc R, [L1,X] conc L, reverse(R1,L1).

lenPropagate([], []).
lenPropagate([R|Rs],L) :- R size NR, L size NL, L1 size NL1,
     NL #= NR + NL1, lenPropagate(Rs,L1).

delete( X, [X|L],  L).
delete( Y, [X|Xs], [X|Xt]) :- delete( Y, Xs, Xt).

%---------auxiliary predicates--------------------------------------------------

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



