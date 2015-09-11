
:- module(number_words,
          [number_word/2]).


:- use_module(library(settings)).

:- setting(split_teens, boolean, false , 'If true, 13 is thir teen.').
:- setting(irregular_decade, boolean, true , 'If true, 20 is two ty, else twen ty.').
:- setting(split_decade, boolean, true , 'If true, 20 is twenty, else twen ty.').


:- discontiguous number_word/2.

number_word(T, N, W, english) :- number_word(T, N, W).

number_word(1, [one]) :- !.
number_word(2, [two]) :- !.
number_word(3, [three]) :- !.
number_word(4, [four]) :- !.
number_word(5, [five]) :- !.
number_word(6, [six]) :- !.
number_word(7, [seven]) :- !.
number_word(8, [eight]) :- !.
number_word(9, [nine]) :- !.
number_word(10, [ten]) :- !.
number_word(11, [eleven]) :- !.
number_word(12, [twelve]) :- !.

number_word(Teen, Words) :-
        between(13, 19, Teen),
        !,
        N is Teen - 10, 
        number_word(N, [W1]),
        Ws = [W1, teen],
        (setting(split_teens, false) ->
         atomic_list_concat(Ws, X),
         Words = [X]
        ;
         Words = Ws
        ).        

decade_word(2, [X, ty]) :-
        !, 
        (setting(irregular_decade, true) ->
         X = twen
        ;
         X = two
        ).

decade_word(3, [X, ty]) :-
        !, 
        (setting(irregular_decade, true) ->
         X = thir
        ;
         X = three
        ).

decade_word(5, [X, ty]) :-
        !, 
        (setting(irregular_decade, true) ->
         X = fif
        ;
         X = five
        ).

decade_word(N, [X, ty]) :-
        between(4, 9, N),
        !, 
        number_word(N, [X]).

number_word(N, Ws) :-
        between(20, 99, N),
        !,
        D is N div 10,
        R is N rem 10,
        decade_word(D, DecadeWord0),
        (setting(split_decade, true) ->
         DecadeWord = DecadeWord0
        ;
         atomic_list_concat(DecadeWord0, DecadeWord1),
         DecadeWord = [DecadeWord1]
        ),
        (R = 0 -> UnitWord = []
        ;
         number_word(R, UnitWord)
         ),
        append(DecadeWord, UnitWord, Ws).

number_word(N, Ws) :-
        between(100, 999, N),
        !,
        H is N div 100,
        R is N rem 100,
        number_word(H, HundredWord),
        number_word(R, RestWord),
        append([HundredWord, [hundred], RestWord], Ws).
        
