%% ----------------------------------------------------------------------
%%
%% assertion.pl
%% author: Eyal Dechter
%%
%%
%% ----------------------------------------------------------------------

:- dynamic assertions/1.
assertions(on).

%% asserting(Goal)
asserting(Goal) :-
        call(Goal), !.
asserting(Goal) :-
        throw(error(assertion_error, context(asserting/1, assertion_error(Goal)))).
        