%% kinship.pl
%% sdcl for kinship relations
%% ----------------------------------------------------------------------
%%
%% Why do you hear Brenda is Susanna's mother? Because we are talking
%% about Susanna. And because you want to communicate to me the
%% property of grandmother. And because Susanna's mother is Amy and
%% her grandmother is Brenda.

subject(amy).
subject(susanna).
subject(eyal).
subject(brenda).
subject(avi).


query(grandmother).
query(mother).
query(parent).
query(gender).

answer(grandmother-[X, Y]     | grandmother, X) --->
        answer(mother-[X, Z]  | mother, X),
        answer(mother-[Z, Y]  | mother, Z).

answer(mother-[X, Y] | mother, X) --->
        answer(parent-[X, Y]      | parent, X),
        answer(gender-[X, female] | gender, X). 

% answer(mother-[susanna, amy] | mother, susanna).
% answer(mother-[amy, brenda]  | mother, amy).

fact(parent-[amy, susanna]).
fact(parent-[brenda, amy]).
fact(parent-[avi, eyal]).

fact(gender-[avi, male]).
fact(gender-[eyal, male]).
fact(gender-[susanna, female]).
fact(gender-[brenda, female]).
fact(gender-[amy, female]).

rule(mother-[X, Y], [parent-[X, Y], gender-[X, female]]).



        




answer(P-[X|Ys] | P, X) ---> fact(P-[X|Ys]).

hear(Property-[X, Y]) --->
        query(Property),
        subject(X),
        answer(Property-[X, Y] | Property, X). 

% answer([X, 's', grandmother, is, Y | Z], Z | grandmother, X) --->
%         answer([X, 's', mother, is, U | T], T | mother, X),
%         answer([U, 's', mother, is, V | R], R | mother, U).


parent(amy    | susanna ).
parent(eyal   | susanna ).
parent(brenda | amy     ).
parent(avi    | eyal    ).


gender(male   | avi     ).
gender(female | brenda  ).
gender(male   | eyal    ).
gender(female | amy     ).
gender(female | suanna  ).

mother(X | Y) --->
        gender(female | X),
        parent(X | Y).

is_mother(X, Y) --->
        person(Y),
        mother(X | Y).

person(amy).
person(eyal).
person(brenda).
person(avi).
person(susanna). 
