%% simple.pl
%% simple sdcl for English
%% ----------------------------------------------------------------------


s(X, Y) ---> s(X, Z), s(Z, Y) :: 0.000001.
s(X, Y) --->
        num(Number),
        person(Person),
        transitivity(Transitivity), 
        np(X, Z | Number, Person),
        vp(Z, U | Number, Person, Transitivity),
        stop(U, R), 
        s(R, Y).
s(X, X). 

% a recursive rule to test whether we can handle infinite derivations

stop([stop|X], X). 

np(X, Y | Number, third) --->
        pn(X, Y | Number)                       :: 1.
np(X, Y | Number, third) --->
        det(X, Z | Number), n(Z, Y | Number)    :: 1.

np(['I'|Y], Y | singular, first).
np([we|Y], Y  | plural, first).

num(singular) :: 0.7.
num(plural) :: 0.3.

person(first).
person(second).
person(third).

pp(X, Y) ---> prep(X, Z), np(_, Z, Y) :: 1.

pn([eyal|X], X | singular) :: 0.5.

pn([they|X], X | plural) :: 0.5.
pn([they|X], X | singular) :: 0.5.

n(X, Y | Number) --->
        root(_, X, Z), plSuffix(Z, Y | Number) :: 1.

root(book, [book|X], X).
root(car, [car|X], X).
root(cat, [cat|X], X).

plSuffix(X, X | singular).
plSuffix([s| X], X | plural).
        
% n([book|X], X | singular) :: 1.
% n([books|X], X | plural) :: 1.
% n([car|X], X | singular) :: 1.

transitivity(transitive).
transitivity(intransitive).

vp(X, Y | Number, Person, transitive) --->
        verb_root(Root | transitive),
        verb(X, Z | Number, Person, Root),
        np(Z, Y | _, _)                                              :: 0.6.
vp(X, Y | Number, Person, intransitive) --->
        verb_root(Root | intransitive),        
        verb(X, Y | Number, Person, Root)                         :: 0.4.

verb_root(see | transitive).
verb_root(see | intransitive).
verb_root(sing |  transitive).
verb_root(sing |  intransitive).
verb_root(write |  intransitive).

verb([Root,s |Y], Y | singular, third, Root).
verb([Root,s |Y], Y | plural, third, Root).
verb([Root|Y], Y | plural, third, Root).
verb([Root|Y], Y | singular, first, Root).
verb([Root|Y], Y | plural, first, Root).
verb([Root|Y], Y | singular, second, Root).
verb([Root|Y], Y | plural, second, Root).


        
        
        

% v([sees|X], X | singular, transitive).
% v([see|X], X | plural, transitive).

% v([sings|X], X | singular, intransitive, third).
% v([sing|X], X | plural, intransitive, ).

det([the|X], X | singular) :: 1.
det([a|X], X | singular)   :: 1.
det([the|X], X | plural)   :: 1.
det(X, X | plural)   :: 1.
