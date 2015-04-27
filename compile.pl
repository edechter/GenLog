
:- use_module(library(varnumbers)).
:- use_module(library(gensym)).
:- use_module(library(debug)).



%% ----------------------------------------------------------------------
%% compile SDCL syntax to prolog syntax
%%
%%
compile_sdcl_file(File) :-
        open(File, read, ID),
        retractall(sdcl_rule(_, _, _, _, _, _)),
        reset_gensym, 
        !,
        repeat,
        read_clause(ID, Clause, []),        
        (
         Clause = end_of_file -> 
         close(ID), !
        ;
         Clause = macro(Macro) ->
         expand_macro(Macro, Rules),
         compile_sdcl_clauses(_, Rules),
         fail
        ;
         compile_sdcl_clause(_, Clause),
         fail
        ),
        normalize_rules.

compile_sdcl_clauses(RuleIds, Clauses) :-
        findall(RuleId,
                (member(Clause, Clauses),
                 compile_sdcl_clause(RuleId, Clause)),
                RuleIds).        

compile_sdcl_clause(RuleId, Clause) :-
        (var(RuleId) -> 
         gensym('', RuleNum),
         atom_number(RuleNum, RuleNum1),
         RuleId = r(RuleNum1)
        ;
         ground(RuleId) -> true
        ;
         throw(error(instantiation_error, compile_sdcl_clause/2),
               'RuleId argument must be either a variable or completely ground.')
        ),
         
        tr_sdcl_clause(Clause, TrClause),
        TrClause = sdcl_rule(RuleId, _, _, _, _, _),
        assert(TrClause).

retract_all_rules :-
        retractall(sdcl_rule(_, _, _, _, _, _)).

%% display compiled rules
show_rules :-
        findall(Id-W, 
                sdcl_rule(Id, _, _, W, _, _),
                Assoc),
        keysort(Assoc,AssocSorted),
        !,
        member(Id-W, AssocSorted),
        pprint_rule(Id, RString), 
        format("~|~w: ~t ~10+~w  ~t ~65+:: ~2f\n", [Id, RString, W]),
        fail
        ;
        true.


:- begin_tests(compile).

test(compile_sdcl_clause1,
     [setup(retract_all_rules),
      cleanup(retract_all_rules),
      true(AClause =@= TClause)]) :- 
        Clause = (s(X, Y | [boy], Y)),
        compile_sdcl_clause(r(2), Clause),
        TClause = sdcl_rule(r(2), sdcl_term(s/4, [X1, Y1], [[boy], Y1]),
                     true, 1.0, 1.0, rule_group(s/4, [[boy], '$VAR'(0)])),
        call(TClause),
        AClause = sdcl_rule(_, _, _, _, _, _),
        call(AClause).
:- end_tests(compile).


%% ----------------------------------------------------------------------
%% translate SDCL syntax to structure representation
%%
%% an SDCL term is of the form
%% sdcl_term(Functor/Arity, Vars, Conds
%% tr_sdcl_term(+TermIn, -TermOut) is det.
%% TermIn is a sdcl term, e.g., s(X, Y | Y, G)
%% TermOut is structured representation, e.g. sdcl_term(s/4, [X, Y], [Y, G]).
tr_sdcl_term(TermIn, TermOut) :-
        copy_term(TermIn, TermInCopy), 
        numbervars(TermInCopy),
        tr_numbered_sdcl_term(TermInCopy, TermTmp),
        varnumbers(TermTmp, TermOut),
        term_variables(TermOut, VsOut),
        term_variables(TermIn, VsIn),
        VsIn = VsOut.

:- begin_tests('translate sdcl terms').

test(tr_sdcl_term1,
     [true(TermOut =@= sdcl_term(s/4, [_A, B],  [B, _C]))]
    ) :- 
        TermIn  = s(_X, Y | Y, _Z),
        tr_sdcl_term(TermIn, TermOut).

test(tr_sdcl_term2,
     [true(TermOut =@= sdcl_term(s/4, [[_A], B],  [B, _C]))]
    ) :- 
        TermIn  = s([_X], Y | Y, _Z),
        tr_sdcl_term(TermIn, TermOut).
        
:- end_tests('translate sdcl terms').

%% tr_sdcl_clause(Clause, sdcl_rule(RuleId, RuleHead,
%% RuleBody, RuleWeight, RuleGroup).  RuleId is not bound here, can be bound
%% later to provide unique Id to the rule.
tr_sdcl_clause(Clause, Rule) :-
        \+ var(Clause),
        copy_term(Clause, ClauseCopy), 
        numbervars(ClauseCopy), 
        Rule = sdcl_rule(_, RuleHead, RuleBody, RuleWeight, RuleAlpha, RuleGroup),
        % strip rule weight if present
        (
         ClauseCopy = (Clause1 :: RuleWeight)
        ;
         ClauseCopy = Clause1,
         RuleWeight = 1.0
        ),
        (
         Clause1 = (H ---> B), !
        ;
         Clause1 = H,
         B = true
        ),
        % we always set default alpha value to 1
        RuleAlpha = 1.0,
        and_to_list(B, BList),
        maplist(tr_numbered_sdcl_term, BList, BList1),
        list_to_and(BList1, RuleBody0), 
        tr_numbered_sdcl_term(H, RuleHead0),
        % unnumber the variables NB: both head and body are unnumbered
        % at once so that we preserve the correspondence between variables
        varnumbers((RuleHead0, RuleBody0), (RuleHead, RuleBody)),
        goal_to_rule_group(RuleHead, RuleGroup),
        !.

goal_to_rule_group(sdcl_term(F/A, _, Args), RuleGroup) :-
        copy_term(Args, Args1),
        numbervars(Args1), 
        RuleGroup = rule_group(F/A, Args1).

        
        

:- begin_tests('translate sdcl rules').

test('translate sdcl rule (no weight)',
     [
      true(TermOut =@= sdcl_rule(_, Head, Body, Weight, Alpha, RuleGroup))]
    ) :- 
        RuleIn  = (s(X, Y | Y, Z) ---> a(X | Y), b(Y)),
        tr_sdcl_clause(RuleIn, TermOut),
        Head = sdcl_term(s/4, [X, Y], [Y, Z]),
        Body = (sdcl_term(a/2, [X], [Y]), sdcl_term(b/1, [Y], [])),
        Weight = 1.0,
        Alpha = 1.0, 
        RuleGroup = rule_group(s/4, ['$VAR'(0), '$VAR'(1)]).

test('translate sdcl rule (with weight)',
     [true(TermOut =@= sdcl_rule(_, Head, Body, Weight, Alpha, RuleGroup))]
    ) :- 
        RuleIn  = (s(X, Y | Y, Z) ---> a(X | Y), b(Y) :: 3.2),
        tr_sdcl_clause(RuleIn, TermOut),
        Head = sdcl_term(s/4, [X, Y], [Y, Z]),
        Body = (sdcl_term(a/2, [X], [Y]), sdcl_term(b/1, [Y], [])),
        Weight = 3.2,
        Alpha = 1.0,
        RuleGroup = rule_group(s/4, ['$VAR'(0), '$VAR'(1)]).
                        
:- end_tests('translate sdcl rules').


tr_numbered_sdcl_term(TIn, sdcl_term(Functor/Arity, Vars, Conds)) :-
        functor(TIn, Functor, _),
        TIn =.. [_|Args],
        tr_split_args(Args, Vars, Conds),
        length(Vars, NVars),
        length(Conds, NConds),
        Arity is NVars + NConds,
        !.


%% tr_split_args(In, Vars, Conds) takes a list of terms of the form
%% [A, ..., B, (C|D), E, ...] and separates it in Vars = [A,..,B] and
%% Conds = [D, ..E]. All "variables" should be numbered vars (see
%% numbervars and varnumbers above)
tr_split_args(Args, Vars, Conds) :-
        tr_split_args(Args, Vars, Conds, vars).
tr_split_args([], [], [], _).
tr_split_args([(A|B)|In], [A], [B|In], vars) :- 
        !.
tr_split_args([A|In], [A|Vars], Conds, vars) :-
        tr_split_args(In, Vars, Conds, vars).
tr_split_args([A|In], Vars, [A|Conds], conds) :-
        tr_split_args(In, Vars, Conds, conds).

         
% rule_repr(Rule, RuleRepr) :-
%         copy_term(Rule, RuleRepr), 
%         numbervars(RuleRepr).


%% ----------------------------------------------------------------------
%%     expand_macro(Macro, Rules)
%%
%%     A macro is indicated by a macro/1 term in a SDCL file. The
%%     argument is a prolog clause whose head is a SDCL rule, and
%%     whose body defines a copy of that rule for each binding of the
%%     body variables.


expand_macro( (Head :- Body), Rules) :-
        findall(Head,
                Body,
                Rules).


:- begin_tests(translate).

test_expanded(
              [(s(X | a, Z) ---> a(X | a), b(X | a,Z)),
               (s(X1 | b, Z1) ---> a(X1 | b), b(X1 | b,Z1))
              ]
              ).
              
test(expand_macro,
     [setup(test_expanded(TRules)),
      all(Rule =@= TRules)]) :-
        expand_macro( (
                       (s(X | Y, Z) ---> a(X | Y), b(X | Y,Z))
                       :- 
                       (member(Y, [a, b]))
                      ),
                      Rules),
        member(Rule, Rules).

:- end_tests(translate).

        

        
        
              

         
        