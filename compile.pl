
:- use_module(library(varnumbers)).
:- use_module(library(gensym)).
:- use_module(library(debug)).



%% ----------------------------------------------------------------------
%% compile SDCL syntax to prolog syntax
%%
%%
compile_sdcl_file(File) :-
        open(File, read, ID),
        retractall(sdcl_rule(_, _, _, _, _)),
        reset_gensym, 
        !,
        repeat,
        read_clause(ID, Clause, []),        
        (
         Clause = end_of_file -> 
         close(ID), !
        ;
         gensym('', RuleNum),
         atom_number(RuleNum, RuleNum1),
         compile_sdcl_clause(r(RuleNum1), Clause),
         fail
        ),
        normalize_rules.


compile_sdcl_clause(RuleId, Clause) :-
        tr_sdcl_clause(Clause, TrClause),
        TrClause = sdcl_rule(RuleId, _, _, _, _),
        assert(TrClause).

%% display compiled rules
show_rules :-
        findall(Id-W, 
                sdcl_rule(Id, _, _, W, _),
                Assoc),
        keysort(Assoc,AssocSorted),
        !,
        member(Id-W, AssocSorted),
        pprint_rule(Id, RString), 
        format("~|~w: ~t ~10+~w  ~t ~65+:: ~2f\n", [Id, RString, W]),
        fail
        ;
        true.

%% ----------------------------------------------------------------------
%% translate SDCL syntax to structure representation
%%
%% an SDCL term is of the form
%% sdcl_term(Functor/Arity, Vars, Conds)

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
        Rule = sdcl_rule(_, RuleHead, RuleBody, RuleWeight, RuleGroup),
        % strip rule weight if present
        (
         ClauseCopy = (Clause1 :: RuleWeight)
        ;
         ClauseCopy = Clause1,
         RuleWeight = 1
        ),
        (
         Clause1 = (H ---> B), !
        ;
         Clause1 = H,
         B = true
        ),
        and_to_list(B, BList),
        maplist(tr_numbered_sdcl_term, BList, BList1),
        list_to_and(BList1, RuleBody0), 
        tr_numbered_sdcl_term(H, RuleHead0),
        % unnumber the variables NB: both head and body are unnumbered
        % at once so that we preserve the correspondence between variables
        varnumbers((RuleHead0, RuleBody0), (RuleHead, RuleBody)),
        rule_head_to_rule_group(RuleHead, RuleGroup),
        !.

rule_head_to_rule_group(sdcl_term(F/A, _, Args), RuleGroup) :-
        copy_term(Args, Args1),
        numbervars(Args1), 
        RuleGroup = rule_group(F/A, Args1).

%% rule_groups(-RuleGroups) is det.
%% RuleGroups is a list of rule groups present in the current rule set. 
rule_groups(RuleGroups) :-
        findall(RuleGroup,
              sdcl_rule(_, _, _, _, RuleGroup),
              RuleGroups0),
        sort(RuleGroups0, RuleGroups).
        
        

:- begin_tests('translate sdcl rules').

test('translate sdcl rule (no weight)',
     [
      true(TermOut =@= sdcl_rule(_, Head, Body, Weight, RuleGroup))]
    ) :- 
        RuleIn  = (s(X, Y | Y, Z) ---> a(X | Y), b(Y)),
        tr_sdcl_clause(RuleIn, TermOut),
        Head = sdcl_term(s/4, [X, Y], [Y, Z]),
        Body = (sdcl_term(a/2, [X], [Y]), sdcl_term(b/1, [Y], [])),
        Weight = 1,
        RuleGroup = rule_group(s/4, ['$VAR'(0), '$VAR'(1)]).

test('translate sdcl rule (with weight)',
     [true(TermOut =@= sdcl_rule(_, Head, Body, Weight, RuleGroup))]
    ) :- 
        RuleIn  = (s(X, Y | Y, Z) ---> a(X | Y), b(Y) :: 3.2),
        tr_sdcl_clause(RuleIn, TermOut),
        Head = sdcl_term(s/4, [X, Y], [Y, Z]),
        Body = (sdcl_term(a/2, [X], [Y]), sdcl_term(b/1, [Y], [])),
        Weight = 3.2,
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
        
        

        
        
              

         
        