

:- module(compile,
          [compile_sdcl_file/1,
           
           remove_rule/1,
           remove_all_rules/0,

           rule_group_rules/2,
           
           translate_to_gl_term/2,
           translate_to_gl_rule/3,
           
           save_gl/0,
           save_gl/1,
           save_gl/2,
           save_gl/3,

           load_gl/1
          
           ]).

:- use_module(library(varnumbers)).
:- use_module(library(gensym)).
:- use_module(library(debug)).
:- use_module(sdcl).
:- use_module(gl_rule).
:- use_module(pprint).

% :- add_import_module(compile,
                      % user, end).

:- op(1000, xfy, --->).

:- op(1200, xfy, ::).
:- op(950, xfy, @).
%% ----------------------------------------------------------------------
/*
  gl_rule/5.

  A genlog rule is a structure of the form gl_rule(RuleId, RuleHead, RuleGuard, RuleBody, RuleGroup).
  
  RuleId:       Int for integer Int. This is a unique key for genlog rules.
  RuleHead:     a gl_term/3 structure representing the head of the rule.
  RuleGuard:    a list of prolog goals 
  RuleBody:     a conjunction of gl_term/3 structures representing the body of the rule.
  RuleGroup:    a rule_group/2 structure representing the group of alternative rules to which this rule belongs.

  For each genlog rule, we maintain global variables gl_rule_prob(RuleId) and
  a gl_rule_alpha(RuleId). Because these need to be modified
  frequently, these are maintained in the record database.

*/


%% ----------------------------------------------------------------------
%% compile GenLog syntax to prolog syntax
%%
%%

%% split_gl_file(+File, +PrologStream, +GenLogStream)
%%
%% Send regular prolog clauses into the prolog stream and anything
%% inside a genlog block into the genlog stream.
split_gl_file(File, PTmp, GLTmp) :-
       open(File, read, FileId),
       PTmp = './.__genlog_tmp_pl',
       GLTmp = './.__genlog_tmp_gl', 
       open(PTmp, write, PrologStream),
       open(GLTmp, write, GenLogStream),
       Mode0 = prolog,
       split_gl_file(FileId, Mode0, PrologStream, GenLogStream).

split_gl_file(FileId, Mode, PrologStream, GenLogStream) :-
        read_term(FileId, Clause, [module(compile)]),
        split_gl_file_go(Clause, FileId, Mode, PrologStream, GenLogStream).

split_gl_file_go(end_of_file, FileId, _, PrologStream, GenLogStream) :-
        !, 
        close(FileId),
        close(PrologStream),
        close(GenLogStream).
split_gl_file_go((:- begin(genlog)), FileId, prolog, PrologStream, GenLogStream) :-
         !,
         split_gl_file(FileId, genlog, PrologStream, GenLogStream).
split_gl_file_go(Clause, FileId, prolog, PrologStream, GenLogStream) :- 
        !,
        write_canonical(PrologStream, Clause), write(PrologStream, '.\n'), 
        split_gl_file(FileId, prolog, PrologStream, GenLogStream).
split_gl_file_go((:- end(genlog)), FileId, genlog, PrologStream, GenLogStream) :-
        !, 
        split_gl_file(FileId, prolog, PrologStream, GenLogStream).
split_gl_file_go(Clause, FileId, genlog, PrologStream, GenLogStream) :-
        !, 
        write_canonical(GenLogStream, Clause), write(GenLogStream, '.\n'), 
        split_gl_file(FileId, genlog, PrologStream, GenLogStream).
split_gl_file_go(_, FileId,_,PrologStream,GenLogStream) :-
        close(FileId),
        close(PrologStream),
        close(GenLogStream),
        throw(error(evaluation_error, context(split_gl_file/3, 'Cannot parse gl file.'))).
        
%%
compile_gl(File) :-
        open(File, read, FileId),
        remove_all_rules,
        reset_gensym,
        !,
        repeat,
        read_clause(FileId, Clause, []),
        (Clause = end_of_file ->
         true, !
        ; 
         compile_sdcl_clause(Clause),
         fail
        ).
       

compile_sdcl_file(File) :-
        debug(compile, "Compiling rules in file ~w...", [File]),
              
        split_gl_file(File, PTmp, GLTmp),
        load_files([PTmp], [module(compile)]),
        compile_gl(GLTmp),
        record_rule_groups, 
        normalize_rules,
        
        debug(compile, "Success! Finished compiling rules in file ~w.", [File]).

                  
compile_sdcl_clause(macro(Macro)) :-
        !,
        expand_macro(Macro, Rules),
        compile_sdcl_clauses(Rules).
compile_sdcl_clause(Clause) :-
        !, 
        (
         gensym('', RuleNum),
         atom_number(RuleNum, RuleNum1),
         RuleId = RuleNum1
        ),
         
        translate_to_gl_rule(Clause, TrClause, Prob),
        TrClause = gl_rule(RuleId, _, _, _, _),
        assert(TrClause),
        term_to_atom(gl_rule_prob(RuleId), NameP),
        term_to_atom(gl_rule_alpha(RuleId), NameA),
        nb_setval(NameP, Prob),
        nb_setval(NameA, 1.0).


compile_sdcl_clauses(Clauses) :-
        findall(_,
                (member(Clause, Clauses),
                 compile_sdcl_clause(Clause)),
                _).        


remove_rule_params(RuleId) :- 
        term_to_atom(gl_rule_prob(RuleId), NameP),
        term_to_atom(gl_rule_alpha(RuleId), NameA),
        nb_delete(NameP),
        nb_delete(NameA).

remove_rule(RuleId) :-
        remove_rule_params(RuleId), 
        retractall(gl_rule(RuleId, _, _, _, _)).

remove_all_rules :-
        rules(RuleIds),
        retractall(gl_rule(_, _, _, _, _)),
        forall(member(RuleId, RuleIds),
               remove_rule_params(RuleId)).

record_rule_groups :-
        retractall('gl_rule_group_rules'(_, _, _)),
        rule_groups(Gs),
        length(Gs, N),
        findall(Id, between(1, N, Id),GroupIds),
        pairs_keys_values(IdGroups, GroupIds, Gs),
        forall(member(Id-G, IdGroups),
               (get_rule_group_rules(G, RuleIds),
                assert('gl_rule_group_rules'(Id, G, RuleIds)))).

rule_group_rules(RuleGroup, Rules) :-
        'gl_rule_group_rules'(_, RuleGroup, Rules),
        !.

rule_group_id_rules(RuleGroupId, Rules) :-
        'gl_rule_group_rules'(RuleGroupId, _, _),
        !.
        

        

%% display compiled rules
show_rules :-
        findall(Id-W, 
                (gl_rule(Id, _, _, _, _),
                 get_rule_prob(Id, W)), 
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
     [setup(remove_all_rules),
      cleanup(remove_all_rules),
      true(AClause =@= TClause)]) :-
        reset_gensym,
        Clause = (s(_X, _Y | [boy], _Y)),
        compile_sdcl_clause(Clause),
        TClause = gl_rule(1, gl_term(s/4, [_X1, _Y1], [[boy], _Y1]),[],
                     true, rule_group(s/4, [[boy], '$VAR'(0)], [])),
        call(TClause),
        AClause = gl_rule(_, _, _, _, _),
        call(AClause).
:- end_tests(compile).


%% ----------------------------------------------------------------------
%%        translating
%%
%% translate GenLog syntax to structure representation
%%
%% an GenLog term is of the form
%% gl_term(Functor/Arity, Vars, Conds)

%%        translate_to_gl_term(+TermIn, -TermOut) is det.
%%
%% - TermIn is a term in genlog syntax, e.g., s(X, Y | Y, G)
%% - TermOut is a gl_term/3 structure, e.g. gl_term(s/4, [X, Y], [Y, G]).

translate_to_gl_term(TermIn, TermOut) :-
        copy_term(TermIn, TermInCopy), 
        numbervars(TermInCopy),
        tr_numbered_gl_term(TermInCopy, TermTmp),
        varnumbers(TermTmp, TermOut),
        term_variables(TermOut, VsOut),
        term_variables(TermIn, VsIn),
        VsIn = VsOut.

:- begin_tests('translate genlog terms').

test(translate_to_gl_term1,
     [true(TermOut =@= gl_term(s/4, [_A, B],  [B, _C]))]
    ) :- 
        TermIn  = s(_X, Y | Y, _Z),
        translate_to_gl_term(TermIn, TermOut).

test(translate_to_gl_term2,
     [true(TermOut =@= gl_term(s/4, [[_A], B],  [B, _C]))]
    ) :- 
        TermIn  = s([_X], Y | Y, _Z),
        translate_to_gl_term(TermIn, TermOut).
        
:- end_tests('translate genlog terms').

%%         translate_to_gl_rule(+Clause, -GenLogRule, -RuleWeight)
%%
%% - Clause is a prolog clause in GenLog syntax.
%% - GenLog is the corresponding gl_rule/4 term.
%% - RuleWeight is the probability value extracted from the rule (or a default of 1).
%%
%% Note: RuleId is not bound here. It must be bound later to provide
%% unique id to the rule.
translate_to_gl_rule(Clause, Rule, RuleWeight) :-
        \+ var(Clause),
        copy_term(Clause, ClauseCopy), 
        numbervars(ClauseCopy), 
        Rule = gl_rule(_, RuleHead, RuleGuards, RuleBody, RuleGroup),
        % strip rule weight if present
        (
         ClauseCopy = (Clause1 :: RuleWeight)
        ;
         ClauseCopy = Clause1,
         RuleWeight = 1.0
        ),
        % extract body
        (
         Clause1 = (HG ---> B) -> true
        ;
         Clause1 = HG,
         B = true
        ),
        % extract list of guards
        (
         HG = (H @ G) -> true
        ;
         H = HG,
         G = []
        ),
        % we always set default alpha value to 1
        RuleAlpha = 1.0,
        and_to_list(B, BList),
        maplist(tr_numbered_gl_term, BList, BList1),
        list_to_and(BList1, RuleBody0), 
        tr_numbered_gl_term(H, RuleHead0),
        RuleGuards0 = G,
        % unnumber the variables NB: both head and body are unnumbered
        % at once so that we preserve the correspondence between variables
        varnumbers((RuleHead0, RuleGuards0, RuleBody0), (RuleHead, RuleGuards, RuleBody)),
        goal_to_rule_group(RuleHead, RuleGuards, RuleGroup),
        !.

goal_to_rule_group(gl_term(F/A, _, Args), Guards, RuleGroup) :-
        copy_term((Args, Guards), (Args1, Guards1)),
        numbervars((Args1, Guards1)), 
        RuleGroup = rule_group(F/A, Args1, Guards1).

        
        

:- begin_tests('translate gl rules').

test('translate gl rule (no weight)',
     [
      true(TermOut =@= gl_rule(_, Head, Guards, Body, RuleGroup))
     ]
    ) :- 
        RuleIn  = (s(X, Y | Y, Z) @ [Y = Z] ---> a(X | Y), b(Y)),
        translate_to_gl_rule(RuleIn, TermOut, RuleWeight),
        Head = gl_term(s/4, [X, Y], [Y, Z]),
        Body = (gl_term(a/2, [X], [Y]), gl_term(b/1, [Y], [])),
        Guards = [Y=Z],
        Weight = 1.0,
        Alpha = 1.0, 
        RuleGroup = rule_group(s/4, ['$VAR'(0), '$VAR'(1)], ['$VAR'(0)='$VAR'(1)]),
        assertion(RuleWeight = Weight).

test('translate gl rule (with weight)',
     [true(TermOut =@= gl_rule(_, Head, Guards, Body, RuleGroup))]
    ) :- 
        RuleIn  = (s(X, Y | Y, Z) ---> a(X | Y), b(Y) :: 3.2),
        translate_to_gl_rule(RuleIn, TermOut, Weight),
        Head = gl_term(s/4, [X, Y], [Y, Z]),
        Guards = [],
        Body = (gl_term(a/2, [X], [Y]), gl_term(b/1, [Y], [])),
        Weight = 3.2,
        Alpha = 1.0,
        RuleGroup = rule_group(s/4, ['$VAR'(0), '$VAR'(1)], []),
        assertion(Weight=RuleWeight).
                        
:- end_tests('translate gl rules').


tr_numbered_gl_term(TIn, gl_term(Functor/Arity, Vars, Conds)) :-
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
        replace_qvars_with_vars((Head :- Body),
                                (Head1 :- Body1)),
        findall(Head1,
                call_comp(Body1),
                Rules1),
        findall(Rule,
                (member(Rule1, Rules1),
                 comp_to_term(Rule1, Rule)
                 ),
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

test_expanded_qvars([(a(X | Y, Z) ---> a(X | Y), b(X | Y,Z)),
                      (b(X | Y, Z) ---> a(X | Y), b(X | Y,Z))]).

test(expand_macro_qvars,
     [setup(test_expanded_qvars(TRules)),
      all(Rule =@= TRules)]) :-
        expand_macro( (
                       ('?X'(X | Y, Z) ---> a(X | Y), b(X | Y,Z))
                       :- 
                       (member('?X', [a, b]))
                      ),
                      Rules),
        member(Rule, Rules).

:- end_tests(translate).

        
%% replace q_vars with Prolog vars.
%% a Q var is an atom of the form '?..'
%% Q vars can appear as functors

is_qvar(X) :-
        atom(X), 
        sub_atom(X, 0, _, _, '?').

replace_qvars_with_vars(TermIn, TermOut) :-
        empty_assoc(Empty), 
        replace_qvars_with_vars(Empty, TermIn, _, TermOut).

replace_qvars_with_vars(AssocIn, Q, AssocOut, TermOut) :-
        is_qvar(Q),
        !,
        (
         get_assoc(Q, AssocIn, V) ->
         TermOut = V,
         AssocIn = AssocOut
        ;
         put_assoc(Q, AssocIn, V, AssocOut),
         TermOut = V
        ).
replace_qvars_with_vars(AssocIn, V, AssocOut, TermOut) :-
        var(V),
        !,
        AssocOut = AssocIn,
        TermOut = V.
replace_qvars_with_vars(AssocIn, [], AssocIn, []) :- !.
replace_qvars_with_vars(AssocIn, [X|Xs], AssocOut, TermOut) :-
        !,
        replace_qvars_with_vars(AssocIn, X, AssocTmp, XOut),
        replace_qvars_with_vars(AssocTmp, Xs, AssocOut, XsOut),
        TermOut = [XOut|XsOut].
replace_qvars_with_vars(AssocIn, A, AssocOut, AOut) :-
        atomic(A),
        !,
        AssocIn = AssocOut,
        A = AOut.
% if Term is an explicit compound term
replace_qvars_with_vars(AssocIn, Term, AssocOut, TermOut) :-
        compound(Term),
        Term =.. ['$COMP'|As],
        !,
        replace_qvars_with_vars(AssocIn, As, AssocOut, AsOut),
        TermOut =.. ['$COMP'|AsOut].
% if it has a qvar as a functor
replace_qvars_with_vars(AssocIn, Term, AssocOut, TermOut) :-
        compound(Term),
        Term =.. [F|As],
        is_qvar(F),
        !,
        Term1 = '$COMP'(F,As),
        replace_qvars_with_vars(AssocIn, Term1, AssocOut, TermOut).
% if it is any other compound term
replace_qvars_with_vars(AssocIn, Term, AssocOut, TermOut) :-
        compound(Term),
        Term =.. [F|As],
        !,
        replace_qvars_with_vars(AssocIn, As, AssocOut, AsOut),
        TermOut =.. [F|AsOut].

%% comp_to_term(+CompTerm, -Term) translates between an explicit
%% compound term '$COMP'(Functor, Args) and a Prolog compound term
%% Functor(Args). Ignores functors :-/2 and any built in predicates.

comp_to_term(X, X) :-
        var(X),
        !.
comp_to_term('$COMP'(F, As), Term) :-
        !,
        assertion(atomic(F)),
        comp_to_term(As, As1),
        Term =.. [F| As1].
comp_to_term([], []) :- !.
comp_to_term([X|Xs], [Y|Ys]) :-
        !,
        comp_to_term(X, Y),
        comp_to_term(Xs, Ys).
comp_to_term(TermIn, TermOut) :-
        TermIn =.. [F|As],
        !,
        comp_to_term(As, AsOut),
        TermOut =.. [F|AsOut].
comp_to_term(X, X).
        
         
%% call_comp(+CompTerm) is a meta_interpreter that evaluates explicit
%% compound terms as generated by expand_macro; it evalutes an
%% explicit compound if present, otherwise just calls
% :- meta_predicate
%         call_comp(0,:)
        

% :- add_import_module(compile, genlog, start).
call_comp('$COMP'(F,As)) :-
        !,
        T =.. [F|As],
        call_comp(T).
call_comp((A,B)) :-
        !,
        call_comp(A),
        call_comp(B).
call_comp(X) :- call(X).
        
        


%% ----------------------------------------------------------------------
%%      save_gl(+File).
%%      save_gl(+File, +Options).

%% default location to save gl files
'*default_gl_save_dir*'('.').
'*default_gl_prefix*'('gl_'). 

save_gl :-
        '*default_gl_save_dir*'(Dir),
        '*default_gl_prefix*'(Prefix),
        save_gl(Dir, Prefix, []).

save_gl(File) :-
        save_gl(File, []).

save_gl(File, Options) :-
        (
         exists_file(File) ->
           throw(evaulation_error(save_gl/2, 'file already exists'))
        ;
         tell(Path),
         listing(compile:gl_rule/5),
         listing(compile:gl_rule_group_rules/3),
         write_global_vars_,
         (member(ovbem_info(Info), Options) ->
          portray_clause(Info)),
         told
        ).

save_gl(Dir, Prefix, Options) :-
        %% assuming no more than 1e4 GLs of the same prefix.
        between(1, 1000, I),
        format(atom(File), "~w~|~`0t~d~4+.gl", [Prefix, I]),
        directory_file_path(Dir, File, Path),
        save_gl(Path, Options).

write_global_vars_ :-
        nb_current(N, V),
        format("global_var(~w, ~w).\n", [N, V]),
        fail
        ;
        true.
        

        



        


%% ----------------------------------------------------------------------
%%      load_gl(+File).

load_gl(File) :-
        \+ exists_file(File),
        !,
        format(atom(Msg), "Cannot find file ~w", [File]),
        throw(evaluation_error(load_gl/1, Msg)).
load_gl(File) :-
        style_check(-singleton), 
        qcompile(File),
        load_global_vars_,
        style_check(+singleton).

load_global_vars_ :-
        forall(global_var(C, V),
               (term_to_atom(C, T),
                nb_setval(T, V))).
        


        
        
              

         
        