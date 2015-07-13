%% sample.pl
%% author: Eyal Dechter
%% ----------------------------------------------------------------------

:- module(sample, []).

:- use_module(sdcl).
:- use_module(gl_rule).
:- use_module(compile).
:- use_module(pgen).

%%FIXME: What's going on with the options record here?


sample(Goal, LogProb) :-
        sample(Goal, LogProb, []).

sample(Goal, LogProb, Options) :-

        % make options record
        % make_sample_options(Options, OptRecord, _RestOptions),

        sample_(Goal, LogProb, Options).

sample_(Goal, LogProb, OptRecord) :-
        (unconstrained(Goal) ->
         sample_unconstrained_(Goal, LogProb, OptRecord)
        ;
         sample_constrained_(Goal, LogProb, OptRecord)
        ).

sample_constrained_(Goal, LogProb, OptRecord) :-
        mi_best_first_all(Goal, Derivations, _, OptRecord), 
        sample_from_derivations_(Goal, Derivations, LogProb).        

sample_from_derivations_(Goal, [], LogProb) :- fail, !.
sample_from_derivations_(Goal, Derivations, LogProb) :-
        findall(Derivation-Prob,
                (member(Derivation, Derivations),
                 Derivation = deriv(_, _, Prob)),
                DPs),
        %% choose derivation
        sample_categorical(DPs, Derivation, Prob),
         
        LogProb0 is log(Prob),

        %% for each unconstrained goal remaining in the derivation,
        %% sample unconstrained
        Derivation = deriv(Goal, dgraph(_, Nodes, _), _),
        bagof(LogProb,
                (member(goal(_, G, _), Nodes),
                 (unconstrained(G) ->
                  sample_unconstrained_(G, LogProb, OptRecord)
                 ;
                  LogProb = 0)
                ),
                LogProbs
               ),
        !,
        sum_list(LogProbs, LogProb1), 
        LogProb is LogProb0 + LogProb1.
                           
                           
                           
                
        
        
        
                
        

% sample_unconstrained_(Goal, LogProb, OptionsRecord)
%
% sample an unconstrained goal. Because this is a generative model and
% their is no evidence, we can just sample sequentially. Easy. 
sample_unconstrained_(true, LogProb, OptRecord) :- !,
        LogProb = 0.
sample_unconstrained_((Goal, Rest), LogProb, OptRecord) :-
        % pprint_term(Goal, Out),
        % writeln(Out),
        !,
        sample_unconstrained_(Goal, LogProb0, OptRecord),
        sample_unconstrained_(Rest, LogProb1, OptRecord),
        LogProb is LogProb0 + LogProb1.
sample_unconstrained_(Goal, LogProb, OptRecord) :-
        % pprint_term(Goal, Out),
        % writeln(Out),
        copy_term(Goal, GoalCp),
        Goal = gl_term(F/A, Args, Conds),
        writeln(Goal), 
        findall(RuleId-Prob,
                (Rule = gl_rule(RuleId, Goal, HGuard-BGuard, Body, _),
                 call(Rule),
                 call_list(HGuard),
                 get_rule_prob(RuleId, Prob)),
                RuleDistribution),

        list_to_categorical(RuleDistribution, Gen),

        yield(Gen, RuleId, _),
        member(RuleId-Prob, RuleDistribution),
        !,
        Rule = gl_rule(RuleId, Goal, HGuard-BGuard, Body, _),
        call(Rule),
        call_list(HGuard),
        call_list(BGuard),
        !, 
        LogProb0 is log(Prob),
        sample_unconstrained_(Body, LogProb1, OptRecord),
        LogProb is LogProb0 + LogProb1.
                 


call_list([]) :- !.
call_list([G|Gs]) :-
        call(G),
        call_list(Gs).
        

        
sample_categorical(KWs, K, W) :-
        list_to_categorical(KWs, Gen),
        yield(Gen, K, _),
        member(K-W, KWs),
        !.
        
