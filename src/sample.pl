%% sample.pl
%% author: Eyal Dechter
%% ----------------------------------------------------------------------

:- module(sample, [sample/2,
                   sample/3,
                   sample_probs_from_alphas/0]).

:- use_module(sdcl).
:- use_module(gl_rule).
:- use_module(compile).
:- use_module(pgen).
:- use_module(pprint).

:- use_module(library(pairs)).

:- use_module(library(real)).

%%FIXME: What's going on with the options record here?

:- r(library("MCMCpack")).
        
sample(Goal, LogProb) :-
        sample(Goal, LogProb, []).

sample(Goal, LogProb, Options) :-

        % make options record
        % make_sample_options(Options, OptRecord, _RestOptions),
        sample_probs_from_alphas,  
        sample_(Goal, LogProb, Options).

sample_probs_from_alphas :-
        num_rule_groups(N),
        forall(between(1, N, RGId),
               (sdcl:get_rule_group_id_alphas(RGId, RAs),
                pairs_keys_values(RAs, Rs, As),
                Xs <- rdirichlet(1, As),
                [Ps]= Xs,
                pairs_keys_values(RPs, Rs, Ps), 
                forall(member(R-P, RPs),
                       set_rule_prob(R, P))
               )
              ).

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
        % writeln(Derivation),
         
        LogProb0 is log(Prob),

        %% for each unconstrained goal remaining in the derivation,
        %% sample unconstrained
        Derivation = deriv(Goal, dgraph(_, Nodes, _), _),
        %% bind goal of this sample to its realization in this derivation
        last(Nodes, goal(_, P, _)),
        translate_to_gl_term(Goal, Goal1),
        Goal1 = P,
        writeln(P), 
        bagof(LogProb,
                (member(goal(_, G, H), Nodes),
                 (unconstrained(G) ->

                  translate_to_gl_term(G1, G),
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
% their is no evidence, we can just sample sequentially.

sample_time_limit(1).
sample_unconstrained_(Goal, LogProb, OptRecord) :-
        translate_to_gl_term(Goal, GoalTr), 
        catch(
              (get_time(Now),
               sample_time_limit(T), 
               sample_unconstrained_(GoalTr, LogProb, Now-T, OptRecord)),
              time_limit_exceeded,
              (read(_),
               sample_unconstrained_(Goal, LogProb, OptRecord))).
              
            
              
sample_unconstrained_(true, LogProb, StartTime-TimeLimit, OptRecord) :-
        get_time(Now),
        Now-StartTime >= TimeLimit,
        !,
        throw(time_limit_exceeded).
sample_unconstrained_(true, LogProb, TimeInfo, OptRecord) :- !,
        writeln(true), 
        LogProb = 0.
sample_unconstrained_((Goal, Rest), LogProb, TimeInfo, OptRecord) :-
        pprint_term((Goal, Rest)), nl, 
        !,
        sample_unconstrained_(Goal, LogProb0, TimeInfo, OptRecord),
        sample_unconstrained_(Rest, LogProb1, TimeInfo, OptRecord),
        LogProb is LogProb0 + LogProb1.
sample_unconstrained_(Goal, LogProb, TimeInfo, OptRecord) :-
        pprint_term(Goal, Out),
        writeln(Out),
        Goal = gl_term(F/A, Args, Conds),
        findall(RuleId-Prob,
                (Rule = gl_rule(RuleId, Goal, HGuard-BGuard, Body, _),
                 call(Rule),
                 call_list(HGuard),
                 get_rule_prob(RuleId, Prob)
                ),
                RuleDistribution),

        list_to_categorical(RuleDistribution, Gen),

        yield(Gen, RuleId, _),
        member(RuleId-Prob, RuleDistribution),
        format('~w -- ', [RuleId]), pprint_rule(RuleId), format('with prob ~w', [Prob]), nl,
        !,
        Rule = gl_rule(RuleId, Goal, HGuard-BGuard, Body, _),
        call(Rule),
        pprint_rule(RuleId), nl, 
        
        call_list(HGuard),
        !, 
        LogProb0 is log(Prob),
        sample_unconstrained_(Body, LogProb1, TimeInfo, OptRecord),
        call_list(BGuard),
        
        
        LogProb is LogProb0 + LogProb1.
                 
take(N, Xs, Ys) :-
        length(Ys, N),
        append(Ys, _, Xs).

call_list([]) :- !.
call_list([G|Gs]) :-
        call(G),
        call_list(Gs).

call_list_once([]).
call_list_once([G|Gs]) :-
        once(G),
        call_list(Gs).


        
sample_categorical(KWs, K, W) :-
        list_to_categorical(KWs, Gen),
        yield(Gen, K, _),
        member(K-W, KWs),
        !.
        
