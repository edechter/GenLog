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
:- use_module(misc).

:- use_module(library(pairs)).
:- use_module(library(real)).

%%FIXME: What's going on with the options record here?

:- r(library("MCMCpack")).

%% ----------------------------------------------------------------------
%%     sample(Goal, LogProb)
%%     sample(Goal, LogProb, Options)
%%
%% Sample goal using current rule alphas. Will first sample rule
%% probabilities from Dirichlet prior, and then sample explanations
%% using those priors. Note: global rule probabilities will be
%% modified by this predicate.
sample(Goal, LogProb) :-
        sample(Goal, LogProb, []).

sample(Goal, LogProb, Options) :-
        writeln(top-Goal), 
        sample_probs_from_alphas,
        sample_(Goal, LogProb, Options).

%% sample_(Goal, LogProv, OptRecord)
%%
%% Worker predicate that either samples goal unconstrained or
%% constrained.
sample_(Goal, LogProb, OptRecord) :-
        (unconstrained(Goal) ->
         sample_unconstrained_(Goal, LogProb, OptRecord)
        ;
         sample_constrained_(Goal, LogProb, OptRecord)
        ).

%% sample_constrained_(Goal, LogProb, OptRecord)
%%
%% Sample a predicate that is constrained by some of its arguments
%% being ground. To do this, we need to find its derivations and
%% sample from those.
sample_constrained_(Goal, LogProb, OptRecord) :-
        writeln(mid1-Goal),
        mi_best_first_all(Goal, Derivations, _, OptRecord),
        writeln(mid-Goal),
        sample_from_derivations_(Goal, Derivations, LogProb, OptRecord).        

%% sample_from_derivations_(+Goal, +Derivations, LogProb)
%% sample_from_derivations_(+Goal, +Derivations, Derivation, LogProb)
%%
%% Given a set of Derivations (a list of deriv(Goal, DGraph,
%% ConditionalProb)), sample one of these and unify its result with Goal.
sample_from_derivations_(_Goal, [], _LogProb, _OptRecord) :-
        fail,
        !.
sample_from_derivations_(Goal, Derivations, LogProb, OptRecord) :-
        findall(Derivation-Prob,
                (member(Derivation, Derivations),
                 Derivation = deriv(_, _, Prob)),
                DPs),
        
        %% sample from distribution over derivations
        sample_categorical(DPs, Derivation, Prob),

         
        LogProb0 is log(Prob),

        %% for each unconstrained goal remaining in the derivation,
        %% sample unconstrained; there should not be any unconstrained
        %% nodes remaining in the derivation, because
        %% mi_best_first_all would have found derivations for those.
        
        Derivation = deriv(Goal, dgraph(_, Nodes, _), _),
        %% bind goal of this sample to its realization in this derivation

        sample_remaining_(Nodes, LogProb1, OptRecord),
        % findall(G-LogProb,
        %         (member(goal(_, G, _), Nodes),
        %          (unconstrained(G) ->
        %           writeln(2-G),
        %           sample_unconstrained_(G, LogProb, OptRecord),
        %           writeln(4-G)
        %          ;
        %           LogProb = 0)
        %         ),
        %         GLogProbs
        %        ),

        % last(GLogProbs, GlTerm-_),
        % translate_to_gl_term(Goal, GlTerm1),
        % writeln(glterm1-GlTerm1),
        % GlTerm=GlTerm1, 
        % writeln(42-(GlTerm=GlTerm1)),
        !,
        LogProb is LogProb0 + LogProb1.

sample_remaining_(Nodes, Out, Opts) :-
        sample_remaining_(Nodes, 0, Out, Opts).

sample_remaining_([], In, Out, _Opts) :-
        !,
        In=Out.
sample_remaining_([goal(_, G, _)|Nodes], In, Out, Opts) :-
        (unconstrained(G) ->
        sample_unconstrained_(G, LogProb, Opts)
        ;
         LogProb=0
        ),
        Tmp is In + LogProb,
        sample_remaining_(Nodes, Tmp, Out, Opts).

%% ----------------------------------------------------------------------
%%     sample_probs_from_alphas
%%
%% Sample rule probabilities using current Dirichlet distribution over
%% each rule group. Uses the rdirichlet function from R. Note: will
%% replace global rule probabilities with the probabilities sampled
%% here.
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

                           
                           
                           
                
        
        
        
                
        

% sample_unconstrained_(Goal, LogProb, OptionsRecord)
%
% sample an unconstrained goal. Because this is a generative model and
% their is no evidence, we can just sample sequentially.

sample_time_limit(1).

sample_unconstrained_(Goal, LogProb, OptRecord) :-
        \+ is_gl_term(Goal),
        !,
        translate_to_gl_term(Goal, GlTerm),
        sample_unconstrained_(GlTerm, LogProb, OptRecord).
sample_unconstrained_(GlTerm, LogProb, OptRecord) :-
        catch(
              (get_time(Now),
               sample_time_limit(T), 
               sample_unconstrained_(GlTerm, LogProb, Now-T, OptRecord)),
              time_limit_exceeded,
              (read(_),
               sample_unconstrained_(GlTerm, LogProb, OptRecord))).
              
sample_unconstrained_(true, _LogProb, StartTime-TimeLimit, _OptRecord) :-
        get_time(Now),
        Now-StartTime >= TimeLimit,
        !,
        throw(time_limit_exceeded).
sample_unconstrained_(true, LogProb, _TimeInfo, _OptRecord) :- !,
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
        
