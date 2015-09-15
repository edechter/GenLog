%% runner.pl
%%
%% Author: Eyal Dechter
%% ----------------------------------------------------------------------
 
:- module(runner, [main/1,
                   succ_goal/3
                  ]).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

% add genlog project root directory to search path
:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', src], Src),
   asserta(user:file_search_path(genlog, Src));
   true.

% add this directory to search path
:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/experiments/scripts/learn_successor/'], Exp),
   asserta(user:file_search_path(experiment, Exp));
   true.

%% ----------------------------------------------------------------------

:- use_module(library(lists)).

:- use_module(genlog(experiment)).
:- use_module(genlog(gl_rule)).
:- use_module(genlog(prove)).
:- use_module(genlog(compile)).

:- use_module(genlog(learn)).
:- use_module(genlog(pgen)).
:- use_module(genlog(pprint)).

:- use_module(genlog(data_utils)).
:- use_module(genlog(kb)).
:- use_module(genlog(misc)).
:- use_module(genlog(interact)).

:- ensure_loaded(experiment(number_words)).

:- use_module(experiment(analyze)).
:- use_module(experiment(plot)).

%% experiment root directory
:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', experiments, '/', data], Root),
   set_setting(experiment:root, Root).

%% select genlog file
gl_file(GlFile) :-
   getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', experiments, '/', gls, '/', 'succ_04.gl'], GlFile).

%% ------------------------------------------
%% make number data

number_word_settings([split_teens(false), irregular_decade(true), split_decade(false)]).

:- number_word_settings(Settings),
        forall(member(S, Settings),
               (S =.. [F, V],
                set_setting(number_words:F, V))).

succ_goal(N, Count, Goal) :-
        number_word(N, W),
        N1 is N+1,
        number_word(N1, W1),
        Goal = count(succ(W, W1), Count).

succ_goals(Lo, Hi, Count, Goals) :-
        findall(Goal,
                (between(Lo, Hi, N),
                 succ_goal(N, Count, Goal)),
                Goals).

decade_goal(N0, Count, Goal) :-
        between(2, 9, N0),
        N is N0 * 10, 
        number_word(N, W),
        N1 is N + 10,
        number_word(N1, W1),
        Goal = count(succ(W, W1), Count).

decade_goals(Count, Goals) :-
        findall(Goal,
                decade_goal(_, Count, Goal),
                Goals).

transition_goal(Count, Goal) :-
        between(2, 9, D),
        N is D * 10 -1,
        succ_goal(N, Count, Goal).

transition_goals(Count, Goals) :-
        findall(G,
                transition_goal(Count, G),
                Goals).
        

power_law_goals(Exp, Lo, Hi, C, GoalWeights) :-
        succ_goals(Lo, Hi, 1, Gs),
        enum(Gs, IGs),
        findall(G-W,
                (member(I-G, IGs),
                 W is C/(I**Exp)),
                GoalWeights).

goal_list(Goals) :-
        succ_goals(1, 9, 1000, Goals1),
        % Goals2=[],
        succ_goals(1, 99, 1000, Goals2),
        % Goals3=[],
        transition_goals(1000, Goals3),
        append([Goals1, Goals2, Goals3], Goals).

%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%%                               Constants
exp_constants(
   constants{beam_width:          200,
             time_limit_seconds:  10,
             init_alpha :         uniform(2),
             max_iter   :         10
            }
             ).

phase(
       constants{
                 goal_generator: GoalGen, 
                 goals_per_iter: L,
                 max_online_iter: 1,
                 run_id: phase
                }
      ) :-
        goal_list(Goals),
        length(Goals, L), 
        list_to_circular(Goals, GoalGen).





phases([phase]).


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------

init_run :-
        load_random_seed,
        gl_file(GlFile),
        compile_gl_file(GlFile),
        exp_constants(Consts),
        set_rule_alphas(Consts.init_alpha).
        
run_phase(Phase, Options0) :-
        PhaseSpec =.. [Phase, Spec],
        call(PhaseSpec),
        exp_constants(Consts),
        merge_options(Spec, Options0, Options1), 
        merge_options(Consts, Options1,  Options),
        GoalGen = Spec.goal_generator,
        run_online_vbem(GoalGen, _, Options).

run_training(Options) :-
        init_run,
        phases(Phases),
        forall(member(Phase, Phases),
               run_phase(Phase, Options)).

run_testing(_Options) :-
        count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), 
        asserta(data(D)), 
        pairs_keys_values(D.transition_probs, Xs, Ys), 
        plot_xy(Xs, Ys), 
        show,
        writeln(D).

main(Options) :-
        run_training(Options),
        run_testing(Options).




%% ----------------------------------------------------------------------
%%    test


