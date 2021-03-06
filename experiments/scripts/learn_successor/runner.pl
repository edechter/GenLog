%% runner.pl
%%
%% Author: Eyal Dechter
%% ----------------------------------------------------------------------
 
:- module(runner, [main/1]).

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

:- ensure_loaded(experiment(number_words)).

%% experiment root directory
:- getenv('HOME', Dir),
   atomic_list_concat([Dir, '/', data], Root), 
   set_setting(experiment:root, Root).

%% select genlog file
gl_file(GlFile) :-
   getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', experiments, '/', gls, '/', 'succ_04.gl'], GlFile).

%% ------------------------------------------
%% make number data
succ_goal(N, Count, Goal) :-
        number_word(N, W),
        N1 is N+1,
        number_word(N1, W1),
        % append(W, W1, W2),
        Goal = count(succ(W, W1), Count).

succ_goals(Lo, Hi, Count, Goals) :-
        findall(Goal,
                (between(Lo, Hi, N),
                 succ_goal(N, Count, Goal)),
                Goals).

power_law_goals(Exp, Lo, Hi, C, GoalWeights) :-
        succ_goals(Lo, Hi, 1, Gs),
        enum(Gs, IGs),
        findall(G-W,
                (member(I-G, IGs),
                 W is C/(I**Exp)),
                GoalWeights).

%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%%                               Constants
exp_constants(
   constants{beam_width:500,
             time_limit_seconds:5,
             max_iter:100000,

             exp:0.85,
             c:10,
             lo:1,
             hi:30}).

%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------

main(Options) :-
        exp_constants(Const),
        gl_file(GlFile),
        compile_gl_file(GlFile),
        Options0 = [beam_width(Const.beam_width),
                    time_limit_seconds(Const.time_limit_seconds),
                    constrained_only(false),
                    max_iter(Const.max_iter)],
        merge_options(Options, Options0, Options1),
        set_rule_alphas(normal(0.2, 0.005)),

        % power_law_goals(Const.exp,
        %                 Const.lo,
        %                 Const.hi,
        %                 Const.c,
        %                 GoalWeights),
        succ_goals(1, 99, 1, Goals),
        % succ_goals(20, 28, 1, Gs1), 
        % append(Gs0, Gs1, Goals),
        % list_to_categorical(GoalWeights, GoalGen),xo
        list_to_random_choice(Goals, GoalGen),
        run_online_vbem(GoalGen, _Data, Options1).

% %% ----------------------------------------------------------------------
% %%    analyze

% analyze1(GlFile, Ls) :-
%         load_gl(GlFile),
%         number_goals(1, 20, 1, Goals),
%         prove_goals(Goals, Ds, [beam_width(10), time_limit_seconds(10)]), 
%         findall(L,k

%                 (member(D, Ds), 
%                  loglikelihood(D, L)),
%                 Ls).

% analyze(Dir, LoglikelihoodData) :-
%         absolute_file_name(Dir, Path),
%         directory_files(Path, Files),
%         findall(F, 
%                 (member(F, Files),
%                  atom_prefix(F, 'ovbem_gl'),
%                  file_name_extension(_, 'gl', F)),
%                 Files1),
%         % writeln(Files1),
%         retractall(worked(_)),
%         findall(Ls,
%                 (member(F, Files1),
%                  directory_file_path(Path, F, P),
%                  (analyze1(P, Ls) ->
%                   assert(worked(P))
%                  ;
%                   throw(error('FAIL'))
%                  )
%                 ),
%                 LoglikelihoodData),
%         length(LoglikelihoodData, N).
        


bench :-
        exp_constants(Const),
        gl_file(GlFile),
        compile_sdcl_file(GlFile),
        Options = [beam_width(Const.beam_width),
                    time_limit_seconds(Const.time_limit_seconds),
                    max_iter(Const.max_iter)],
        succ_goal(15, 1, G),
        prove_goals([G], _L, Options).
            


df_list(X-X, []) :- !.
df_list([X|Y]-Z, [X|Xs]) :- df_list(Y-Z, Xs).