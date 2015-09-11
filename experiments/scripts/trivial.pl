:- module(trivial, [main/1]).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.


% add genlog project root directory to search path
:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', src], Src),
   asserta(user:file_search_path(genlog, Src));
   true.
        
%% ----------------------------------------------------------------------

:- use_module(genlog(experiment)).

:- use_module(genlog(prove)).
:- use_module(genlog(gl_rule)).
:- use_module(genlog(kb)).
:- use_module(genlog(compile)).

:- use_module(genlog(learn)).
:- use_module(genlog(pgen)).
:- use_module(genlog(pprint)).

:- use_module(genlog(data_utils)).
:- use_module(genlog(interact)).
:- use_module(genlog(sample)).



%% experiment root directory
:- absolute_file_name('/Users/edechter/Dropbox/Projects/SDCL/experiments/data', Abs),
   set_setting(experiment:root, Abs).

%% genlog file
gl_file(GlFile) :- 
   getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', 'experiments/gls', '/', 'trivial.gl'], GlFile).



goal_count(1000).

% goal( count(s([a,a]), N) ) :- goal_count(N).
% goal( count(s([b,b]), N) ) :- goal_count(N).
% goal( count(s([a,a,a]), N) ) :- goal_count(N), N1 is N / 2.
% goal( count(s([b,b,b]), N) ) :- goal_count(N), N1 is N / 2.
% goal( count(s([a,a,a,a]), N) ) :- goal_count(N), N1 is N / 4.
% goal( count(s([b,b,b,b]), N) ) :- goal_count(N), N1 is N / 4.

goal( count(s([a,b]), N) ) :- goal_count(N).
goal( count(s([a,a,b,b]), N1) ) :- goal_count(N), N1 is N/2.
goal( count(s([a,a,a,b,b,b]), N1) ) :- goal_count(N), N1 is N/4.
goal( count(s([a,a,a,a,b,b,b,b]), N1) ) :- goal_count(N), N1 is N/4.



goals(Goals) :-
        findall(G, goal(G),
                 Goals).

%% ----------------------------------------------------------------------
%%                               Constants
exp_constants(
   constants{beam_width:200,
             time_limit_seconds:5,
             goals_per_iter: 1,
             max_iter:50,
             max_online_iter: 100}).


main(Options) :-
        gl_file(GL_FILE),
        compile_gl_file(GL_FILE),
        exp_constants(Const),
        merge_options(Options, Const, Options1),
        % set_rule_alphas(normal(0.2, 0.05)),
        set_rule_alphas(uniform(2)),
        goals(Goals),
        list_to_random_choice(Goals, GoalGen),
        list_to_gen(Goals, GoalGen),
        run_online_vbem(GoalGen, Data, Options1).
  

run_script :-
        % repeat,
        random_property(state(E)),
        writeln(E),
        % open('/Users/edechter/Dropbox/Projects/SDCL/scratch/seed.txt', read, S),
        % read(S, T),
        % T = state(Rand),
        % set_random(state(Rand)),
        run_experiment('trivial.pl'),
        sample_probs_from_alphas,
        normalize_rules,
        pprint_rule_probs,
        ask(s(X), [beam_width(100), time_limit_seconds(2)]).
        
        
    