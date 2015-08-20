:- module(pianto_lang, [main/1]).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.


% add genlog project root directory to search path
:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', src], Src),
   asserta(user:file_search_path(genlog, Src));
   true.

% add this directory to search path
:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/experiments/scripts/'], Exp),
   asserta(user:file_search_path(experiment, Exp));
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
:- use_module(genlog(misc)).
:- use_module(genlog(interact)).

:- use_module(genlog(data_utils)).


%% experiment root directory
:- absolute_file_name('/Users/edechter/Dropbox/Projects/SDCL/experiments/data', Abs),
   set_setting(experiment:root, Abs).

%% genlog file
gl_file(GlFile) :- 
   getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', 'experiments/gls', '/', 'pianto_lang.gl'], GlFile).


goal(N, s(Xs)) :-
        replicate(N, [a], As),
        replicate(N, [b], Bs),
        % replicate(N, [c], Cs),
        append([As, Bs], Xs)
        ;
        replicate(N, [a], Xs).

        
goals(Goals, Lo, Hi) :-
        findall(G,
                (between(Lo, Hi, N),
                 goal(N, G)),
                 Goals).

%% ----------------------------------------------------------------------
%%                               Constants
exp_constants(
   constants{beam_width:100,
             time_limit_seconds:5,
             max_iter:100000}).


main(Options) :-
        gl_file(GL_FILE),
        compile_gl_file(GL_FILE),
        exp_constants(Const),
        Options0 = [beam_width(Const.beam_width),
                    time_limit_seconds(Const.time_limit_seconds),
                    constrained_only(false),
                    max_iter(Const.max_iter)],
        merge_options(Options, Options0, Options1),
        set_rule_alphas(normal(0.4, 0.05)),
        goals(Goals, 1, 4),
        list_to_random_choice(Goals, GoalGen),
        run_online_vbem(GoalGen, Data, Options1).
  

        