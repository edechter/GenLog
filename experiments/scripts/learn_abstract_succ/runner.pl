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
   atomic_list_concat([Dir, '/', 'experiments/gls', '/', 'abstract_succ.gl'], GlFile).

% goal_list([
%            s([1, a], [2, b]),
%            s([1, b], [2, c]),
%            s([2, a], [3, b]),
%            s([2, b], [3, c])
%            ]).

goal_list([
           s([1, a], [1, b]),
           s([1, b], [1, c]),
           s([1, c], [2, a]),
           
           s([2, a], [2, b]),
           s([2, b], [2, c]),
           s([2, c], [3, a]),           

           s([3, a], [3, b]),
           s([3, b], [3, c]),
           s([3, c], [4, a])           
          ]).

goal(N, G) :-
        goal_list(List), 
        random_member(G, List).         


%% ----------------------------------------------------------------------
%%                               Constants
exp_constants(
   constants{beam_width:100,
             time_limit_seconds:5,
             max_iter:100}).


main(Options) :-
        gl_file(GL_FILE),
        compile_gl_file(GL_FILE),
        exp_constants(Const),
        Options0 = [beam_width(Const.beam_width),
                    time_limit_seconds(Const.time_limit_seconds),
                    constrained_only(false),
                    max_iter(Const.max_iter)],
        merge_options(Options, Options0, Options1),
        set_rule_alphas(normal(0.2, 0.05)),
        goal_list(Goals),
        list_to_random_choice(Goals, GoalGen),
        run_online_vbem(GoalGen, Data, Options1).
  

        