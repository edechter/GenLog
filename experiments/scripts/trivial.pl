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



%% experiment root directory
:- absolute_file_name('/Users/edechter/Dropbox/Projects/SDCL/experiments/data', Abs),
   set_setting(experiment:root, Abs).

%% genlog file
gl_file(GlFile) :- 
   getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', 'experiments/gls', '/', 'trivial.gl'], GlFile).


        
goal( s([a,a,a,a,a]|[a]) ).
goal( s([a,a,b,b,a]|[a]) ).
goal( s([b,b]|[a]) ).
goal( s([a,a]|[a]) ).
        

goals(Goals) :-
        findall(G, goal(G),
                 Goals).

%% ----------------------------------------------------------------------
%%                               Constants
exp_constants(
   constants{beam_width:500,
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
        set_rule_alphas(normal(0.2, 0.005)),
        goals(Goals),
        list_to_random_choice(Goals, GoalGen),
        run_online_vbem(GoalGen, Data, Options1).
  

        