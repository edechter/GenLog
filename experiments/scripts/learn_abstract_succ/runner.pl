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

%% ----------------------------------------------------------------------
%% Create the observed data goals.
%% There are several categories:
%% 1) Units: analog of 1-10
%% 2) Decades: analog of 10-99 minus the transition goals (see 3)
%% 3) Transitions: analog of 9-10, 19-20, ...

unit_list([1,2,3,4]).

unit_goals(Xs) :- findall(X, unit_goal(X), Xs). 

unit_goal(s([X], [Y])) :-
        unit_list(Units), 
        append([_, [X, Y], _], Units).

decade_goals(Goals) :-
        findall(G, decade_goal(G), Goals).

decade_goal(s([D, b, U], [D, b, U1])) :-
        unit_list(Units),
        member(D, Units), 
        member(U, Units), 
        last(Units, L),
        U \= L,
        unit_goal(s([U], [U1])).

transition_goal(s([D, b, Last], [D1, b, First])) :-
        unit_goal(s([D], [D1])),
        unit_list(Units),
        last(Units, Last),
        Units = [First|_].
              
goals(phase1, Goals) :-
        unit_goals(Goals).
goals(phase2(UnitWeight, DecadeWeight, TransitionWeight), Goals) :-
        findall(U-UnitWeight,
                unit_goal(U),
                UnitGoals),
        
        findall(D-DecadeWeight,
                decade_goal(D),
                DecadeGoals),
        writeln(DecadeGoals),
        findall(T-TransitionWeight,
                transition_goal(T),
                TransitionGoals),
        
        append([UnitGoals, DecadeGoals, TransitionGoals], Goals).
                     

                 
                 
%% ----------------------------------------------------------------------
%%                               Constants
exp_constants(
   constants{beam_width:100,
             time_limit_seconds:5,
             goals_per_iter:1000,
             max_iter:50
             }).

phase2_spec(phase2(UnitWeight, DecadeWeight, TransitionWeight)) :-
        UnitWeight         = 10,
        DecadeWeight       = 5,
        TransitionWeight   = 4.
            
iters(phase1, 0).
iters(phase2(_, _, _), 2).

alpha_init(uniform(4)).

run_phase(Which, Options) :-
       iters(Which, Iters),
       exp_constants(Const),
       Const1 = Const.put([max_online_iter=Iters]),
       merge_options(Options, Const1, Options1),
       goals(Which, Goals),
       list_to_categorical(Goals, GoalGen),
       run_online_vbem(GoalGen, Data, Options1).
       
run(Options) :-
        gl_file(GL_FILE),
        compile_gl_file(GL_FILE),
        alpha_init(InitParams), 
        set_rule_alphas(InitParams),
        %% run
        % merge_options(Options, [run_id(phase1)], Options1), 
        % run_phase(phase1, Options1),
        phase2_spec(Phase2),
        merge_options(Options, [run_id(phase2)], Options2), 
        run_phase(Phase2, Options2).
       

main(Options) :-
        run(Options). 

        