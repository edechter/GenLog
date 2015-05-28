

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- getenv('GENLOG_DIR', Dir),
   asserta(file_search_path(genlog, Dir));
   true.
   
        
%% ----------------------------------------------------------------------

:- use_module(library(lists)).

:- use_module(genlog(experiment)).

:- use_module(genlog(sdcl)).
:- use_module(genlog(compile)).

:- use_module(genlog(learn)).
:- use_module(genlog(pgen)).
:- use_module(genlog(pprint)).

:- use_module(genlog(data_utils)).
:- use_module(genlog(number_words)).

:- [number_syllables].
:- [consonants].
:- [vowels].

%% this file
:- absolute_file_name('./syllables.pl', Abs),
   set_setting(experiment:runner, Abs).

%% experiment data directory
:- set_setting(experiment:root, '../data').


%% ------------------------------------------
%% make number data
number_goal(N, Count, Goal) :-
        number_phones(N, Phones),
        Goal = count(hear(Phones-[]), Count).

number_goals(Lo, Hi, Count, Goals) :-
        findall(Goal,
                (between(Lo, Hi, N),
                 number_goal(N, Count, Goal)),
                Goals).

number_phones(N, Phones) :-
       number_syllables(N, Syls), % Syls is list of words, each one is list of triples
       flatten(Syls, Syls1),      % Syls1 is a list of triples
       findall(P,
               (
                member(Trip, Syls1),
                (Trip = (P, _, _);
                 Trip = (_, P, _);
                 Trip = (_, _, P))
               ),
               Phones).

number_phone_lexicon(Phones) :-
        findall(P,
                (between(1, 99, N),
                 number_phones(N, Xs),
                 member(P, Xs)),
                Ps),
        sort(Ps, Phones).

number_phone(Phone) :-
        number_phone_lexicon(Phones),
        member(Phone, Phones).
        


gl_file('../../gls/number_morph.gl').
        
main(Options) :-
        % experiment:setup_experiment,
        gl_file(GlFile),
        compile_sdcl_file(GlFile),
        Options0 = [beam_width(100), time_limit_seconds(2)],
        merge_options(Options, Options0, Options1),
        set_rule_alphas(uniform),
        number_goals(1, 10, 2, Goals1),
        number_goals(10, 20, 1, Goals2),
        append(Goals1, Goals2, Goals),
        list_to_random_choice(Goals, GoalGen),
        run_online_vbem(GoalGen, Data, Options1).      
  

%% ----------------------------------------------------------------------
consonant(C) :-
        consonants(Cs),
        member(C, Cs).

vowel(V) :-
        vowels(Vs),
        member(V, Vs).