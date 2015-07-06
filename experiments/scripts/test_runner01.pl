/*
  test_runner01.pl
  --------------------------------------------------

  small test case for genlog
  
*/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- getenv('GENLOG_ROOT', Dir),
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



gl_file('../../gls/test01.gl').
        
main(Options) :-
        % experiment:setup_experiment,
        gl_file(GlFile),
        compile_sdcl_file(GlFile),
        Options0 = [beam_width(50), time_limit_seconds(20)],
        merge_options(Options, Options0, Options1),
        set_rule_alphas(uniform),
        number_goals(1, 1, 1, Goals1),
        number_goals(21, 21, 1, Goals2),
        append(Goals1, Goals2, Goals),
        list_to_random_choice(Goals, GoalGen),
        run_online_vbem(GoalGen, Data, Options1).

%% ----------------------------------------------------------------------
%%    analyze
analyze1(GlFile, Ls) :-
        load_gl(GlFile),
        number_goals(1, 20, 1, Goals),
        prove_goals(Goals, Ds, [beam_width(100), time_limit_seconds(20.0)]), 
        findall(L,
                (member(D, Ds), 
                 loglikelihood(D, L)),
                Ls).

analyze(Dir, LoglikelihoodData) :-
        absolute_file_name(Dir, Path),
        directory_files(Path, Files),
        findall(F, 
                (member(F, Files),
                 atom_prefix(F, 'ovbem_gl'),
                 file_name_extension(_, 'gl', F)),
                Files0),
        length(Files1, 10), 
        append(Files1, _, Files0),
        writeln(Files1),
        retractall(worked(_)),
        findall(Ls,
                (member(F, Files1),
                 directory_file_path(Path, F, P),
                 (analyze1(P, Ls) ->
                  assert(worked(P))
                 ;
                  throw(error('FAIL'))
                 )
                ),
                LoglikelihoodData),
        length(LoglikelihoodData, N).
        
%% ----------------------------------------------------------------------
consonant(C) :-
        consonants(Cs),
        member(C, Cs).

vowel(V) :-
        vowels(Vs),
        member(V, Vs).