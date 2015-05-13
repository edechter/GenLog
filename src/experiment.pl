/*
  experiment.pl

  This module contains predicates for running a GenLog simulation
  experiment.

  An experiment consists of a runner script RUNNER that contains a
  predicate MAIN.  Running an experiment sets up a directory for it in
  EXPERIMENTS_DIR and automatically creates a README file for it, a
  data directory for it, and a git branch for it. The
  README file contains information about the experiment
  

*/

:- module(experiment,
          [run_experiment/0,
           run_experiment/1
          ]).

:- use_module(library(readutil)).
:- use_module(library(record)).
:- use_module(library(settings)).


%% ----------------------------------------------------------------------
%%
%%       Settings
%%

:- setting(root, atom,   '.', 'The root directory for experiment files and data.').

:- setting(path, atom, 'NONE', 'The path to the experiment directory. Default: <root>/<time>').

:- setting(data_path, atom, 'NONE', 'The path to the data directory. Default: <path>/data').

:- setting(git_hash, atom, 'NONE', 'The current git commit.').

:- setting(runner, atom, 'gl_runner.pl', 'Path to the runner script for the experiment.').

:- setting(genlog_file, atom, 'NONE', 'The .gl file.').

:- setting(datetime, compound, date(none), 'The time this experiment was run.').


        
%% ----------------------------------------------------------------------
%%      run_experiment
%%      run_experiment(+PATH_TO_RUNNER)
%%
%%      Run experiment specified in the runner script at
%%      PATH_TO_RUNNER.
%%
%%      run_experiment will 1) create a commit; 2) generate a
%%      directory experiments_root_dir/1 whose name is the git commit
%%      hash; 3) create a 'config.pl' file with information about the
%%      experiment; 4) create a 'data' subdirectory; 5) execute the
%%      runner script predicate 'main/0'

run_experiment :-
        setting(runner, PATH_TO_RUNNER),
        run_experiment(PATH_TO_RUNNER).

run_experiment(PATH_TO_RUNNER) :-
        \+ exists_file(PATH_TO_RUNNER),
        !,
        throw(error(argument_error, context(run_experiment/1, 'Cannot find experiment runner script'))).
run_experiment(PATH_TO_RUNNER) :-
        load_files([PATH_TO_RUNNER], [module(runner)]),
        setup_experiment,
        print_message(informational, experiment(settings)),
        runner:main.

setup_experiment :-
        get_time(Time),
        stamp_date_time(Time, DateTime, 'local'),
        format_time(atom(TimeLabel), "%F-%T", DateTime),
        set_setting(datetime, DateTime),

        current_git_commit_hash(Hash),
        set_setting(git_hash, Hash),
        
        atomic_list_concat(['exp_', TimeLabel], ExperimentDirectoryName),
        
        setting(root, ExperimentsRoot),
        absolute_file_name(ExperimentsRoot, ExperimentsRootAbs),
        set_setting(root, ExperimentsRootAbs),
        
        atomic_list_concat([ExperimentsRootAbs, '/', ExperimentDirectoryName], ExperimentPath0),
        prolog_to_os_filename(ExperimentPath0, ExperimentPath),

        set_setting(path, ExperimentPath),
       
        make_directory_path(ExperimentPath),
        make_data_dir(ExperimentPath), 
        
        write_readme(ExperimentPath),
        write_config(ExperimentPath),

        print_message(informational, experiment(settings)).

write_readme(Directory) :-
        directory_file_path(Directory, 'README.txt', Path),
        open(Path, write, Stream),
        format(Stream, "README.txt", []),
        close(Stream).

write_config(Directory) :-
        directory_file_path(Directory, 'config.pl', Path),
        save_settings(Path).

make_data_dir(Directory) :-
        atomic_list_concat([Directory, '/', 'data'], DataPath0),
        prolog_to_os_filename(DataPath0, DataPath),
        set_setting(data_path, DataPath),
        (exists_directory(DataPath) -> true
        ;
         make_directory(DataPath)
        ).

current_git_commit_hash(Hash) :-
        tmp_file('t', Tmp),
        format(atom(CMD),
               'git log | head -n1 | sed \'s/commit \\(.*\\)/\\1/g\' > ~w',
               [Tmp]),
        shell(CMD),
        sleep(0.1),
        open(Tmp, read, StreamIn), 
        read_line_to_codes(StreamIn, HashCodes),
        close(StreamIn),
        atom_codes(Hash, HashCodes).




%% ----------------------------------------------------------------------
%%
%%      Messages
%%

:- multifile
	prolog:message/1.

exp_prefix --> ['Experiment Info:     ' -[]].

prolog:message(experiment(settings)) -->
        {findall(N-V,
                (setting(Module:N, V),
                 Module=experiment), 
                NVs)}, 
        exp_prefix, ['Experiment settings ~`.t~70|'-[]], [nl], 
        exp_settings_go_(NVs),
        exp_prefix, ['~`.t~70|'-[]], [nl].

exp_settings_go_([]) --> [].
exp_settings_go_([N-V|NVs]) -->
        exp_prefix, ['~w: ~` t~40| ~w'-[N, V]], [nl],
        exp_settings_go_(NVs).
       
        


