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

:- setting(root, atom,   '.', 'The root directory for experiment files and data.').
:- setting(git_hash, atom, 'NONE', 'The current git commit.').
:- setting(runner, atom, 'gl_runner.pl', 'Path to the runner script for the experiment.').
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
        make_directory_path(ExperimentPath),
        
        write_readme(ExperimentPath),
        write_config(ExperimentPath).



write_readme(Directory) :-
        directory_file_path(Directory, 'README.txt', Path),
        writeln(Path), 
        open(Path, write, Stream),
        format(Stream, "README.txt", []),
        close(Stream).

write_config(Directory) :-
        directory_file_path(Directory, 'config.pl', Path),
        save_settings(Path).

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


        
        
         