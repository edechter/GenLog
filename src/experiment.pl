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


:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- getenv('GENLOG_DIR', Dir),
   atomic_list_concat([Dir, '/', src], Src),
   asserta(user:file_search_path(genlog, Src))
   ;
   true.


:- use_module(library(readutil)).
:- use_module(library(record)).
:- use_module(library(settings)).

%% ----------------------------------------------------------------------
%%
%%       Settings
%%

:- setting(root, atom,   '.', 'The root directory for experiment files and data.').
:- setting(runner, atom,   none, 'Script for running experiment').

%% ----------------------------------------------------------------------
%%
%%      Config
%%

:- record config(
                 %% The path to the experiment directory. Default: <root>/<time>.
                 path:atom,

                 %% Data directory, where data files will be saved.
                 %% Default: <path>/data
                 data_path:atom,

                 %% The current git commit when experiment was run.
                 git_hash:atom,

                 %% The datetime experiment was initiated.
                 start_time,

                 %% The random seed
                 %% Default: the system's current random seed
                 random_seed
                 ).

init_config(Fields, Config) :-
        init_config_(Config0), 
        set_config_fields(Fields, Config0, Config).

init_config_(Config) :-
        default_config(Config0), 
        init_config_(Config0, Config).

init_config_ -->
        {current_datetime(DateTime)}, 
        init_config_path(DateTime),
        init_config_data_path,
        init_config_git_hash,
        init_config_start_time(DateTime),
        init_config_random_seed.

init_config_path(DateTime, Config, Config1) :-
        timelabel(DateTime, TimeLabel),
        atomic_list_concat(['exp_', TimeLabel], ExperimentDirectoryName),
        
        setting(root, ExperimentsRoot),
        absolute_file_name(ExperimentsRoot, ExperimentsRootAbs),
        
        atomic_list_concat([ExperimentsRootAbs, '/', ExperimentDirectoryName], ExperimentPath0),
        prolog_to_os_filename(ExperimentPath0, ExperimentPath),

        set_path_of_config(ExperimentPath, Config, Config1).

init_config_data_path(Config, Config1) :-
        config_path(Config, Directory), 
        atomic_list_concat([Directory, '/', 'data'], DataPath0),
        prolog_to_os_filename(DataPath0, DataPath),
        set_data_path_of_config(DataPath, Config, Config1).

init_config_git_hash(Config, Config1) :-
        current_git_commit_hash(Hash),
        set_git_hash_of_config(Hash, Config, Config1).

init_config_start_time(DateTime, Config, Config1) :-
        set_start_time_of_config(DateTime, Config, Config1).

init_config_random_seed(Config, Config1) :-
        random_property(state(Seed)),
        set_random_seed_of_config(Seed, Config, Config1).
        
require_runner :-
        setting(runner, Runner),
        (Runner = none ->
         throw(error(existence_error, context(init_config/2, 'No runner script specified.')))
        ;
         true).
         

        


        
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
%%      runner script predicate 'main/1'

run_experiment :-
        require_runner,
        setting(runner, RUNNER), 
        run_experiment(RUNNER).

run_experiment(PATH_TO_RUNNER) :-
        \+ exists_file(PATH_TO_RUNNER),
        !,
        throw(error(argument_error, context(run_experiment/1, 'Cannot find experiment runner script.'))).
run_experiment(PATH_TO_RUNNER) :-
        set_setting(runner, PATH_TO_RUNNER),
        load_files([PATH_TO_RUNNER], [module(runner)]),
        setup_experiment(Config),
        config_data_path(Config, DataPath), 
        Options = [save_dir(DataPath)],
        call(runner:main, Options).

current_datetime(DateTime) :- 
        get_time(Time),
        stamp_date_time(Time, DateTime, 'local').

timelabel(DateTime, TimeLabel) :-
        format_time(atom(TimeLabel), "%F-%H-%M-%S", DateTime).

setup_experiment(Config) :-
        setup_experiment([], Config).

setup_experiment(Fields, Config) :-
        require_runner,

        init_config(Fields, Config),

        config_path(Config, ExperimentPath),
        config_data_path(Config, DataPath),
       
        make_directory_path(ExperimentPath),
        make_data_dir(DataPath), 

        write_readme(ExperimentPath),
        write_config(Config, ExperimentPath),

        print_message(informational, experiment(settings)), 
        print_message(informational, experiment(config(Config))).

write_readme(Directory) :-
        directory_file_path(Directory, 'README.txt', Path),
        open(Path, write, Stream),
        format(Stream, "README.txt", []),
        close(Stream).

write_config(Config, Directory) :-
        directory_file_path(Directory, 'settings.pl', SettingsPath),
        save_settings(SettingsPath),
        directory_file_path(Directory, 'config.pl', ConfigPath),
        save_config(Config, ConfigPath).
        

make_data_dir(DataPath) :-
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

save_config(Config, Path) :-
        tell(Path),
        findall(_,
                (config_data(N, Config, V),
                 T =.. [N, V],
                portray_clause(T)),
                _),
        told.
                 
         



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
       
        
prolog:message(experiment(config(Config))) -->
        {findall(N-V,
                config_data(N, Config, V), 
                NVs)}, 
        exp_prefix, ['Experiment config ~`.t~70|'-[]], [nl], 
        exp_config_go_(NVs),
        exp_prefix, ['~`.t~70|'-[]], [nl].

exp_config_go_([]) --> [].
exp_config_go_([random_seed-V|NVs]) -->
        !, 
        {random_seed_short_atom_(V, A)}, 
        exp_prefix, ['random_seed: ~` t~40| ~w...'-[A]], [nl],
        exp_config_go_(NVs).
exp_config_go_([N-V|NVs]) -->
        exp_prefix, ['~w: ~` t~40| ~w'-[N, V]], [nl],
        exp_config_go_(NVs).

random_seed_short_atom_(Seed, Atom) :-
        number_codes(Seed, Codes),
        length(Xs, 8), 
        append(Xs, _, Codes),
        atom_codes(Atom, Xs).


