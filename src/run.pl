#!/usr/bin/env swipl          

:- use_module(experiment).

:- initialization main.


run :-
        current_prolog_flag(argv, [Runner, DataPath|_]),
        experiment:run_experiment(Runner, [data_path(DataPath)]).

main :-
        catch(run, E, (print_message(error, E), fail)),
        halt.
main :-
        halt(1).


