#!/usr/bin/env swipl          

:- use_module(experiment).

:- initialization main.


run :-
        current_prolog_flag(argv, [Runner|_]),
        experiment:run_experiment(Runner).

main :-
        catch(run, E, (print_message(error, E), fail)),
        halt.
main :-
        halt(1).


