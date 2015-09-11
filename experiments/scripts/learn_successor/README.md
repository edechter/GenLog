## README.md

### Experiment:

- model how children learn the successor relation between number words
  in different languages.

----------------------------------------------------------------------


*  9/11/2015
** Experiment 1

commit 757cdbe
PWD: /Users/edechter/Dropbox/Projects/SDCL/experiments/scripts/learn_successor/

Learning successors 1..99 in two phases. 
    1. 1-9, single batch, count 100 each.  
    2. 1-98, single batch, count 100 each. 
       
RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/390cghfpg36dt71/Screenshot%202015-09-11%2011.15.48.png?dl=0

Succeeds in learning up to 19 (with exception of 8-9?) but not beyond. 

:LOGBOOK:

IN: ../gls/succ_04.gl.
num_preds(1, 4).
num_preds(2, 4).

IN: runner.pl
number_word_settings([split_teens(false), irregular_decade(true), split_decade(false)]).

exp_constants(
constants{beam_width:          200,
time_limit_seconds:  5,
init_alpha :         uniform(2),
max_iter   :         10
}
).

phase1(
constants{
goal_generator: GoalGen, 
goals_per_iter: 9,
max_online_iter: 1,
run_id: phase1
}
) :-
succ_goals(1, 9, 100, Goals),
list_to_circular(Goals, GoalGen). 

phase2(
constants{
goal_generator: GoalGen, 
goals_per_iter: 98,
max_online_iter: 1,
run_id: phase2
}
) :-
succ_goals(1, 98, 100, Goals),
list_to_circular(Goals, GoalGen).

phases([phase1, phase2]).

?- run_experiment('runner.pl').
?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), 
asserta(data(D)), 
pairs_keys_values(D.transition_probs, Xs, Ys), 
plot_xy(Xs, Ys), 
show. 
:END:

** Experiment 2
      
    
    
    
