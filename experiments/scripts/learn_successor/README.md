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

Code:     

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
       
RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://ww w.dropbox.com/s/390cghfpg36dt71/Screenshot%202015-09-11%2011.15.48.png?dl=0

Succeeds in learning up to 19 (with exception of 8-9?) but not beyond. 

Some sample interrogations:

:LOGBOOK:
?- interact:ask(succ([twenty, one], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 16
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 164
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 162
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 200
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -9.72 ; cond prob: 0.22
% Expl Search:     score: -9.81 ; cond prob: 0.20
% Expl Search:     score: -10.66 ; cond prob: 0.09
% Expl Search:     score: -10.66 ; cond prob: 0.09
% Expl Search:     score: -10.75 ; cond prob: 0.08
% Expl Search:     score: -10.75 ; cond prob: 0.08
% Expl Search:     score: -10.82 ; cond prob: 0.07
% Expl Search:     score: -11.05 ; cond prob: 0.06
% Expl Search:     score: -11.15 ; cond prob: 0.05
% Expl Search:     score: -11.68 ; cond prob: 0.03
% Expl Search:     
X = [two, two],
L = 0.5353574252404573 ;
X = [two, one],
L = 0.16544635118254014 ;
X = [one, two],
L = 0.16544635118254014 ;
X = [twenty, two],
L = 0.07407287853599932 ;
X = [two, thirty],
L = 0.05967699385846397 ;
X = [two, seventy],
L = 7.962693151529136e-28 a

?- interact:ask(succ([twenty, nine], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 16
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 406
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 402
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 612
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -11.81 ; cond prob: 0.03
% Expl Search:     score: -11.87 ; cond prob: 0.03
% Expl Search:     score: -11.87 ; cond prob: 0.03
% Expl Search:     score: -11.90 ; cond prob: 0.03
% Expl Search:     score: -11.93 ; cond prob: 0.03
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -12.02 ; cond prob: 0.02
% Expl Search:     
X = [one, ten],
L = 0.07632984947858547 ;
X = [ten, ten],
L = 0.07182420775752052 ;
X = [one, one],
L = 0.05562747643834421 ;
X = [twenty, ten],
L = 0.05555964828993593 ;
X = [ten, one],
L = 0.05234386615494547 ;
X = [one, eightty],
L = 0.04918998960381789 ;
X = [ten],
L = 0.04815839207757795 a

?- interact:ask(succ([threeteen], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 16
% Expl Search:     Beam Size: 80
% Expl Search:     Beam Size: 64
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
1441985913.558166-1441985907.452394>=6
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 1574
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -5.27 ; cond prob: 0.77
% Expl Search:     score: -7.04 ; cond prob: 0.13
% Expl Search:     score: -8.39 ; cond prob: 0.03
% Expl Search:     score: -9.30 ; cond prob: 0.01
% Expl Search:     score: -10.16 ; cond prob: 0.01
% Expl Search:     score: -10.54 ; cond prob: 0.00
% Expl Search:     score: -12.01 ; cond prob: 0.00
% Expl Search:     score: -12.16 ; cond prob: 0.00
% Expl Search:     score: -12.62 ; cond prob: 0.00
% Expl Search:     score: -12.62 ; cond prob: 0.00
% Expl Search:     
X = [fourteen],
L = 0.9535470602505295 ;
X = [fourteen, fiveteen, fiveteen],
L = 0.002311466527629369 ;
X = [fourteen, fourteen, fourteen],
L = 0.0016241285049746188 ;
 X = [fourteen, two],
L = 0.001390261917832639 a

:END: 






** Experiment 2

Same as Experiment 1, but we increase the number of predicates of type
1 and 2 to 5.


      
    
    
    
