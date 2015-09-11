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
https://www.dropbox.com/s/390cghfpg36dt71/Screenshot%202015-09-11%2011.15.48.png?dl=0

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
commit: 9043f16

Same as Experiment 1, but we increase the number of predicates of type
1 and 2 to 5.

Experiment log: 
:LOGBOOK:
?- run_experiment('runner.pl') 

% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-11-48-15
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-11-48-15/data
% Experiment Info:     git_hash:         9043f16f1d2010f7e8dff820185811918a229630
% Experiment Info:     start_time:       date(2015,9,11,11,48,15.87036395072937,14400,EDT,true)
% Experiment Info:     random_seed:      12487481...
% Experiment Info:     ...............................................
% Compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl...
% Success! Finished compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl.
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),9,/Users/edechter/data/exp_2015-09-11-11-48-15/data,phase1)
% Online VBEM:     
...

-- Note: After fininshing training, this resulted in exception due to 
bug in run_testing, so manually ran the following.
?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), asserta(data(D)), pairs_keys_values(D.transition_probs, Xs, Ys), plot_xs(Xs, Ys), show. 
:END:

RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/c18d11f09ftd45q/Screenshot%202015-09-11%2012.19.04.png?dl=0

Much higher probabilities betweeen 1 and 10. Failing at 18. Slightly
higher probability at the decade transitions vs within decade,
probably because the result is of length 1, not 2. 

:LOGBOOK:
Some interrogations: 
interact:ask(succ([twenty, one], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 189
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 169
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
1441988585.719006-1441988580.615021>=5
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 364
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -8.24 ; cond prob: 0.33
% Expl Search:     score: -9.18 ; cond prob: 0.13
% Expl Search:     score: -9.19 ; cond prob: 0.13
% Expl Search:     score: -9.47 ; cond prob: 0.10
% Expl Search:     score: -9.75 ; cond prob: 0.07
% Expl Search:     score: -9.76 ; cond prob: 0.07
% Expl Search:     score: -10.41 ; cond prob: 0.04
% Expl Search:     score: -10.42 ; cond prob: 0.04
% Expl Search:     score: -10.62 ; cond prob: 0.03
% Expl Search:     score: -10.71 ; cond prob: 0.03
% Expl Search:     
X = [twenty],
L = 0.3275453659450952 ;
X = [one],
L = 0.20050252435539187 ;
X = [thirty],
L = 0.1991315583589306 ;
X = [twenty, two],
L = 0.1260392185553493 ;
X = [one, two],
L = 0.04930934659968724 ;
X = [thirty, two],
L = 0.04897218656784609 a

?-  interact:ask(succ([twenty, nine], X), L, [beam_width(200), time_limit_seconds(3)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,3)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 189
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 169
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
1441988698.817007-1441988695.658908>=3
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 364
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -9.37 ; cond prob: 0.33
% Expl Search:     score: -10.31 ; cond prob: 0.13
% Expl Search:     score: -10.32 ; cond prob: 0.13
% Expl Search:     score: -10.60 ; cond prob: 0.10
% Expl Search:     score: -10.88 ; cond prob: 0.07
% Expl Search:     score: -10.89 ; cond prob: 0.07
% Expl Search:     score: -11.54 ; cond prob: 0.04
% Expl Search:     score: -11.55 ; cond prob: 0.04
% Expl Search:     score: -11.75 ; cond prob: 0.03
% Expl Search:     score: -11.84 ; cond prob: 0.03
% Expl Search:     
X = [twenty],
L = 0.3275453659450952 ;
X = [one],
L = 0.20050252435539187 ;
X = [thirty],
L = 0.1991315583589306 ;
X = [twenty, ninety],
L = 0.09562235507186667 a

?-  interact:ask(succ([twenty, nine], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 449
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 427
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
1441988720.387642-1441988713.571632>=6
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 224
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -9.37 ; cond prob: 0.38
% Expl Search:     score: -10.31 ; cond prob: 0.15
% Expl Search:     score: -10.32 ; cond prob: 0.15
% Expl Search:     score: -10.88 ; cond prob: 0.08
% Expl Search:     score: -10.89 ; cond prob: 0.08
% Expl Search:     score: -11.34 ; cond prob: 0.05
% Expl Search:     score: -11.61 ; cond prob: 0.04
% Expl Search:     score: -11.97 ; cond prob: 0.03
% Expl Search:     score: -12.31 ; cond prob: 0.02
% Expl Search:     score: -12.43 ; cond prob: 0.02
% Expl Search:     
X = [twenty],
L = 0.4266529899990143 ;
X = [one],
L = 0.23158176649118545 ;
X = [thirty],
L = 0.23033327322929043 ;
X = [ninety],
L = 0.09336682690778347 

?-  interact:ask(succ([threeteen], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 150
% Expl Search:     Beam Size: 125
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
1441988808.66424-1441988801.602493>=6
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 616
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -5.47 ; cond prob: 0.60
% Expl Search:     score: -6.84 ; cond prob: 0.15
% Expl Search:     score: -7.42 ; cond prob: 0.09
% Expl Search:     score: -7.61 ; cond prob: 0.07
% Expl Search:     score: -8.76 ; cond prob: 0.02
% Expl Search:     score: -9.00 ; cond prob: 0.02
% Expl Search:     score: -9.30 ; cond prob: 0.01
% Expl Search:     score: -10.12 ; cond prob: 0.01
% Expl Search:     score: -10.17 ; cond prob: 0.01
% Expl Search:     score: -10.28 ; cond prob: 0.00
% Expl Search:     
X = [fourteen],
L = 0.979071581598065 ;
X = [fourteen, fourteen],
L = 0.006726150200643701 ;
X = [fourteen, fiveteen],
L = 0.0048548646150131875 ;
X = [fourteen, fourteen, fourteen],
L = 0.0011798535287663799 ;
X = [fourteen, fiveteen, fiveteen],
L = 0.0003933943137764275 a

:END:
      
** Experiment 3:

Same as Experiment 2, but we increase the number of predicates of type
1 and 2 to 6.    
    

