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
commit: 57dd412

Same as Experiment 2, but we increase the number of predicates of type
1 and 2 to 6.  
  
:LOGBOOK:

:-? run_experiment('runner.pl')
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-12-31-39
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-12-31-39/data
% Experiment Info:     git_hash:         57dd4124c496bef3aba4d7541ca2328c672ce54c
% Experiment Info:     start_time:       date(2015,9,11,12,31,39.11435508728027,14400,EDT,true)
% Experiment Info:     random_seed:      12487481...
% Experiment Info:     ...............................................
% Compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl...
% Success! Finished compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl.
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),9,/Users/edechter/data/exp_2015-09-11-12-31-39/data,phase1)
% Online VBEM:     
% Online VBEM:     Running ...
% Online VBEM:     Iter 1 ...


-- Note: After fininshing training, this resulted in exception due to 
bug in run_testing, so manually ran the following.
?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), asserta(data(D)), pairs_keys_values(D.transition_probs, Xs, Ys), plot_xs(Xs, Ys), show. 
:END:

RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/om1mv9j0e4sxgzn/Screenshot%202015-09-11%2012.50.44.png?dl=0

Transition probabilities: 
:LOGBOOK:


Ys = [0.8348960076808607, 0.999253885131731, 0.9986538643591465,
0.9391001523421316, 0.9730483824073327, 0.9449757873672098,
0.9971780000883821, 0.789456843422771, 0.8794408386258151,
0.9975555499135818, 0.9001371398237797, 0.9563340562193792,
0.9967708006390283, 0.9107669206573844, 0.9107669206573844,
0.9975555499135816, 0.9938129656515987, 0.9676153966798274,
0.9885466529654681, 0.012321756209360765, 0.6798276086324226,
0.714234259323295, 0.19138312482816436, 1.0, 0.4844497711816233,
0.7082975563001966, 0.03834661441979709, 0.38073878664534594,
0.040979729320411475, 0.03363982000168605, 0.49319172435950304,
0.32471670217292986, 0.27844446724080346, 0.40828605589618067,
0.34053545494270526, 0.3460329031601364, 0.28049122855693664,
0.3460329031601362, 0.04239924695924115, 4.379478066228747e-30,
0.08495714231458074, 0.2646891617462048, 0.1726765890252186,
0.2646891617462049, 0.1386475674695755, 0.26468916174620505,
0.14575191204811141, 0.13323886722429543, 0.04188196295044259,
0.027229563767386868, 0.22276141974957683, 0.2751776404608424,
0.15728853912227564, 0.6307033801944121, 0.19862554038859268,
0.4129928457132665, 0.159639102622173, 0.07796537924292767,
0.0470196375597008, 0.07354731075302953, 0.21452364368870178,
0.19511861712071873, 0.4417921179746485, 0.16221203124790504,
0.17671018766958807, 0.18011162460971047, 0.22167195518355712,
0.18011162460971064, 0.04319656888495399, 0.02209149117335391,
0.4121078631143307, 0.23961048567634619, 0.015800949271745628,
0.9950544805217981, 0.49135796960265354, 0.6912595968535984,
0.3164101398927246, 0.04142940907041435, 0.043125754669901394,
0.006144407516198805, 0.7525353878604034, 0.34960566437647106,
0.01138228048581305, 0.5229520093492716, 0.3541270945120603,
0.5330225528816955, 0.20313644441381115, 0.5286979095486882,
0.04241986401500388, 0.12496472510159319, 0.25560930800247933,
0.1866832358436482, 0.1339037178817731, 0.4976924636850074,
0.34697614882518835, 0.3482424648918911, 0.13492084502653034]
:END:

Lower but still high probabilities between 1 and 8. Failing
at 19. Failing at 18. Somewhat higher probabilities within
decades.

Some interrogations: 
:LOGBOOK:
?- interact:ask(succ([twenty, one], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 36
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 191
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 176
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 228
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -8.98 ; cond prob: 0.22
% Expl Search:     score: -9.44 ; cond prob: 0.14
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.88 ; cond prob: 0.09
% Expl Search:     score: -10.47 ; cond prob: 0.05
% Expl Search:     score: -10.63 ; cond prob: 0.04
% Expl Search:     score: -11.09 ; cond prob: 0.03
% Expl Search:     
X = [twenty, two],
L = 0.6798276086324226 ;
X = [twenty, nine],
L = 0.10672399591189384 ;
X = [twenty, four],
L = 0.10672399591189384 ;
X = [twenty, six],
L = 0.10672399591189365 a

?- interact:ask(succ([twenty, nine], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 36
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 191
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 176
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 256
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -7.66 ; cond prob: 0.23
% Expl Search:     score: -8.12 ; cond prob: 0.14
% Expl Search:     score: -8.17 ; cond prob: 0.14
% Expl Search:     score: -9.30 ; cond prob: 0.04
% Expl Search:     score: -9.34 ; cond prob: 0.04
% Expl Search:     score: -9.40 ; cond prob: 0.04
% Expl Search:     score: -9.40 ; cond prob: 0.04
% Expl Search:     score: -9.68 ; cond prob: 0.03
% Expl Search:     score: -9.68 ; cond prob: 0.03
% Expl Search:     score: -9.76 ; cond prob: 0.03
% Expl Search:     
X = [twenty, ten],
L = 0.6155793603408802 ;
X = [ninety],
L = 0.04242055518274406 ;
X = [fourty],
L = 0.04145030122645818 ;
X = [thirty],
L = 0.040979729320411495 ;
X = [fifty],
L = 0.04072817537476827 ;
X = [eightty],
L = 0.04060541828246643 ;
X = [sixty],
L = 0.03973425358073265 ;
X = [seventy],
L = 0.03973425358073265 a

:END: 


** Experiment 4:
commit: 3257b17
 Using the saved grammar from Experiment 3 as an initial state, will
 run another phase with count 10.

:LOGBOOK:
%%                               Constants
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
                 goals_per_iter: 98,
                 max_online_iter: 1,
                 run_id: phase1
                }
      ) :-
        succ_goals(1, 98, 10, Goals),
        list_to_circular(Goals, GoalGen). 



?- load_gl('/Users/edechter/data/exp_2015-09-11-12-31-39/data/ovbem_gl_phase2_0001.gl.gz').
[Note: if this file is already ungzipped, needs to be run with just the .gl extension.]

?- run_experiment('runner.pl', run_phase(phase1), []).
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-13-35-16
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-13-35-16/data
% Experiment Info:     git_hash:         3257b17725cd20b43c36c44ca8b924b750e6af3e
% Experiment Info:     start_time:       date(2015,9,11,13,35,16.314450979232788,14400,EDT,true)
% Experiment Info:     random_seed:      12487481...
% Experiment Info:     ...............................................
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),98,/Users/edechter/data/exp_2015-09-11-13-35-16/data,phase1)
% Online VBEM:     

?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), asserta(data(D)), 
pairs_keys_values(D.transition_probs, Xs, Ys), plot_xs(Xs, Ys), show. 

:END:


RESULTS: 
 :LOGBOOK:

D = results{count_probs:[1-0.8390001040147295,
2-0.8373576321805238, 3-0.8363214057107003, 4-0.7870824206275033,
5-0.7752689382151293, 6-0.7350197737518672, 7-0.7330610911768565,
8-0.581211211281657, 9-0.514225571358012, 10-0.5130055664738716,
11-0.4631174440864562, 12-0.44496566864008413, 13-0.4436403734201107,
14-0.4051889537108783, 15-0.3699524873921848, 16-0.36907477172291847,
17-0.3668618352208935, 18-0.35524892000448166, 19-0.3514388265156975,
20-0.004441083145107802, 21-0.003250291733076483,
22-0.0021411009419126095, 23-0.00041963131897050003,
24-0.00034625175147086926, 25-0.00023549202155979793,
26-0.0001700935313980902, 27-6.3873511412007796e-6,
28-2.727629476290606e-6, 29-1.0660151696264009e-7,
30-3.504429358041355e-9, 31-1.8052367825100212e-9,
32-7.495513433538626e-10, 33-2.1068453810675387e-10,
34-8.95179249707476e-11, 35-3.248904344030328e-11,
36-1.1978453352637196e-11, 37-3.3989698357059276e-12,
38-1.4364403974915184e-12, 39-5.80937116945751e-14,
40-2.2639843503918793e-43, 41-5.166161113534382e-44,
42-1.029698285790497e-44, 43-2.198151531211833e-45,
44-4.254170635066141e-46, 45-2.4006025861660414e-47,
46-4.7847837369289404e-48, 47-8.657268415343265e-49,
48-1.0996442268987749e-49, 49-4.3815662103330167e-51,
50-1.1599955302723025e-52, 51-2.6683299433157714e-53,
52-8.430961917286475e-54, 53-1.3637536486869197e-54,
54-8.890723760615465e-55, 55-2.465904723445398e-55,
56-1.0831080774659141e-55, 57-1.757298807080553e-56,
58-2.2156040565149053e-57, 59-9.875815063469617e-59,
60-7.071767713946587e-60, 61-2.654368844455653e-60,
62-1.0569900639275715e-60, 63-4.77814626298516e-61,
64-1.4491087960378401e-61, 65-4.11461032924732e-62,
66-1.168305527358529e-62, 67-3.2275348183031876e-63,
68-9.16428644813009e-64, 69-3.4511425411091973e-65,
70-7.66370696744935e-67, 71-3.950927217951057e-67,
72-1.0963215384695047e-67, 73-1.6746290758766072e-69,
74-1.6746290758766072e-69, 75-1.1979915973076806e-69,
76-8.465719175205023e-70, 77-2.7124702671376968e-70,
78-8.162963786257057e-71, 79-3.340502672832985e-72,
80-2.0466739362264267e-74, 81-1.5768067718338626e-74,
82-6.050405444432932e-75, 83-6.4595366494724035e-77,
84-3.3708587343020917e-77, 85-1.8186430522754376e-77,
86-1.0120247650543928e-77, 87-2.0177959873903926e-78,
88-1.1387545584035195e-78, 89-4.59819006645908e-80,
90-5.629954302209654e-81, 91-1.6199786134397618e-81,
92-2.857090514106272e-82, 93-3.898099778575489e-83,
94-2.5033354264029265e-83, 95-9.82196980108112e-84,
96-3.6197289650391064e-84, 97-5.04012635721577e-85],
transition_probs:[1-0.8390001040147295, 2-0.9980423460898918,
3-0.9987625042992382, 4-0.9411243276245523, 5-0.9849907937176952,
6-0.9480836101135096, 7-0.9973351974396378, 8-0.7928550816257078,
9-0.8847481971727081, 10-0.9976274908287458, 11-0.9027532532827667,
12-0.9608052435118737, 13-0.9970215787118503, 14-0.9133275012533173,
15-0.9130369522762547, 16-0.9976274908287457, 17-0.9940040970785011,
18-0.9683452621627444, 19-0.9892748625703462, 20-0.01263685970368853,
21-0.7318691469798154, 22-0.658741158562405, 23-0.19598857333445027,
24-0.8251332439159782, 25-0.6801179215973158, 26-0.7222899963721223,
27-0.0375519932398352, 28-0.42703609305215523,
29-0.039082110634620006, 30-0.03287410402677036,
31-0.5151300249119526, 32-0.41520943436111357, 33-0.28108086253844505,
34-0.42489081436716, 35-0.3629333840224732, 36-0.3686920907550542,
37-0.2837569872873116, 38-0.42261051639876823,
39-0.040442827837496906, 40-3.897124635958309e-30,
41-0.22818890566271613, 42-0.19931594527566684,
43-0.21347530257606645, 44-0.1935340023042375,
45-0.056429391110418364, 46-0.19931594527566643,
47-0.18093332721657834, 48-0.12701976814648336,
49-0.03984530726533202, 50-0.026474449422599005,
51-0.23002932974141682, 52-0.31596399607200865,
53-0.16175540372098454, 54-0.6519303372112576, 55-0.27735702849850935,
56-0.4392335466848775, 57-0.1622459331290377, 58-0.1260800979086617,
59-0.044573916690710744, 60-0.07160692731180104,
61-0.37534728964878744, 62-0.39820768170006704, 63-0.4520521456209805,
64-0.30327845073802856, 65-0.28394074623641136, 66-0.2839407462364111,
67-0.2762577718518936, 68-0.28394074623641186,
69-0.037658606162549395, 70-0.02220628929741695,
71-0.5155373547986808, 72-0.27748462018949943,
73-0.015274981080955829, 74-1.0, 75-0.7153772823874897,
76-0.706659311653817, 77-0.32040636016868657, 78-0.30094205585047507,
79-0.04092266926942594, 80-0.00612684418088102, 81-0.7704240250115826,
82-0.383712548202477, 83-0.010676204609421537, 84-0.5218421873304819,
85-0.5395192132404719, 86-0.5564724555421552, 87-0.19938207611766726,
88-0.5643556462198471, 89-0.04037911446787556, 90-0.12243848603120278,
91-0.28774276423592815, 92-0.17636594029100816, 93-0.1364359917660801,
94-0.6421937786614941, 95-0.39235532312161736, 96-0.3685339130894779,
97-0.13924043501310376]}
:END:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/xo1exvfo30aklqz/Screenshot%202015-09-11%2013.58.30.png?dl=0

Slight improvements across the board.

** Experiment 5: 

Same Experiment 4, but count 100. That is, start with grammar learned
after experiment 3, and train on numbers 1-98 for a one batch 100
count.



