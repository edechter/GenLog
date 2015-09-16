

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

% add genlog project root directory to search path
:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', src], Src),
   asserta(user:file_search_path(genlog, Src)),
   !
   ;
   absolute_file_name('../../src', Src), 
   asserta(user:file_search_path(genlog, Src)).


:- use_module(genlog(compile)).
:- use_module(genlog(interact)).


/*
  On my machine, invoking crash results in the following crash:

  ----
  
SWI-Prolog [thread 1]: received fatal signal 11 (segv)
C-stack trace labeled "crash":
  [0] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(save_backtrace+0xb9) [0x101b182d9]
  [1] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(crashHandler+0x3d) [0x101b1882d]
  [2] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(dispatch_signal+0x3f7) [0x101abe2c7]
  [3] /usr/lib/system/libsystem_platform.dylib(_sigtramp+0x1a) [0x7fff8cd7f5aa]
  [5] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(analyseVariables2+0x581) [0x101a7dc11]
  [6] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(analyseVariables2+0x1ea) [0x101a7d87a]
  [7] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(compileClause+0x24b) [0x101a6da4b]
  [8] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(PL_next_solution+0xd7c7) [0x101a52867]
  [9] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(query_loop+0x13a) [0x101aa125a]
  [10] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(prologToplevel+0x51) [0x101aa1871]
  [11] /Users/edechter/Dropbox/Projects/swipl-all/lib/swipl-7.3.6/lib/x86_64-darwin13.2.0/libswipl.dylib(PL_toplevel+0x38) [0x101a5f3c8]
  [12] /usr/local/bin/swipl(main+0x35) [0x101a35f05]
  [13] /usr/lib/system/libdyld.dylib(start+0x1) [0x7fff8fcfe5fd]

  ---
*/
crash :-
        compile_gl_file(gl('crash.gl')),
        ask('A(t=1, i=1)'([] | [one]), L, [beam_width(10), time_limit_seconds(2)]).


