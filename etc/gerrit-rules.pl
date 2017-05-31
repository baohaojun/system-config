% -*- Mode: Prolog -*-

submit_rule(R) :-
    res_rule(R).

submit_rule(R) :-
    non_res_rule(R).

non_res_rule(submit(R, CR, V)) :-
    \+ contains_res_change(),
    gerrit:current_user(U),
    R = label('Shit', reject(U)),
    gerrit:max_with_block(-2, 2, 'Code-Review', CR),
    gerrit:max_with_block(-1, 1, 'Verified', V).

res_rule(submit(CR, V)) :-
    contains_res_change(),
    CR = label('Code-Review', ok(user(153))),
    gerrit:max_with_block(-2, 2, 'Code-Review', CR),
    gerrit:max_with_block(-1, 1, 'Verified', V).

contains_res_change() :-
    gerrit:commit_delta('core/res/res/.*');
    gerrit:commit_delta('core/java/android/provider/Settings.java$').



