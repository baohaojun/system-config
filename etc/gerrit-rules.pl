% -*- Mode: Prolog -*-

submit_rule(submit(ARR, CR, V)) :-
    all_done_and_go('All-Reviewer-Required', ARR),
    gerrit:max_with_block(-2, 2, 'Code-Review', CR),
    gerrit:max_with_block(-1, 1, 'Verified', V).

% 如果是mol线的话，就使用原来的逻辑
submit_rule(submit(CR, V)) :-
    gerrit:change_branch(B), regex_matches('refs/heads/mol/.*', B),
    gerrit:max_with_block(-2, 2, 'Code-Review', CR),
    gerrit:max_with_block(-1, 1, 'Verified', V).

% 如果被 153 （cmbuild） Review +2 的话，也使用原来的逻辑（直接可以进了）
submit_rule(submit(CR, V)) :-
    CR = label('Code-Review', ok(user(153))),
    gerrit:max_with_block(-2, 2, 'Code-Review', CR),
    gerrit:max_with_block(-1, 1, 'Verified', V).

all_done_and_go(Category, label(Category, ok(U))) :-
    gerrit:current_user(U),
    findall(Score, score('Code-Review', Score), All),
    all_done_list(All),
    one_plus2_list(All),
    !.

all_done_and_go(Category, label(Category, need(_))).

score(Category, Score) :-
    gerrit:commit_label(label(Category, Score), User).

all_done_list([]).
all_done_list([X|T]) :- X \= 0, all_done_list(T).

one_plus2_list([]) :- false.
one_plus2_list([X|T]) :- X = 2; one_plus2_list(T).

