submit_rule(submit(CR, V)) :-
    all_done_and_go('Code-Review', CR),
    gerrit:max_with_block(-1, 1, 'Verified', V).

all_done_and_go(Category, label(Category, ok(U))) :-
    gerrit:current_user(U),
    findall(Score, score(Category, Score), All),
    all_done_list(All),
    one_plus2_list(All),
    !.

all_done_and_go(Category, label(Category, need(_))).

score(Category, Score) :-
    gerrit:commit_label(label(Category, Score), User).

% Simple Prolog routine to sum a list of integers.
all_done_list([]).
all_done_list([X|T]) :- X \= 0, all_done_list(T).

one_plus2_list([]) :- false.
one_plus2_list([X|T]) :- X = 2; one_plus2_list(T).

