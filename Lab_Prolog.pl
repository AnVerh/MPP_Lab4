% Предикат для генерації чисельного ряду
generateSeries(Size, Series) :-
 length(Series, Size),
 maplist(random(1, 1000), Series).
% Предикат для сортування чисельного ряду
sortSeries(Series, SortedSeries) :-
 sort(Series, SortedSeries).
% Предикат для виведення чисельного ряду
printSeries([]).
printSeries([X|Xs]) :-
 write(X), write(' '),
 printSeries(Xs).

% Генерування випадкового числа для бета розподілу
random_beta(BetaResult) :-
    Alpha is 2,
    %Beta is 5,
    random(R),
    BetaResult is R^(1/Alpha).

% Ділення ряду чисел на інтервали за бета розподілом
split_into_intervals(SortedSeries, Intervals, UppercaseAlphabet) :-
    min_list(SortedSeries, Min),
    max_list(SortedSeries, Max),
    %write('Max='), write(Max), nl,
    IntervalSize is (Max - Min) / Intervals,
    %write('Interval size='), write(IntervalSize), nl,
    split_into_intervals_helper(Min, Max, IntervalSize, Intervals, UppercaseAlphabet).

split_into_intervals_helper(_, _, _, 0, _).
split_into_intervals_helper(Start, Max, IntervalSize, Intervals, [H|T]) :-
    random_beta(Beta),
    %write('Beta='), write(Beta), nl,
    RemainingIntervals is Intervals - 1,
    RemainingDistance is Max - Start,
    (RemainingIntervals = 0 ->
        LastIntervalSize = RemainingDistance
    ;
        LastIntervalSize is min(IntervalSize * Beta, RemainingDistance / RemainingIntervals)
    ),
    End is Start + LastIntervalSize,
    %write('End='), write(End), nl,
    format('[~w - ~w]: ~w~n', [Start, End, H]),
    NextStart is End,
    NextIntervals is Intervals - 1,
    split_into_intervals_helper(NextStart, Max, IntervalSize, NextIntervals, T).
 
% Формування лінгвістичного ряду
alphabet_series(Series, Size, Intervals, UppercaseAlphabet, AlphabetSeries) :-
 min_list(Series, Min),
 max_list(Series, Max),
 findall(X, (between(1, Size, I), nth1(I, Series, Elem), find_interval(Elem, Min, 
Max, Intervals, Index), nth1(Index, UppercaseAlphabet, X)), AlphabetSeries).

find_interval(Elem, Min, Max, Intervals, Index) :-
 IntervalSize is (Max - Min) / Intervals,
 find_interval_helper(Elem, Min, IntervalSize, 1, Intervals, Index).
find_interval_helper(Elem, Start, IntervalSize, CurrentIndex, Intervals, Index) :-
 End is Start + IntervalSize,
 (Elem =< End -> Index = CurrentIndex ; NextStart is End, NextIndex is 
CurrentIndex + 1, find_interval_helper(Elem, NextStart, IntervalSize, NextIndex, 
Intervals, Index)).

% Предикат для виведення символьного ряду
print_char_series([]) :- nl.
print_char_series([H|T]) :-
 write(H), write(' '),
 print_char_series(T).
 
% Предикат для обчислення матриці передування
count_transitions(AlphabetSeries, Matrix) :-
 count_transitions_helper(AlphabetSeries, [], Matrix).
count_transitions_helper([_], Matrix, Matrix).
count_transitions_helper([H1, H2|T], TempMatrix, Matrix) :-
 ( select([H1, H2, X], TempMatrix, Rest)
 -> NewX is X + 1,
 NewTempMatrix = [[H1, H2, NewX]|Rest]
 ; NewTempMatrix = [[H1, H2, 1]|TempMatrix]
 ),
 count_transitions_helper([H2|T], NewTempMatrix, Matrix).
% Предикат для виведення матриці передування
print_matrix(Matrix, INTERVALS) :-
 write('\t'),
 forall(between(1, INTERVALS, N), (Char is N + 64, char_code(C, Char), 
write(C), write('\t'))), nl,
 forall(between(1, INTERVALS, N1),
 (Char1 is N1 + 64, char_code(C1, Char1), write(C1), write(':\t'),
 forall(between(1, INTERVALS, N2),
 (Char2 is N2 + 64, char_code(C2, Char2),
 ( member([C1, C2, X], Matrix) -> write(X) ; write(0) ),
 write('\t'))),
 nl)).
% Головний предикат
main :-
 SIZE is 100, % Розмір масиву чисел
 INTERVALS is 26, % Кількість інтервалів
 UppercaseAlphabet = 
['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'
],
 generateSeries(SIZE, Series),
 sortSeries(Series, SortedSeries),
 write('Початковий ряд:\n'),
 printSeries(Series),
 nl,
 write('\nВідсортований ряд:\n'),
 printSeries(SortedSeries),
 nl,
write('\nІнтервали:\n'),
% print_intervals(Series, SortedSeries, SIZE, INTERVALS, UppercaseAlphabet),
split_into_intervals(SortedSeries, 3, UppercaseAlphabet),
alphabet_series(Series, SIZE, INTERVALS, UppercaseAlphabet, AlphabetSeries),
write('\nЛінгвістичний ряд:\n'),
 print_char_series(AlphabetSeries),
 count_transitions(AlphabetSeries, Matrix),
 write('\nМатриця передування:\n'),
 print_matrix(Matrix, INTERVALS).
% Предикат для вимірювання часу виконання
time_main :-
 statistics(runtime, [Start|_]),
 main,
 statistics(runtime, [Stop|_]),
 Runtime is Stop - Start,
 format('\nЧас виконання програми: ~3d мс.~n', [Runtime]).
% Виклик предикату для вимірювання часу виконання
:- initialization(time_main).
