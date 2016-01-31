%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project Declarative Programming %
%           Jens Nevens           %
%                                 %
%     find_heuristically(-S)      %
%    find_heuristically(-S,+T)    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(find_heuristic, [find_heuristically/1,
                           find_heuristically/2]).

%:- use_module(small_instance).
%:- use_module(large_short_instance).
%:- use_module(large_long_instance).

:- use_module(utils).
:- use_module(valid).
:- use_module(cost).

%find_heuristically(-Schedule)
%       -Schedule: a schedule/1 functor that contains a list
%           of event/4 functors
%
%       Calls find_heuristically/2. The result of this is
%       passed on.
find_heuristically(schedule(Events)) :-
    find_heuristically(schedule(Events),100).

% find_heuristically(-Schedule,+Time)
%       -Schedule: a schedule/1 functor that contains a list
%           of event/4 functors
%       +Time: Number of seconds this predicate can take.
%
%       The current time is bound to a variable. A valid 
%       schedule, together with its cost, is generated and 
%       duplicated 3 times. The outer_loop/4 predicate
%       tries to modify these schedules to find a better one,
%       it wil do this until Time is over.
find_heuristically(schedule(Events),Time) :-
    get_time(StartTime),
    is_valid(schedule(InitEvents)),
    cost(schedule(InitEvents), InitCost),
    duplicate(node(InitEvents, InitCost), 3, NodeList),
    outer_loop(NodeList, StartTime, Time, Events),
    !.

% outer_loop(+NodeList, +StartTime, +Time, -Events)
%       +NodeList: List of node/2 functors. Each node contains 
%           list of event/4 functors and cost 
%       +StartTime: Timestamp when searching started 
%       +Time: Amount of time searching can take 
%       -Events: List of events/4 functor with lowest cost 
%           found after Time seconds.
%
%       When Time seconds have elapsed, the best node has to
%       be found from NodeList. Otherwise, another set of 
%       modifications can be made. Each iteration includes
%       the 3 best nodes and a random extra node. This extra 
%       node makes this PAC. If there were no Time limit, all 
%       valid schedules would be evaluated.
outer_loop(NodeList, StartTime, Time, Events) :-
    get_time(EndTime),
    Diff is EndTime - StartTime,
    Diff < Time,
    !,
    modify(NodeList, NewNodeList),
    sort_functors_asc(2,NewNodeList,SortedNodeList),
    getN(SortedNodeList, 3, BestNodes),
    length(SortedNodeList, Length),
    random_between(4, Length, X),
    nth1(X, SortedNodeList, ExtraNode),
    outer_loop([ExtraNode|BestNodes], StartTime, Time, Events).

outer_loop(NodeList, _, _, Events) :-
    find_best(NodeList, node(Events, Cost)),
    format("Cost is ~f", Cost).

% modify(+NodeList, -NewList)
%       +NodeList: List of node/2 functors 
%       -NewList: List of modified node/2 functors 
%
%       Each node/2 functors contains a list of event/4
%       functors. From each of these, a random event will
%       be chosen. This will be modified by modify_event/2.
%       When this results in a valid schedule, a new node is 
%       created and added to NewList. If not, the modification 
%       is retried by backtracking. The original node is 
%       preserved as well, in case all modifications are worse.
modify([], []).

modify([node(Events,Cost)|RestNodes], NewList) :-
    modify(RestNodes, List),
    random_element(Events, Event),
    modify_event(Event, ModEvent),
    delete_first(Event, Events, Removed),
    is_valid(schedule([ModEvent|Removed])),
    cost(schedule([ModEvent|Removed]), NewCost),
    append([node(Events,Cost)], [node([ModEvent|Removed],NewCost)], Temp),
    append(Temp, List, NewList).

% modify_event(+Event, -ModEvent)
%       +Event: an event/4 functor 
%       -ModEvent: a modified event/4 functor 
%
%       The Event is modified; the Day and Start is adjusted.
%       This is done by a random permutation of all possible 
%       Days and Hours. The use of member/2 allows backtracking.
%       The two events must be on different days OR at different
%       start hours.
modify_event(event(EID,RID,Day,Start), event(EID,RID,Day2,Start2)) :-
    first_day(FirstDay),
    last_day(LastDay),
    findall(Days, between(FirstDay, LastDay, Days), Range),
    random_permutation(Range, DayPermutation),
    member(Day2, DayPermutation),
    availability(RID,Day2,StartHour,EndHour),
    duration(EID, Duration),
    Max is EndHour - Duration,
    findall(Hour, between(StartHour,Max,Hour), Hours),
    random_permutation(Hours, HourPermutation),
    member(Start2, HourPermutation),
    (Day =\= Day2; Start =\= Start2).

% duplicate(+Elem, +N, -List)
%       +Elem: An item to be duplicated
%       +N: How many duplicates are needed
%       -List: List containing N times Elem
%
%       The item Elem is copied N times, resulting
%       in List.
duplicate(Node, N, List) :-
    duplicate_acc(Node, N, [], List).

duplicate_acc(_, 0, List, List).

duplicate_acc(Node, N, List0, List) :-
    N1 is N - 1,
    duplicate_acc(Node, N1, [Node|List0], List).

% random_element(+List, -Elem)
%       +List: A list of which a random item will be
%           chosen
%       -Elem: A random item from List.
%
%       A random item from List is chosen and bound to 
%       Elem.
random_element(List, Elem) :-
    length(List, Length),
    random(1, Length, X),
    nth1(X, List, Elem).

% getN(+List, +N, -Result)
%       +List: A list of items
%       +N: How many items to take
%       -Result: Contains the first N items of List
%
%       The first N items of List are bound to Result.
getN(List, N, Result) :-
    getN_acc(List, N, [], Result).

getN_acc([], _, Result, Result).

getN_acc(_, 0, Result, Result).

getN_acc([First|Rest], N, Result0, Result) :-
    N1 is N - 1,
    getN_acc(Rest, N1, [First|Result0], Result).

% find_best(+List, -Best)
%       +List: A list of nodes
%       -Best: The best item of List
%
%       Calls the helper predicate find_best/3.
%       This predicate assumes List contains 
%       node/2 functors. The first node is initialized
%       as the best.
find_best([First|Rest], Best) :-
    find_best(First,Rest,Best).

% find_best(+CurrentBest, +Rest, -Best)
%       +CurrentBest: The current best node
%       +Rest: The rest of the node-list
%       -Best: The best item after going through Rest 
%
%       Each node in Rest is checked. When its cost is lower
%       than that of CurrentBest, CurrentBest is replaced.
%       When at the end of the list, Best is bound to 
%       CurrentBest.
find_best(Best,[],Best).

find_best(node(_, CurrentCost), [node(NewEvents, NewCost)|Rest], Best) :-
    NewCost < CurrentCost,
    !,
    find_best(node(NewEvents,NewCost), Rest, Best).

find_best(Current, [_|Rest], Best) :-
    find_best(Current, Rest, Best).
