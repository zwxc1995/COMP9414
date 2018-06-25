% pathsearch.pl

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% This file provides code for insert_legs(), head_member() and build_path()
% used by bfsdijkstra(), ucsdijkstra(), greedy() and astar().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).

% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N)  :-
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).

% According to the teacher's hint, update goals and s to prepare for initializing Intention.
:- dynamic s/3.
s(goal(X0,Y0),goal(X1,Y1),1):-
   land_or_dropped(X1,Y1),  
   distance((X0,Y0),(X1,Y1),1).
s(goal(X0,Y0),goal(X1,Y1),999):-
   between(1,10,Y1),
   between(1,10,X1),
   not(land_or_dropped(X1,Y1)),
   distance((X0,Y0),(X1,Y1),1).

goal(goal(X,Y)):-
   agent_at(X,Y).

% This trigger is to filter out the coordinates of unreachable stones.
% trigger1(+Path,-Result).
trigger1([],[]).
trigger1([goal(X,Y)|T],[[goal(X,Y),[]]|G]):-
   not(land_or_dropped(X,Y)),
   trigger1(T,G).
trigger1([goal(X,Y)|T],G):-
   land_or_dropped(X,Y),
   trigger1(T,G).

% To find all goals.
% findG(+goal(G0,G1),-G2).
findG(goal(G0,G1),G2):-
   solve(goal(G0,G1),Path,_,_),
   trigger1(Path,G2).

% According to findG,get the correct form of the result.
% initial_intentions(-Intentions).
initial_intentions(intents(L,[])):-
   monster(X,Y),
   findG(goal(X,Y),G),
   findall(M,member(M,G),L).   
 
% trigger(+Percepts,-Goals).
trigger([],[]).
trigger([stone(X,Y)|T],[goal(X,Y)|G]):-
   trigger(T,G).

% This trigger is to filter out the coordinates of unreachable stones.Also put them in a new list.
% trugger2(Path,List)
trigger2([],[]).
trigger2([goal(X,Y)|T],[goal(X,Y)|G]):-
   not(land_or_dropped(X,Y)),
   trigger2(T,G).
trigger2([goal(X,Y)|T],G):-
   land_or_dropped(X,Y),
   trigger2(T,G).

% Determine whether a path can go through.
% road_right(+goal(X,Y)).
road_right(goal(X,Y)):-
   solve(goal(X,Y),Path,_,_),
   trigger2(Path,B),
   B=[].

% If the path can go through, perform the following classification discussion, complete the recursion, return the result.
% new_goal(+Goal,+Intentions,Intentions2).
new_goal(goal(X1,Y1),intents(Int_drop,Int_pick0),intents(Int_drop,Int_pick1)):-
   agent_at(X0,Y0),
   distance((X0,Y0),(X1,Y1),X),
   new_goal1(goal(X1,Y1),X,Int_pick0,Int_pick1).
new_goal1(goal(X1,Y1),_,[],[[goal(X1,Y1),[]]]).
new_goal1(goal(X1,Y1),_,[[goal(X1,Y1),[]]|T],[[goal(X1,Y1),[]]|T]).
new_goal1(goal(X1,Y1),_,[[goal(X1,Y1),Plan]|T],[[goal(X1,Y1),Plan]|T]).
new_goal1(goal(X1,Y1),X,[[goal(X2,Y2),Plan]|T],[[goal(X1,Y1),[]],[goal(X2,Y2),Plan]|T]):-
   agent_at(X0,Y0),
   distance((X0,Y0),(X2,Y2),Y),
   Y>X,!.
new_goal1(goal(X1,Y1),X,[[goal(X2,Y2),Plan]|T0],[[goal(X2,Y2),Plan]|T1]):-
   agent_at(X0,Y0),
   distance((X0,Y0),(X2,Y2),Y),
   X>=Y,
   new_goal1(goal(X1,Y1),X,T0,T1).

% Here are mainly divided into three cases, if you repeat first, you don't need to operate. If you don't repeat and the path works, you can do the above operation. If it doesn't work, do not operate.
% incorporate_goals(+Goals,+Intentions,-Intentions1).
incorporate_goals([],I,I).
incorporate_goals([goal(X1,Y1)|G],I,I1):-
   member([goal(X1,Y1),[]],I),
   incorporate_goals(G,I,I1).
incorporate_goals([goal(X1,Y1)|G],I,I1):-
   not(member([goal(X1,Y1),[]],I)),
   road_right(goal(X1,Y1)),
   new_goal(goal(X1,Y1),I,I2),
   incorporate_goals(G,I2,I1).
incorporate_goals([goal(X1,Y1)|G],I,I1):-
   not(road_right(goal(X1,Y1))),
   incorporate_goals(G,I,I1).

% According to the topic requirements, divided into three main situations.
% get_action(+Intentions,-Intentions1,-Action).
move([],[]).
move([goal(X,Y)|Path],[move(X,Y)|Path1]):-
   move(Path,Path1).

get_action(intents([[goal(X1,Y1),[Plan|T]]|T1],Int_pick),intents([[goal(X1,Y1),T]|T1],Int_pick),Plan):-
   agent_stones(1),
   applicable(Plan).

get_action(intents([[goal(X1,Y1),[Plan|_]]|T1],Int_pick),intents([[goal(X1,Y1),P1]|T1],Int_pick),Action):-
   agent_stones(1),
   not(applicable(Plan)),
   solve(goal(X1,Y1),Path,_,_),
   append(Path1,[_],Path),
   move(Path1,Plan1),
   append(Plan1,[drop(X1,Y1)],[Action|P1]).

get_action(intents([[goal(X1,Y1),[]]|T1],Int_pick),intents([[goal(X1,Y1),P1]|T1],Int_pick),Action):-
   agent_stones(1),
   solve(goal(X1,Y1),Path,_,_),
   append(Path1,[_],Path),
   move(Path1,Plan1),
   append(Plan1,[drop(X1,Y1)],[Action|P1]).

get_action(intents(Int_drop,[[goal(X1,Y1),[Plan|T]]|T1]),intents(Int_drop,[[goal(X1,Y1),T]|T1]),Plan):-
   agent_stones(0),
   applicable(Plan).

get_action(intents(Int_drop,[[goal(X1,Y1),[Plan|_]]|T1]),intents(Int_drop,[[goal(X1,Y1),P1]|T1]),Action):-
   agent_stones(0),
   not(applicable(Plan)),
   solve(goal(X1,Y1),Path,_,_),
   append(Path1,[_],Path),
   move(Path1,Plan1),
   append(Plan1,[pick(X1,Y1)],[Action|P1]).

get_action(intents(Int_drop,[[goal(X1,Y1),[]]|T1]),intents(Int_drop,[[goal(X1,Y1),P1]|T1]),Action):-
   agent_stones(0),
   agent_at(X0,Y0),
   solve(goal(X1,Y1),Path,_,_),
   append(Path1,[_],Path),
   move(Path1,Plan1),
   append(Plan1,[pick(X1,Y1)],Plan2),
   remove_list(Plan2,[move(X0,Y0)],Plan3),
   Plan3=[Action|P1].

get_action(intents(Int_drop,[]),intents(Int_drop,[]),move(X0,Y0)):-
   agent_stones(0),
   agent_at(X0,Y0).

% remove_list function.
% remove_list(+List1,+List2,-Newlist).
remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result). 
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).

% update_intentions(+Observation,+Intentions,-Intentions1).
update_intentions(at(_,_),Intentions,Intentions).
update_intentions(picked(X,Y),intents(Int_drop,Int_pick1),intents(Int_drop,Int_pick2)):-
   remove_list(Int_pick1,[[goal(X,Y),[]]],Int_pick2).
update_intentions(dropped(X,Y),intents(Int_drop1,Int_pick),intents(Int_drop2,Int_pick)):-
   remove_list(Int_drop1,[[goal(X,Y),[]]],Int_drop2).



