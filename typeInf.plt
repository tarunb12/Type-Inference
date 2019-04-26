:- begin_tests(typeInf).
:- include(typeInf). 

% tests for typeExp, (int + int) -> int
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail, (int + int) -/-> float
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

% (int + int) -> int
test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :-   % should succeed with T = int
    deleteGVars(),                                      % clean up variables
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),      % check definition is type unit
    assertion(X == int), assertion(Y == int),           % make sure the types are int
    gvar(v, int).                                       % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T == int), assertion(X == int), assertion(Y == int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(),                              % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])),        % add my_fct(int) -> float to the gloval variables
    typeExp(my_fct(X), T),                      % infer type of expression using or function
    assertion(X == int), assertion(T == float). % make sure the types infered are correct


% If tests

% if (x > y || x < y) { a && b } else { c || d }
% x > y || x < y : bool -> bool -> bool
% a && b : bool -> bool -> bool
% c || d : bool -> bool -> bool
test(if_T_0, [nondet, true(T == bool)]) :-
    deleteGVars(),
    typeStatement(if(or(greaterThan(X, Y), lessThan(X, Y)), and(A, B), or(C, D)), T),
    assertion(A == bool), assertion(B == bool), assertion(C == bool), assertion(D == bool),
    assertion(X == int), assertion(Y == int).

% if (x < y) { a + b } else { (int) c }
% x < y : int -> int -> bool
% a + b : int -> int -> int
% c : float -> int
test(if_T_1, [nondet, true(T == int)]) :-
    infer([if(lessThan(X, Y), iplus(A, B), fToInt(C))], T),
    assertion(A == int), assertion(B == int), assertion(C == float),
    assertion(X == int), assertion(Y == int).

% if (!(x && y)) { a } else { (float) b }
% !(x && y) : (bool -> bool) -> bool -> bool
% a : float
% (float) b : int -> float
test(if_T_2, [nondet, true(T == float)]) :-
    deleteGVars(),
    typeStatement(if(not(and(X, Y)), A, iToFloat(B)), T),
    assertion(X == bool), assertion(Y == bool), assertion(A == float),
    assertion(B == int). 

% if code type : float -> int, else code type : int -> float --> fail (different types)
test(if_F_0, [fail]) :-
    deleteGVars(),
    typeStatement(if(bool, int, float), int).

% if code type : float -> int, else code type : int -> float --> fail (different types)
% if (... -> bool) { ... -> int } else { ... -> float }
test(if_F_1, [fail]) :-
    deleteGVars(),
    typeStatement(if(bool, int, float), float).

% condition type : int --> fail (supposed to be bool)
% if (... -> int) { ... -> unit } else { ... -> unit }
test(if_F_2, [fail]) :-
    infer([if(int, unit, unit)], unit).


% For tests

% for (let v = ?; bool; float) { ... -> string }
% let v = ? : atom -> float -> unit
test(for_T_0, [nondet, true(T == string)]) :-
    infer([for(gvLet(v, float, float), bool, float, string)], T).

% for (let v = ?; c; v') { ... }
% let v = ? : atom -> int -> unit
% c : bool
% v' : int -> ... -> int
test(for_T_1, [nondet, true(T == int)]) :-
    deleteGVars(),
    typeStatement(for(gvLet(v, int, int), C, int, int), T),
    assertion(C == bool).

% for (let v = ?; a; v') { (float) (b + c) }
% let v = ? : atom -> int -> unit
% a : bool
% v' : int -> ... -> int
% (float) (b + c) : (int -> int -> int) -> float
test(for_T_2, [nondet, true(T == float)]) :-
    deleteGVars(),
    typeStatement(for(gvLet(v, int, int), A, int, iToFloat(iplus(B, C))), T),
    assertion(A == bool), assertion(B == int), assertion(C == int).

% for (let v = ?; a; v') { (float) (b + c) }
% let v = ? : atom -> int -> unit
% a : bool
% v' : int -> ... -> int
% (float) (b + c) : (int -> int -> int) -> float
test(for_T_3, [nondet, true(T == float)]) :-
    deleteGVars(),
    typeStatement(for(gvLet(v, int, int), A, int, iToFloat(iplus(B, C))), T),
    assertion(A == bool), assertion(B == int), assertion(C == int).

% for (let v = ?; a < b; c + d) { ((int) (e + f)) + (g + h) }
% let v = ? : atom -> int -> unit
% a < b : int -> int -> bool
% c + d : int -> int -> int
% ((int) (e + f)) + (g + h) : (float -> float) -> int -> int -> int
test(for_T_4, [nondet, true(T == int)]) :-
    infer([for(gvLet(v, int, int), lessThan(A, B), iplus(C, D), iplus(fToInt(fplus(E, F)), iplus(G, H)))], T),
    assertion(A == int), assertion(B == int), assertion(C == int), assertion(D == int),
    assertion(E == float), assertion(F == float), assertion(G == int), assertion(H == int).

% increment type : bool --> fail
% for (let v = ?; ... -> bool; ... -> bool) { ... -> int }
% let v = ? : atom -> int -> unit
test(for_F_0, [fail]) :-
    infer([for(gvLet(v, int, int), bool, bool, int)], int).

% gvar definition type : float, increment type : int --> fail
% for (let v = ?; bool; int) { ... -> string }
% let v = ? : atom -> float -> unit
test(for_F_1, [fail]) :-
    deleteGVars(),
    typeStatement(for(gvLet(v, float, float), bool, int, int), int).

% gvLet type : unit --> fail (supposed to be gvLet(...) -> unit)
% for (... -> int; ... -> bool, ... -> int) { ... -> string }
test(for_F_2, [fail]) :-
    infer([for(unit, bool, int, string)], string).


% While tests

% while (... -> bool) { ... -> float }
test(while_T_0, [nondet, true(T == float)]) :-
    infer([while(bool, float)], T).

% while (!a) { ... -> string }
% !a : bool -> bool
test(while_T_1, [nondet, true(T == string)]) :-
    infer([while(not(A), string)], T),
    assertion(A == bool).

% while (a && b) { print(a) }
% a && b : bool -> bool -> bool
% print(a) : bool -> unit
test(while_T_2, [nondet, true(T == unit)]) :-
    infer([while(and(A, B), print(A))], T),
    assertion(A == bool), assertion(B == bool).

% conditional should be bool
% while (... -> int) { ... -> bool }
test(while_F_0, [fail]) :-
    infer([while(int, bool)], bool).

% return type should be unit
% while(... -> bool) { ... -> unit }
test(while_F_1, [fail]) :-
    infer([while(bool, unit)], string).

% Print tests

% print(x) : unit
test(print_T_0, [true(T == unit)]) :- 
    typeExp(print(_X), T).

% print('test') : unit
test(print_T_1, [true(T == unit)]) :- 
    typeExp(print('test'), T).

% return type should be unit
% print(... -> int) : int
test(print_F, [fail]) :-
    typeExp(print(int), int).


% Infer tests

% for (let v = ?; !a; b + c) { (float) c + d }
% while (!e) { print(e) }
% if (f || g) { ... -> string } else { ... -> string }
% let v = ? : atom -> int -> unit
% !a : bool -> bool
% b + c : int -> int -> int
% (float) c + d : (int -> float) -> float -> float
% !e : bool -> bool
% print(e) : bool -> unit
% f || g : bool -> bool -> bool
test(infer_T_0, [nondet, true(T == string)]) :-
    infer([
        for(gvLet(v, int, int), not(A), iplus(B, C), fplus(iToFloat(C), D)),
        while(not(E), print(E)),
        if(or(F, G), string, string)
    ], T), assertion(A == bool), assertion(B == int), assertion(C == int),
    assertion(D == float), assertion(E == bool), assertion(F == bool),
    assertion(G == bool).

% print(... -> string)
% while (a && b) { print(a) }
% a && b : bool -> bool -> bool
% print(a) : bool -> unit
test(infer_T_1, [nondet, true(T == unit)]) :-
    infer([
        print(string),
        while(and(A, B), print(A))
    ], T), assertion(B == bool).

% (... -> float) + (... -> float)
% for (let v = ?; c < d; e + f) { (float) (g + h) }
% let v = ? : atom -> int -> unit
% c < d : int -> int -> unit
% e + f : int -> int -> int
% (float) g + h : (int -> int -> int) -> float
test(infer_T_2, [nondet, true(T == float)]) :-
    infer([
        fplus(float, iToFloat(fToInt(float))),
        for(gvLet(v, int, int), lessThan(C, D), iplus(E, F), iToFloat(iplus(G, H)))
    ], T), assertion(C == int),
    assertion(D == int), assertion(E == int), assertion(F == int),
    assertion(G == int), assertion(H == int).

% type of last statement (float + float) -> float, not int --> fail
% (... -> int) + (... -> int)
% (int) ((... -> float) + (... -> float))
% (... -> float) + (... -> float)
test(infer_F_0, [fail]) :-
    infer([
        iplus(int, int),
        fToInt(fplus(float, float)),
        fplus(float, float)
    ], int).

% fplus(float, int) --> fail
% while (... -> bool) { ... -> int }
% for (let v = ?; ... -> bool; ... -> int) { ... -> float }
% (... -> float) + (... -> int)
test(infer_F_1, [fail]) :-
    infer([
        while(bool, int),
        for(gvLet(v, int, int), bool, int, float),
        fplus(float, fToInt(float))
    ], float).

:- end_tests(typeInf).