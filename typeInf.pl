% match functions by unifying with arguments 
% and infering the result
typeExp(Fct, T) :-
    \+ var(Fct),                    % make sure Fct is not a variable
    \+ atom(Fct),                   % or an atom
    functor(Fct, Fname, _Nargs),    % ensure we have a functor
    !,                              % if we make it here we do not try anything else
    Fct =.. [Fname|Args],           % get list of arguments
    append(Args, [T], FType),       % make it loook like a function signature
    functionType(Fname, TArgs),     % get type of arguments from definition
    typeExpList(FType, TArgs).      % recurisvely match types

% propagate types
typeExp(T, T).

% list version to allow function matching
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]) :-
    typeExp(Hin, Hout),         % type infer the head
    typeExpList(Tin, Tout).     % recurse on tail

% gvLet(v, T, int) ~ let v = int;
typeStatement(gvLet(Name, T, Code), unit) :-
    atom(Name),             % make sure we have a bound name
    typeExp(Code, T),       % infer the type of Code and ensure it is T
    bType(T),               % make sure we have an infered type
    asserta(gvar(Name, T)). % add definition to database

% ifElse(Cond, TCode, FCode) ~ if (cond) { ...TCode } else { ...FCode }
typeStatement(if(Cond, TCode, FCode), T) :- 
    typeExp(Cond, bool),    % Check conditional type is bool
    typeExp(TCode, T),      % Check type of code for true condition
    typeExp(FCode, T),      % Check type of code for false condition
    bType(T).               % Check that T is basic type

typeStatement(while(Cond, Code), T) :-
    typeExp(Cond, bool),
    typeExp(Code, T),
    bType(T).

% for(gvLet(v, LetT, int), Cond, Increment, ForCode) ~ for (let v = int; Cond; Increment) { ...ForCode }
typeStatement(for(gvLet(Name, LetT, LetCode), Cond, Increment, ForCode), CodeT) :-
    atom(Name),                 % Check that the variable name is an atom
    typeExp(LetCode, LetT),     % Check Code is same type as T
    bType(LetT),                % Check variable is basic type
    asserta(gvar(Name, LetT)),  % Add definition to database
    typeExp(Cond, bool),        % Check conditional type is bool
    typeExp(Increment, LetT),   % Check type of increment
    typeExp(ForCode, CodeT),    % Check type of for code
    deleteGVars().              % Remove definitions from database

typeStatement(T, T).
typeStatement([T], T) :- typeStatement(T, T).

% typeStatement(fctLet(Name, T, TCode, S, Code), S).

% Code is simply a list of statements. The type is 
% the type of the last statement 
typeCode([S], T) :- typeStatement(S, T).
typeCode([S, S2|Code], T) :-
    typeStatement(S, _T),
    typeCode([S2|Code], T).

% top level function
infer(Code, T) :-
    is_list(Code),      % make sure Code is a list
    deleteGVars(),      % delete all global definitions
    typeCode(Code, T).

% Basic types
bType(int).                         % Basic integer type
bType(float).                       % Basic float type
bType(string).                      % Basic string type
bType(bool).                        % Basic boolean type
bType(unit).                        % Basic unit type (non-expressions)
bType([H]) :- bType(H).             % Basic function type 'a ([a])
bType([H|T]) :- bType(H), bType(T). % Basic function type 'a -> ... -> 'z ([a, ..., z])

% Built in functions
fType(iplus, [int, int, int]).          % (int + int) -> int
fType(fplus, [float, float, float]).    % (float + float) -> float
fType(fToInt, [float, int]).            % float -> int (conversion)
fType(iToFloat, [int, float]).          % int -> float (conversion)
fType(lessThan, [int, int, bool]).      % (int > int) -> bool
fType(greaterThan, [int, int, bool]).   % (int < int) -> bool
fType(and,[bool, bool, bool]).          % (bool && bool) -> bool
fType(or, [bool, bool, bool]).          % (bool || bool) -> bool
fType(not, [bool, bool]).               % !bool -> bool
fType(print, [_X, unit]).               % print -> unit

% Check the user defined functions first
functionType(Name, Args) :-
    gvar(Name, Args),
    is_list(Args).      % make sure we have a function, not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !.   % make deterministic

deleteGVars() :- retractall(gvar), asserta(gvar(_X, _Y) :- false()).

:- dynamic(gvar/2).