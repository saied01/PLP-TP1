%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(escribir(_,_)).
proceso(leer(_)).
proceso(secuencia(_,_)).
proceso(paralelo(_,_)).

% borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista ListaOriginal.

borrar([], _, []).
borrar([X|Xs], X, Ys) :- borrar(Xs, X, Ys).
borrar([X|Xs], Y, [X|Ys]) :- X \= Y, borrar(Xs, Y, Ys).

% sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1.

sacarDuplicados([], []).
sacarDuplicados([X|L1], L2) :- sacarDuplicados(L1, Ys), borrar(Ys, X, Xs), append([X], Xs, L2).

%% Ejercicio 2
%% buffersUsados(+P,-BS)
buffersUsados(computar,[]).
buffersUsados(escribir(B,_),[B]).
buffersUsados(leer(B),[B]).
buffersUsados(secuencia(P,Q),BS) :- buffersUsados(P,BSP), buffersUsados(Q,BSQ), append(BSP,BSQ,BSD), sacarDuplicados(BSD, BS).
buffersUsados(paralelo(P,Q),BS) :- buffersUsados(P,BSP), buffersUsados(Q,BSQ), append(BSP,BSQ,BSD), sacarDuplicados(BSD, BS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)
intercalar([],[],[]).
intercalar([X|XS],[],[X|XS]).
intercalar([],[Y|YS],[Y|YS]).
intercalar([X|XS],[Y|YS],[X|ZS]) :- intercalar(XS,[Y|YS],ZS).
intercalar([X|XS],[Y|YS],[Y|ZS]) :- intercalar([X|XS],YS,ZS).


%% Ejercicio 4
%% serializar(+P,?XS)
serializar(computar,[computar]).
serializar(leer(B),[leer(B)]).
serializar(escribir(B,X),[escribir(B,X)]).
serializar(secuencia(P,Q),Z) :- serializar(P,XS), serializar(Q,YS), append(XS,YS,Z).
serializar(paralelo(P,Q),Z)  :- serializar(P,XS), serializar(Q,YS), intercalar(XS,YS,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

procesarInstrucciones(_, [],C1,C1).
procesarInstrucciones(B, [computar|XS],YS,C)       :- procesarInstrucciones(B,XS,YS,C).
procesarInstrucciones(B, [leer(B)|XS],[],C)        :- procesarInstrucciones(B,XS,[],C).
procesarInstrucciones(B, [leer(B)|XS],[_|YS],C)    :- procesarInstrucciones(B,XS,YS,C).
procesarInstrucciones(B1,[leer(B2)|XS],YS,C)       :- B1 \= B2, procesarInstrucciones(B1,XS,YS,C).
procesarInstrucciones(B, [escribir(B,E)|XS],YS,C)  :- append([E],YS,C1), procesarInstrucciones(B,XS,C1,C).
procesarInstrucciones(B1,[escribir(B2,_)|XS],YS,C) :- B1 \= B2, procesarInstrucciones(B1,XS,YS,C).

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(B,P,C) :- serializar(P,LP), contenidoBuffer(B,LP,C).
contenidoBuffer(_,[],[]).
contenidoBuffer(B,LP,C) :- procesarInstrucciones(B,LP,[],C).

%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)

%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(_,_,_).

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.



%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsBasicos(2). % Actualizar con la cantidad de tests que entreguen
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- buffersUsados(escribir(1, hola), [1]).
% Agregar mas tests

cantidadTestsProcesos(0). % Actualizar con la cantidad de tests que entreguen
% Agregar mas tests

cantidadTestsBuffers(0). % Actualizar con la cantidad de tests que entreguen
% Agregar mas tests

cantidadTestsSeguros(0). % Actualizar con la cantidad de tests que entreguen
% Agregar mas tests


tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).