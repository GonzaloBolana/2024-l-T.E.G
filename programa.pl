% Practica de TED 
continente(americaDelSur).
continente(americaDelNorte).
continente(asia).
continente(oceania).

%estaEn(Continente, Pais). Relaciona un pais con el continente en el que esta ubicado
estaEn(americaDelSur, argentina).
estaEn(americaDelSur, brasil).
estaEn(americaDelSur, chile).
estaEn(americaDelSur, uruguay).
estaEn(americaDelNorte, alaska).
estaEn(americaDelNorte, yukon).
estaEn(americaDelNorte, canada).
estaEn(americaDelNorte, oregon).
estaEn(asia, kamtchatka).
estaEn(asia, china).
estaEn(asia, siberia).
estaEn(asia, japon).
estaEn(oceania,australia).
estaEn(oceania,sumatra).
estaEn(oceania,java).
estaEn(oceania,borneo).

%jugador(Jugador).
jugador(amarillo).
jugador(magenta).
jugador(negro).
jugador(blanco).

%aliados(UnJugador,OtroJugador). Relaciona dos jugadores si son aliados
aliados(X,Y):- alianza(X,Y).
aliados(X,Y):- alianza(Y,X).
alianza(amarillo,magenta).

%ocupa(Pais,Jugador,Nro). Relaciona un jugador con un pais en el que tiene ejercitos
%el numero son los ejercitos
ocupa(argentina, magenta, 5).
ocupa(chile, negro, 3).
ocupa(brasil, amarillo, 8).
ocupa(uruguay, magenta, 5).
ocupa(alaska, amarillo, 7).
ocupa(yukon, amarillo, 1).
ocupa(canada, amarillo, 10).
ocupa(oregon, amarillo, 5).
ocupa(kamtchatka, negro, 6).
ocupa(china, amarillo, 2).
ocupa(siberia, amarillo, 5).
ocupa(japon, amarillo, 7).
ocupa(australia, negro, 8).
ocupa(sumatra, negro, 3).
ocupa(java, negro, 4).
ocupa(borneo, negro, 1).

% Usar este para saber si son limitrofes ya que es una relacion simetrica
sonLimitrofes(X, Y) :- limitrofes(X, Y).
sonLimitrofes(X, Y) :- limitrofes(Y, X).

limitrofes(argentina,brasil).
limitrofes(argentina,chile).
limitrofes(argentina,uruguay).
limitrofes(uruguay,brasil).
limitrofes(alaska,kamtchatka).
limitrofes(alaska,yukon).
limitrofes(canada,yukon).
limitrofes(alaska,oregon).
limitrofes(canada,oregon).
limitrofes(siberia,kamtchatka).
limitrofes(siberia,china).
limitrofes(china,kamtchatka).
limitrofes(japon,china).
limitrofes(japon,kamtchatka).
limitrofes(australia,sumatra).
limitrofes(australia,java).
limitrofes(australia,borneo).
limitrofes(australia,chile).

% Definir los siguientes predicados de modo que sean completamente inversibles. 
% Agregar tests para mínimo los casos de prueba comentados.

% 1. tienePresenciaEn/2: Relaciona un jugador con un continente del cual ocupa, al menos, un país.

tienePresenciaEn(Jugador,Continente):-
    jugador(Jugador),
    ocupa(Pais,Jugador,_),
    estaEn(Continente,Pais).

% 2. puedenAtacarse/2: Relaciona dos jugadores si uno ocupa al menos un país limítrofe a algún país ocupado por el otro
puedenAtacarse(Jugador1,Jugador2):-
    jugador(Jugador1),
    jugador(Jugador2),
    ocupa(Pais1,Jugador1,_),
    ocupa(Pais2,Jugador2,_),
    sonLimitrofes(Pais1,Pais2).

% 3. sinTensiones/2: Relaciona dos jugadores que, o bien no pueden atacarse, o son aliados.
sinTensiones(Jugador1,Jugador2):-
    jugador(Jugador1),
    jugador(Jugador2),
    not(puedenAtacarse(Jugador1,Jugador2)).

sinTensiones(Jugador1,Jugador2):- aliados(Jugador1,Jugador2).

% 4. perdió/1: Se cumple para un jugador que no ocupa ningún país.

perdio(Jugador):-
    jugador(Jugador),
    not(ocupa(_,Jugador,_)).

% 5. controla/2: Relaciona un jugador con un continente si ocupa todos los países del mismo

controla(Jugador,Continente):-
    jugador(Jugador),
    continente(Continente),
    forall(estaEn(Continente,Pais),ocupa(Pais,Jugador,_)). 
    %Para todos los paises del continente, el jugador ocupa dichos paises

% Un jugador --> si NO hay ningun pais en el continente, que el jugador NO ocupe 

controlaNOT(Jugador,Continente):-
    jugador(Jugador),
    continente(Continente),
    not((estaEn(Continente,Pais))), not(ocupa(Pais,Jugador,_)).

% 6. reñido/1: Se cumple para los continentes donde todos los jugadores ocupan algún país.

renido(Continente):-
    continente(Continente),
    forall(jugador(Jugador), (ocupa(Pais,Jugador,_), estaEn(Continente,Pais))).
    % Para todo jugador, el jugador tiene que ocupar un pais que dicho pais se encuentre en el continente

renidoNOT(Continente):-
    continente(Continente),
    not(((jugador(Jugador), not((ocupa(Pais,Jugador,_), estaEn(Continente,Pais)))))).

renidoV2(Continente):-
    continente(Continente),
    forall(jugador(Jugador),tienePresenciaEn(Jugador,Continente)).
%Para todo jugador, que tiene presencia en un continente (al menos un pais).


% 7. atrincherado/1: Se cumple para los jugadores que ocupan países en un único continente.
% --> el jugador NO tiene ocupado ningun pais fuera del continente 
%(NO tiene presencias en otros continentes que NO sea el unico que tiene ocupado)

atrincherado(Jugador):-
    ocupa(_,Jugador,_),
    continente(Continente),
    forall(ocupa(Pais,Jugador,_),estaEn(Continente,Pais)).

%Para todos los paises que el jugador ocupa, dichos paises estan en el mismo continente.

% 8. puedeConquistar/2: Relaciona un jugador con un continente si no lo controla, 
% pero todos los países del continente que le falta ocupar son limítrofes a alguno que sí ocupa 
% y pertenecen a alguien que no es su aliado.

puedenConquistar(Jugador,Continente):-
    jugador(Jugador),
    continente(Continente), 
    controlaNOT(Jugador,Continente),
    forall((estaEn(Pais,Continente),not(ocupa(Pais,Jugador,_))),puedeAtacar(Jugador,Pais)).

% Todos los paises del continente, que el jugador NO ocupa....... 

puedeAtacar(Jugador,Pais):-
    ocupa(Pais,Jugador,_),
    sonLimitrofes(PaisAtacado,Pais),
    not((aliados(Jugador,OtroJugador),ocupa(PaisAtacado,OtroJugador,_))).
    % NO existe ningun aliado que ocupe el pais al cual quiero atacar
    %forall(aliados(Jugador,OtroJugador), not(ocupa(PaisAtacado,OtroJugador,_))).
    % PARA TODOS los aliados, ninguno ocupa el pais que quiero atacar

%-------------------------------------------------------------------------------------------------------------

% Parte A:
% 1) loLiquidaron/1 que se cumple para un jugador si no ocupa ningún país.
% Caso de prueba Un jugador que no ocupe ningún país está liquidado (x ej. el blanco) 
loLiquidaron(Jugador):-
    jugador(Jugador),
    not(ocupa(_,Jugador,_)).

% 2) ocupaContinente/2 que relaciona un jugador y un continente si el jugador ocupa todos los países del mismo.
% Caso de prueba Si tiene todos los países el jugador ocupa el continente (x ej. el amarillo con americaDelNorte)
ocupaContinente(Jugador, Continente) :-
    jugador(Jugador),
    continente(Continente),
    forall(estaEn(Continente, Pais), ocupa(Pais, Jugador, _)).

%Para todos los paises del continente, que el jugador ocupa

% 3) seAtrinchero/1 que se cumple para los jugadores que ocupan países en un único continente.
% Caso de prueba Si está en un único continente se atrincheró (x ej. el magenta en américa del sur)
seAtrinchero(Jugador) :-
    ocupa(_, Jugador, _), % es un Jugador que ocupa algo por lo menos
    continente(Continente), % LIGO EL CONTINENTE porque quiero que sea en ese mismo continente la condicion del forall
    forall(ocupa(Pais,Jugador,_), estaEn(Continente,Pais)).

%seAtrincheroV2(Jugador) :-
%    jugador(Jugador),
%    findall(Continente, (ocupa(Pais,Jugador,_), estaEn(Continente,Pais)), Continentes),
%    list_to_set(Continentes, [_]). % si la lista de continentes, solo es una lista con un unico continente

%Para todos los Paises que el Jugador ocupa, dicho pais esta en dicho continente.
%Para todo Jugador que ocupa
%Parte B

/*
4) puedeConquistar/2 que relaciona un jugador y un continente si este puede atacar a cada país que le falte. Es decir, no ocupa dicho continente, 
pero todos los países del mismo que no tiene son limítrofes a alguno que ocupa y a su vez ese país no es de un aliado.

*/

puedeConquistar(Jugador,Continente):-
    jugador(Jugador),
    continente(Continente),
    not(ocupaContinente(Jugador,Continente)),
    forall(paisesFaltantesEnElContinente(Jugador,Continente,Pais),puedeAtacarPais(Jugador,Pais)).

paisesFaltantesEnElContinente(Jugador,Continente,Pais):-
    estaEn(Continente,Pais),
    not(ocupa(Pais,Jugador,_)).


puedeAtacarPais(Jugador,Pais):-
    ocupa(Pais,Jugador,_),
    sonLimitrofes(PaisAtacado,Pais),
    forall(aliados(Jugador,OtroJugador),not(ocupa(PaisAtacado,OtroJugador,_))).
%Para todos los paises del jugador Faltantes en el continente, puede atacar a dicho pais.

% 5) elQueTieneMasEjercitos/2 que relaciona un jugador y un país 
%si se cumple que en ese país hay más ejércitos que en los países del resto del mundo y a su vez ese país es ocupado por ese jugador.
% Ejemplo para el/los caso/s de prueba: El que tiene más ejércitos es el amarillo, en canadá.

elQueTieneMasEjercitos(Jugador,Pais):-
    ocupa(Pais,Jugador,Ejercito),
    forall((ocupa(OtroPais,_,OtroEjercito),Pais\=OtroPais),OtroEjercito < Ejercito).
% Para todos los demas paises ocupados (sin considerar al pais con mayor ejercito), estos tienen un ejercito menor.

% 6)
%objetivo(Jugador, objetivoQueQuiereCumplir).
objetivo(amarillo, ocuparContinente(asia)).
objetivo(amarillo,ocuparPaises(2, americaDelSur)). 
objetivo(blanco, destruirJugador(negro)). 
objetivo(magenta, destruirJugador(blanco)). 
objetivo(negro, ocuparContinente(oceania)).
objetivo(negro,ocuparContinente(americaDelSur)). 

% cumpleObjetivos/1 que se cumple para un jugador si cumple todos los objetivos que tiene.
%Los objetivos se cumplen de la siguiente forma:
% - ocuparContinente: el jugador debe ocupar el continente indicado
% - ocuparPaises: el jugador debe ocupar al menos la cantidad de países indicada de ese continente
% - destruirJugador: se cumple si el jugador indicado ya no ocupa ningún país
%Pensar el/los caso/s de prueba necesario/s.  

cumpleObjetivos(Jugador):-
    jugador(Jugador),
    forall(objetivo(Jugador,Objetivo),cumplirObjetivo(Jugador,Objetivo)).

cumplirObjetivo(Jugador,ocuparContinente(Continente)):-ocupaContinente(Jugador,Continente).

cumplirObjetivo(Jugador, ocuparPaises(Cantidad,Continente)) :- 
    findall(Pais,(estaEn(Continente,Pais), puedeAtacarPais(Jugador,Pais)), ListaDePaises),
    length(ListaDePaises,CantidadDePaises),
    CantidadDePaises >= Cantidad.

cumplirObjetivo(_, destruirJugador(JugadorADestruir)) :- loLiquidaron(JugadorADestruir).

% 7) leInteresa/2 que relaciona un jugador y un continente, y es cierto cuando alguno de sus objetivos implica hacer 
% algo en ese continente (en el caso de destruirJugador, si el jugador a destruir ocupa algún país del continente).

leInteresa(Jugador,Continente):-
    objetivo(Jugador,_),
    algoEnElContinente(Jugador,Continente).

algoEnElContinente(Jugador,Continente):- objetivo(Jugador,ocuparContinente(Continente)).
algoEnElContinente(Jugador,Continente):- objetivo(Jugador,ocuparPaises(_,Continente)).
algoEnElContinente(Jugador,Continente):- objetivo(Jugador,destruirJugador(OtroJugador)), tienePresenciaEn(OtroJugador,Continente).
