%cantante(Cantante,Tema,Tiempo).
cantante(megurineluka,cancion(nightFever,4)).
cantante(megurineluka,cancion(foreverYoung,5)).
cantante(hatsuneMiku,cancion(tellYourWorld,4)).
cantante(gumi,cancion(foreverYoung,4)).
cantante(gumi,cancion(tellYourWorld,5)).
cantante(seeU,cancion(novemberRain,6)).
cantante(seeU,cancion(nightFever,5)).

%1----------------------------------------------------------------------------------------------------------------
esNovedoso(Cantante):-sabeAlMenos2Canciones(Cantante),tiempoTotal(Cantante,Resultado),Resultado <15.

sabeAlMenos2Canciones(Cantante):-cantante(Cantante,Cancion),cantante(Cantante,OtraCancion),Cancion \= OtraCancion.

tiempoTotal(Cantante,Resultado):-findall(Tiempo,tiempoDeCancion(Cantante,Tiempo),ListaDeTiempo),sumlist(ListaDeTiempo,Resultado).

tiempoDeCancion(Cantante,Tiempo):-cantante(Cantante,Cancion),dameTiempo(Cancion,Tiempo).

dameTiempo(cancion(_,Tiempo),Tiempo).

%2----------------------------------------------------------------------------------------------------------------
esAcelerado(Cantante):-vocaloid(Cantante),not((tiempoDeCancion(Cantante,Tiempo), Tiempo < 4)).

vocaloid(Cantante):-cantante(Cantante,_).

%1----------------------------------------------------------------------------------------------------------------
%concierto(Pais,DondeSeRealizará,Fama,TipoDeConcierto(gigante())).
concierto(mikuExpo,estadosUnidos,2000,gigante(2,6)).
concierto(magicalMirai,japon,3000,gigante(3,10)).
concierto(vocalektVisions,estadosUnidos,1000,mediano(9)).
concierto(mikuFest,argentina,100,pequenio(4)).

%2----------------------------------------------------------------------------------------------------------------
/* Se requiere saber si un vocaloid puede participar en un concierto,
esto se da cuando cumple los requisitos del tipo de concierto.
También sabemos que Hatsune Miku puede participar en cualquier concierto.*/

puedeParticipar(hatsuneMiku,Concierto):-concierto(Concierto,_,_,_).
puedeParticipar(Cantante,Concierto):-vocaloid(Cantante),Cantante \= hatsuneMiku, concierto(Concierto,_,_,Requisitos),cumpleRequisitos(Requisitos,Cantante).

cumpleRequisitos(gigante(DebeSaberUnMinimoDe,TiempoMinimo),Cantante):-tiempoMinimo(TiempoMinimo,Cantante),debeSaberUnMinimoDe(DebeSaberUnMinimoDe,Cantante).

cumpleRequisitos(mediano(TiempoMaximo),Cantante):- tiempoTotal(Cantante,Resultado), Resultado =< TiempoMaximo.

cumpleRequisitos(pequenio(TiempoMinimo),Cantante):-cantante(Cantante,Cancion),dameTiempo(Cancion,Tiempo), Tiempo > TiempoMinimo.

tiempoMinimo(TiempoMinimo,Cantante):-tiempoTotal(Cantante,Resultado), Resultado > TiempoMinimo.

debeSaberUnMinimoDe(Cantidad,Cantante):-cantidadDeCanciones(Cantante,TotalDeCanciones),TotalDeCanciones >= Cantidad.

cantidadDeCanciones(Cantante,TotalDeCanciones):-findall(Canciones,cantante(Cantante,Canciones),ListaDeCanciones), length(ListaDeCanciones,TotalDeCanciones).

%3----------------------------------------------------------------------------------------------------------------
/*Conocer el vocaloid más famoso, es decir con mayor nivel de fama. El nivel de fama de un vocaloid se calcula como la fama total
que le dan los conciertos en los cuales puede participar multiplicado por la cantidad de canciones que sabe cantar. */
%FamaTotal=FamaDeConciertosQuePuedeParticipar*CantDeCanciones
elMasFamoso(Cantante):-nivelDeFama(Cantante,CantidadDeFama),forall(nivelDeFama(_,Fama),CantidadDeFama >= Fama).

nivelDeFama(Cantante,CantidadDeFama):-famaTotal(Cantante,Fama),cantidadDeCanciones(Cantante,Total),CantidadDeFama is Fama* Total.

famaTotal(Cantante,FamaTotal):-findall(Fama,participa(Cantante,Fama),ListaDeFama),sumlist(ListaDeFama,FamaTotal).

participa(Cantante,Fama):-,puedeParticipar(Cantante,Concierto),fama(Concierto,Fama).

fama(Concierto,Fama):-concierto(Concierto,_,Fama,_).

%4----------------------------------------------------------------------------------------------------------------
/*Queremos verificar si un vocaloid es el único que participa de un concierto,
esto se cumple si ninguno de sus conocidos ya sea directo o indirectos (en cualquiera de los niveles)
participa en el mismo concierto.*/

conoce(megurineLuka,hatsuneMiku).
conoce(megurineLuka,gumi).
conoce(gumi,seeU).
conoce(seeU,kaito).

unicoParticipante(Cantante,Concierto):-not(sonConocidos(Cantante,OtroCantante)),puedeParticipar(OtroCantante,Concierto),	puedeParticipar(Cantante,Concierto).


seConocen(A,B):-conoce(A,B).
sonConocidos(A,B):-seConocen(A,C),sonConocidos(C,B).


