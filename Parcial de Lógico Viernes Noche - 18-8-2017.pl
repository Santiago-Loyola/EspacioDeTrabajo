%MAL PLANTEADO LA BASE DE CONOCIMIENTOS
cantante(megurineluka,nightFever).
cantante(megurineluka,foreverYoung).
cantante(hatsuneMiku,tellYourWorld).
cantante(gumi,foreverYoung).
cantante(gumi,tellYourWorld).
cantante(seeU,novemberRain).
cantante(seeU,nightFever).
tema(nightFever,4).
tema(foreverYoung,5).
tema(tellYourWorld,4).
tema(foreverYoung,4).
tema(tellYourWorld,5).
tema(novemberRain,6).
tema(nightFever,5).


%Definir los siguientes predicados que sean totalmente inversibles, a menos que se indique lo contrario.

%1)
esNovedoso(Cantante):-sabeAlMenos2Canciones(Cantante),tiempoTotal(Cantante).
sabeAlMenos2Canciones(Cantante):-cantante(Cantante,_),findall(Tema,cantante(Cantante,Tema),Canciones),length(Canciones,Cantidad),Cantidad>=2.
tiempoTotal(Cantante):-cantante(Cantante,Tema),findall(Tiempo,tema(Tema,Tiempo),ListaDeTiempo),sumlist(ListaDeTiempo,Numero),Numero < 15.

/*
Hay algunos vocaloids que simplemente no quieren cantar canciones largas porque
no les gusta, es por eso que se pide saber si un cantante es acelerado, condición
que se da cuando todas sus canciones duran 4 minutos o menos. Resolver sin usar forall/2.
*/
esAcelerado(Cantante):-cantante(Cantante,Tema),tema(Tema,Tiempo),findall(Tiempo,(cantante(Cantante,Tema),tema(Tema,Tiempo)),Tiempos),max_member(Numero,Tiempos),Numero =< 4
