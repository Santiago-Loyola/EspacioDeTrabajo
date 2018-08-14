%MAL PLANTEADO LA BASE DE CONOCIMIENTOS
cantante(megurineluka,nightFever,4).
cantante(megurineluka,foreverYoung,5).
cantante(hatsuneMiku,tellYourWorld,4).
cantante(gumi,foreverYoung,4).
cantante(gumi,tellYourWorld,5).
cantante(seeU,novemberRain,6).
cantante(seeU,nightFever,5).

%Definir los siguientes predicados que sean totalmente inversibles, a menos que se indique lo contrario.

%1)
esNovedoso(Cantante):-sabeAlMenos2Canciones(Cantante),tiempoTotal(Cantante).

sabeAlMenos2Canciones(Cantante):-cantante(Cantante,_,_),findall(Tema,cantante(Cantante,Tema,_),Canciones),length(Canciones,Cantidad),Cantidad>=2.
tiempoTotal(Cantante):-cantante(Cantante,Tema,_),findall(Tiempo,cantante(Cantante,Tema,Tiempo),ListaDeTiempo),sumlist(ListaDeTiempo,Numero),Numero < 15.

/*
Hay algunos vocaloids que simplemente no quieren cantar canciones largas porque
no les gusta, es por eso que se pide saber si un cantante es acelerado, condición
que se da cuando todas sus canciones duran 4 minutos o menos. Resolver sin usar forall/2.
*/
esAcelerado(Cantante):-cantante()findall(Tiempo,cantante(Cantante,_,Tiempo),Tiempos),max_member(Numero,Tiempos),4 >= Numero.
