--Cacería Monstruosa
import Text.Show.Functions

--Los personajes

espadaOxidada = (1.2*)
katanaFilosa  = (10+).(0.9*)
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x= x* 2
espadaMaldita = espadaOxidada.sableLambdico 89

type Número = Float
type Elemento = Número -> Número

data Personaje = Personaje {
lvl::Número,
expe::Número,
capacidadParaCaza::Número,
fuerzaBásica :: Número,
item::Elemento} deriving Show

jP = Personaje 6 30 0 1 katanaFilosa
tala = Personaje 5 25 0 1 (sableLambdico 300)
atu = Personaje 4 20 0 5 baculoDuplicador

suCapacidadParaCaza :: Personaje -> Personaje
suCapacidadParaCaza personaje = personaje { capacidadParaCaza =  (item personaje) (fuerzaBásica personaje) }

suNivel :: Personaje -> Personaje
suNivel personaje = personaje { lvl = (expe personaje^2)/expe personaje + 1 }

--Alquimistas
aprendiz :: Personaje -> Personaje
aprendiz personaje = personaje {item =  (2*(item personaje))}
{-fusionar :: Número
fusionar item = 2*(item)
-}
