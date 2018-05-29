--Parcial 12/05/2016 Polimorfismo
import Text.Show.Functions

type Nombre = String
type Sentimiento = String
type Estrategia = Persona -> Persona
type Numero = Int
data Persona = Vendedor Nombre Estrategia [Sentimiento] | Comprador Nombre [Sentimiento] deriving Show

agradecimiento::Estrategia
agradecimiento = sentir "placer"
defensaAlConsumidor :: Estrategia
defensaAlConsumidor = sentir "tristeza" . sentir "miedo"
juicio :: Int -> Estrategia
juicio cantidadDeAbogados = sentir "depresión" . seguidilla cantidadDeAbogados

niFuNiFa = id

loquito = Comprador "Loquito" ["molestia", "bronca", "ira asesina"]
jorgito = Comprador "Jorge" ["molestia"]
tincho = Comprador "Martín" ["indiferencia", "molestia"]
agus = Comprador "Agustín" ["felicidad"]
lucas = Vendedor "Lucas" gauchada ["felicidad"]
pato = Vendedor "Patricio" (fraudeOlímpico . niFuNiFa) ["bronca", "felicidad"]
flor = Vendedor "Florencia" (estafa . fraudeOlímpico) []
nacho = Vendedor "Ignacio" (estafa . seguidilla 10 . niFuNiFa) ["molestia"]

--------------------------------------------------------------------------------
nombre ( Comprador nom _ ) = nom
nombre ( Vendedor nom _ _ ) = nom
estrategia (Vendedor _ estra _ ) = estra
sentimientos (Comprador _ senti) = senti
sentimientos (Vendedor _ _ senti) = senti

cambiarNombre nuevoNombre (Comprador nombre sentimiento) = Comprador nuevoNombre sentimiento
cambiarNombre nuevoNombre (Vendedor nombre estrategia sentimiento ) = Vendedor nuevoNombre estrategia sentimiento

nuevoSentimiento nuevo (Comprador nombre sentimiento) = Comprador nombre (nuevo:sentimiento)
nuevoSentimiento nuevo (Vendedor nombre estrategia sentimiento) = Vendedor nombre estrategia (nuevo:sentimiento)

--1a
sentir :: Sentimiento -> Persona -> Persona
sentir sentimiento persona = nuevoSentimiento sentimiento persona

--1b
gauchada :: Estrategia
gauchada comprador = nuevoSentimiento "satisfaccion" comprador

--1c
estafa,fraudeOlímpico :: Estrategia
estafa comprador = (nuevoSentimiento "bronca" . nuevoSentimiento "felicidad") comprador

fraudeOlímpico comprador = (nuevoSentimiento "ira asesina" . estafa) comprador

--1d
seguidilla :: Numero -> Estrategia
seguidilla 0 persona = persona
seguidilla numero persona = (seguidilla (numero-1) . nuevoSentimiento "molestia") persona

--2a
sentimientosMalos = ["molestia", "bronca", "ira asesina"]
seSienteBien :: Persona -> Bool
seSienteBien persona = all condicion (sentimientos persona)
condicion sentimiento = not (elem sentimiento sentimientosMalos)

--2b
seSienteMal :: Persona -> Bool
seSienteMal persona = not (seSienteBien persona)

--2c
elementosÚnicos [] = []
elementosÚnicos (x:xs) | (not . elem x) xs = x : elementosÚnicos xs
                       | otherwise = elementosÚnicos xs
quiereMatarATodos::Persona -> Bool
quiereMatarATodos persona = seSienteMal persona && 3 <= ( length . elementosÚnicos . sentimientos) persona && elem "ira asesina" (sentimientos persona)

--3

--raccionar :: Estrategia -> Estrategia
vender :: Persona -> Persona -> Persona
cantidad :: Persona -> Persona -> Int

vender comprador vendedor =  (estrategia vendedor) comprador
cantidad comprador vendedor = (length . sentimientos .vender comprador ) vendedor
raccionar :: Persona -> Persona -> Estrategia
raccionar comprador vendedor | (quiereMatarATodos . vender comprador ) vendedor = juicio (cantidad comprador vendedor)
                             | (seSienteBien.vender comprador) vendedor = agradecimiento
                             | otherwise = defensaAlConsumidor

--4
comparar lista = "placer" == (!!) lista 0
comparar2 sentimiento lista = sentimiento == (!!) lista 0
sienteDepresiónLuegoDeVenta comprador vendedor = (comparar2 "depresion" . sentimientos . raccionar comprador vendedor) vendedor
sienteTristezaLuegoDeVenta comprador vendedor = (comparar2 "tristeza" . sentimientos . raccionar comprador vendedor) vendedor
sientePlacerLuegoDeVenta comprador vendedor = (comparar . sentimientos . raccionar comprador vendedor) vendedor

ventaElite :: Persona -> [Persona] -> [Persona]
condicion2 comprador vendedor =  sientePlacerLuegoDeVenta comprador vendedor
ventaElite comprador vendedor = filter (condicion2 comprador) vendedor
