module TP where
import Text.Show.Functions

-- punto 3.1
type Programa =[Instruccion]
type Instruccion = MicroControlador -> MicroControlador

data MicroControlador = UnMicro {
 memoria :: [Int],
 acumA ::Int,
 acumB :: Int,
 programCounter :: Int,
 mensajeError :: String,
 listaDeInstrucciones :: Programa} deriving (Show)
 
fp20 = UnMicro {
memoria = [],
acumA = 7,
acumB = 24,
programCounter = 0,
mensajeError = "",
listaDeInstrucciones = [] }  
 
xt8088 = UnMicro { memoria = [0] ,
					acumA = 0, 
					acumB = 0,
					programCounter = 0 , 
					mensajeError = "" ,
					listaDeInstrucciones = [swap,nop,lodv 133,lodv 0,str 1 3,str 2 0]  }
                --str 2 0,str 1 3,lodv 0,lodv 133,nop,swap
xt8 = UnMicro {
memoria = [],
acumA = 0,
acumB = 0,
programCounter = 0,
mensajeError = "",
listaDeInstrucciones = [lodv 24.swap.lodv 3] }				
 
nop micro = micro { programCounter = programCounter micro +1 }

lod addr unMicro = unMicro {acumA = (memoria unMicro) !! (addr-1)}

lodv val unMicro = unMicro {acumA = val}

swap unMicro = unMicro {acumA = acumB unMicro , acumB = acumA unMicro}

add unMicro = unMicro {acumA = (acumA unMicro)+ (acumB unMicro), acumB=0}

div2 unMicro | acumB unMicro == 0 = unMicro {mensajeError="Division by Zero"}
			 | otherwise = unMicro {acumA = (acumA unMicro)`div` (acumB unMicro) , acumB=0}

str addr val (UnMicro  m a b pc f ldp ) = UnMicro  (((++drop (addr-1) m).(++[val]).(take (addr-1))) m) a b pc f ldp 


--3.1 Punto 1: Carga de un programa			 
programa1:: Programa
programa2:: Programa
programa1 = [lodv 10,swap,lodv 22,add]
programa2 = [str 1 2,str 2 0,lod 2,swap,lod 1,div2]
programa3 = [str 2 0,str 1 3,lodv 0,lodv 133,nop,swap]


--3.2 Punto 2: Ejecución de un programa
cargar programa unMicro = unMicro {listaDeInstrucciones = listaDeInstrucciones unMicro ++ programa } 

ejecutarInstruccion funcion unMicro = (nop.funcion) unMicro

ejecutarPrograma micro = foldr (ejecutarInstruccion) micro (listaDeInstrucciones micro)


--3.3 Punto 3: IFNZ
ifnz::MicroControlador->MicroControlador
ifnz unMicro |acumA unMicro /= 0 = ejecutarPrograma unMicro 
			 |otherwise = nop unMicro


--3.4 Punto 4: Depuración de un programa			
condicion unMicro funcion = acumA (funcion unMicro) /= 0  ||  acumB (funcion unMicro) /= 0  ||  memoria (funcion unMicro) == [] 						   
depurar unMicro  = unMicro { listaDeInstrucciones = filter (condicion unMicro) (listaDeInstrucciones unMicro) } 


-- 3.5.5 Memoria Ordenada
memoOrdenada [x] = True
memoOrdenada [] = True
memoOrdenada (x:xs)=(x<(head xs) && memoOrdenada(xs) )||(x==(head xs) && memoOrdenada(xs))
testMemoOrdenada micro = memoOrdenada (memoria micro) 


-- 3.6.6 Memoria Infinita
-- No se pueden ejecutar las operaciones, porque la memoria nunca termina de "formarse" ?
-- No se puede evaluar si la memoria esta ordenada, ya que nunca termina
