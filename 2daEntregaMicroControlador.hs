-- ENTREGA 2 desde el punto 3.2

module TP where
import Text.Show.Functions

type Programa =[Instruccion]
type Instruccion = MicroC -> MicroC


 
data MicroC = UnMicro {
 memo :: [Int],
 acumA :: Int,
 acumB::Int,
 pCounter :: Int,
 mError :: String,
 listaDeProgramas::Programa} deriving (Show)

xt8088=UnMicro {
memo = [],
acumA =0,
acumB =0,
pCounter= 0,
mError="",
listaDeProgramas=[]}
  
-- punto 3.2

nop::MicroC->MicroC
nop micro = micro {pCounter = pCounter micro +1}


 
-- MAIN> (nop.nop.nop) xt8088


-- punto 3.3.3.1

lodv::Int->MicroC->MicroC
lodv val micro =  (micro {acumA= val})

swap::MicroC->MicroC
swap micro = (micro {acumA = acumB micro, acumB = acumA micro})
 
add::MicroC->MicroC
add micro = (micro {acumA = (acumA micro)+ (acumB micro), acumB=0})




-- punto 3.4.4.1 y 3.4.4.2
dividir::MicroC->MicroC
dividir unmic
 | acumB unmic == 0 = (unmic {mError="Division by Zero"})
 | otherwise = (unmic {acumA = (acumA unmic) `div` (acumB unmic),acumB=0}) 


lod::Int->MicroC->MicroC
lod addr unmicro = (unmicro {acumA = memo unmicro!!(addr-1)})


guardaValEnAddr addr val memoria= (take (addr-1) memoria)++[val]++(drop (addr) memoria)
str::Int->Int->MicroC->MicroC
str addr val unmicro = (unmicro {memo=guardaValEnAddr addr val (memo unmicro)})


programa1:: Programa
programa2:: Programa

programa1 = [lodv 10,swap,lodv 22,add]
programa2 = [dividir,lod 1,swap,lod 2, str 2 0,str 1 2]
cargar programa unmicro = (unmicro {listaDeProgramas = listaDeProgramas unmicro ++ programa}) 

ejecutarInstruccion funcion unMicro = (nop.funcion) unMicro

ejecutarPrograma micro = foldr (ejecutarInstruccion) micro (listaDeProgramas micro)
verResIntermedios micro = scanr (ejecutarInstruccion) micro (listaDeProgramas micro)

{-
*TP> (ejecutarPrograma.(cargar programa1)) xt8088
UnMicro {memo = [], acumA = [10], acumB = [22], pCounter = 5, mError = "", listaDeProgramas = [<function>,<function>,<function>,<function>]}
*TP> 
*TP> (ejecutarPrograma.(cargar programa2)) xt8088
UnMicro {memo = [2,0], acumA = [2], acumB = [0], pCounter = 7, mError = "Division by Zero", listaDeProgramas = [<function>,<function>,<function>,<function>,<function>,<function>]}
*TP> (dividir.lod 1.swap.lod 2.str 2 0.str 1 2) xt8088
UnMicro {memo = [2,0], acumA = [2], acumB = [0], pCounter = 6, mError = "Division by Zero", listaDeProgramas = []}
*TP> 
-}

progTest=[map (2*), filter odd, map (3+), filter (>3)]
lis = [1..10]

-- ENTREGA 2: 3.3.3 IFNZ


infz::MicroC->MicroC
infz unmicro
 |acumA unmicro /=0 = ejecutarPrograma unmicro 
 |otherwise = nop unmicro

 
xt8=UnMicro {
memo = [],
acumA =0,
acumB =0,
pCounter= 0,
mError="",
listaDeProgramas=[lodv 24.swap.lodv 3]}
{-
*TP> infz xt8
UnMicro {memo = [], acumA = [24], acumB = [3], pCounter = 3, mError = "", listaDeProgramas = [<function>]}
*TP> 
-}

-- punto 3.4.4 depuracion

programa3::Programa

programa3=[str 2 0, str 1 3, lodv 0, lodv 133,nop, swap]

xt88=UnMicro {
memo = [],
acumA =0,
acumB =0,
pCounter= 0,
mError="",
listaDeProgramas=[]}

condicion unMicro funcion = acumA (funcion unMicro) /= 0  ||  acumB (funcion unMicro) /= 0  ||  (memo (funcion unMicro) /= [0] && memo (funcion unMicro) /= [])
depurar unMicro  = unMicro {listaDeProgramas=filter (condicion unMicro) (listaDeProgramas unMicro)}

-- 3.5.5 Memoria Ordenada

memoOrdenada [x] = True
memoOrdenada [] = True
memoOrdenada (x:xs)=(x<(head xs) && memoOrdenada(xs) )||(x==(head xs) && memoOrdenada(xs))
testMemoOrdenada micro = memoOrdenada (memo micro) 

-- 3.6.6 Memoria Infinita

-- No se pueden ejecutar las operaciones, porque la memoria nunca termina de "formarse" ?
-- No se puede evaluar si la memoria esta ordenada, ya que nunca termina

-- 4 Casos de prueba

-- 4.2
prog42::Programa
prog42 = [add,lodv 10,swap, lodv 22]
{-
*TP> (ejecutarPrograma.cargar prog42) xt8088
UnMicro {memo = [], acumA = 32, acumB = 0, pCounter = 4, mError = "", listaDeProgramas = [<function>,<function>,<function>,<function>]}
*TP> 
-}
prog421::Programa
prog421=[dividir,lod 1,swap,lod 2,str 2 0,str 1 2]
{-
*TP> (ejecutarPrograma.cargar prog421) xt8088
UnMicro {memo = [2,0], acumA = 2, acumB = 0, pCounter = 6, mError = "Division by Zero", listaDeProgramas = [<function>,<function>,<function>,<function>,<function>,<function>]}
*TP> 
-}

-- punto 4.3
fp200=UnMicro {
memo = [],
acumA =7,
acumB =24,
pCounter= 0,
mError="",
listaDeProgramas=[]}

prog430::Programa
prog430=[swap,lodv 3]
{-
*TP> (infz.cargar prog430) fp200
UnMicro {memo = [], acumA = 24, acumB = 3, pCounter = 2, mError = "", listaDeProgramas = [<function>,<function>]}
*TP> (infz.cargar prog430) xt8088
UnMicro {memo = [], acumA = 0, acumB = 0, pCounter = 1, mError = "", listaDeProgramas = [<function>,<function>]}
*TP> 
-}

-- 4.4 Depuracion
{-
*TP> (ejecutarPrograma.depurar.cargar programa3) xt88
UnMicro {memo = [3], acumA = 133, acumB = 0, pCounter = 2, mError = "", listaDeProgramas = [<function>,<function>]}
*TP> 
-}

-- 4.5 Orden de la memoria

at86=UnMicro {
memo = [1,2,3,4,5,6],
acumA =0,
acumB =0,
pCounter= 0,
mError="",
listaDeProgramas=[]}

microDesorden=UnMicro {
memo = [2,5,1,0,6,9],
acumA =0,
acumB =0,
pCounter= 0,
mError="",
listaDeProgramas=[]}

{-
*TP> testMemoOrdenada at86
True
*TP> testMemoOrdenada xt8088
True
*TP> testMemoOrdenada microDesorden
False
*TP> 
-}
