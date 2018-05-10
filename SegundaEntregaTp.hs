module TP where
import Text.Show.Functions

-- punto 3.1
type Programa =[Instruccion]
type Instruccion = MicroControlador -> MicroControlador

data MicroControlador = UnMicro {
 memoria :: [Int],
 acumuladorA_B :: (Int,Int),
 programCounter :: Int,
 mensajeError :: String,
 listaDeProgrmas :: Programa} deriving (Show)
 
xt8088 = UnMicro { memoria = [] ,
					acumuladorA_B = (0,0) , 
					programCounter = 0 , 
					mensajeError = "" ,
					listaDeProgrmas = [] }
                
 
nop micro = micro { programCounter = programCounter micro +1 }

lod addr (UnMicro  m (a,b) pc f ldp ) = 	 UnMicro  m (((m)!!addr),0) (pc) f ldp

lodv addr (UnMicro  m (a,b) pc f ldp  ) = UnMicro  m (addr,b) (pc) f ldp

swap (UnMicro  m (a,b) pc f ldp ) = UnMicro  m (b,a) (pc) f ldp

add (UnMicro  m (a,b) pc f ldp)= UnMicro  m (a+b,0) (pc) f ldp

div2 (UnMicro  m (a,b) pc f ldp) | (not.(==0).snd) (a,b) = UnMicro m ((a `div` b),0) (pc) f ldp
                                 | otherwise = UnMicro m (a,b) (pc) "Division by Zero" ldp

str addr val (UnMicro  m a pc f ldp ) = UnMicro  (((++drop (addr-1) m).(++[val]).(take (addr-1))) m) a (pc+1) f ldp 
 
programa1:: Programa
programa2:: Programa
programa1 = [lodv 10,swap,lodv 22,add]
programa2 = [str 1 2,str 2 0,lod 2,swap,lod 1,div2]

cargar programa unMicro = unMicro {listaDeProgrmas = listaDeProgrmas unMicro ++ programa } 

ejecutarInstruccion funcion unMicro = (nop.funcion) unMicro

ejecutarPrograma micro = foldr (ejecutarInstruccion) micro (listaDeProgrmas micro)
