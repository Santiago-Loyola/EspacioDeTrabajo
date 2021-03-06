{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.
import Test.Hspec -- Para poder usar los tests que se piden más abajo (ponerlo luego de instalar hspec!!)


data Usuario = Usuario {nombre::String, billetera::Billetera} deriving (Show,Eq)

pepe = Usuario "José" 10
lucho = Usuario "Luciano" 2
pepe2 = Usuario "José" 20

--Eventos
type Billetera = Float
type Evento = Billetera -> Billetera


depositar :: Billetera -> Evento
depositar cantBille cant = cantBille + cant

extraccion :: Billetera -> Evento
extraccion cantARetirar =  max 0 . depositar (-cantARetirar)

upGrade :: Evento
upGrade cantBille = cantBille +  min 10 ((*) 0.2 cantBille)

cierreDeCuenta :: Evento
cierreDeCuenta cantBille = 0

quedaIgual :: Evento
quedaIgual  = id

ejecutarTest = hspec $ do
  describe "Pruebas de los eventos con una billetera de saldo 10." $ do
  it "1 - Al depositar 10, queda con 20." $ depositar 10 10 `shouldBe` 20
  it "2 - Extraer 3: Debería quedar con 7." $ extraccion 3 10 `shouldBe` 7
  it "3 - Extraer 15: Debería quedar con 0." $ extraccion 15 10 `shouldBe` 0
  it "4 - Un upgrade: Debería quedar con 12." $ upGrade 10 `shouldBe` 12
  it "5 - Cerrar la cuenta: 0." $ cierreDeCuenta 10 `shouldBe` 0
  it "6 - Queda igual: 10." $ quedaIgual 10 `shouldBe` 10
  it "7 - Depositar 1000, y luego tener un upgrade: 1020." $ upGrade (depositar 10 1000) `shouldBe` 1020

--Usuarios
ejecutarTestUsuarios = hspec $ do
  describe "Prueba sin definir nuevas funciones." $ do
    it "8 - ¿Cuál es la billetera de Pepe? Debería ser 10 monedas." $ billetera pepe `shouldBe` 10
    it "9 - ¿Cuál es la billetera de Pepe, luego de un cierre de su cuenta? Debería ser 0." $ (cierreDeCuenta . billetera) pepe `shouldBe` 0
    it "10 - ¿Cómo quedaría la billetera de Pepe si le depositan 15 monedas, extrae 2, y tiene un Upgrade? Debería quedar en 27.6." $ (upGrade . extraccion 2 . depositar 15 . billetera) pepe `shouldBe` 27.6

--Transacciones
compararUsuario :: Usuario -> Usuario -> Bool
compararUsuario usuario otroUsuario = nombre usuario == nombre otroUsuario

crearTransacción :: Usuario -> Evento -> Transaccion
crearTransacción usuario evento otroUsuario | compararUsuario usuario otroUsuario = evento
                                            | otherwise = quedaIgual

pagoEntreUsuarios :: Usuario -> Billetera -> Usuario -> Transaccion
pagoEntreUsuarios extraccionAlUsuario moneda depositarAlUsuario usuarioAComparar | compararUsuario extraccionAlUsuario usuarioAComparar = extraccion moneda
                                                                                 | compararUsuario depositarAlUsuario usuarioAComparar = depositar moneda
                                                                                 | otherwise = quedaIgual


type Transaccion = Usuario -> Evento
transaccion1, transaccion2, transaccion3, transaccion4,transaccion5 :: Transaccion


transaccion1 = crearTransacción lucho cierreDeCuenta  --usuario y monto pattern matching
transaccion2 = crearTransacción pepe (depositar 5)
transaccion3 = crearTransacción lucho tocoYMeVoy
transaccion4 = crearTransacción lucho ahorranteErrante
transaccion5 = pagoEntreUsuarios pepe 7 lucho

ejecutarTestTransacción = hspec $ do
  describe "Consultar lo siguiente sin definir nuevas funciones." $ do
    it "11 ..." $ transaccion1 pepe 20 `shouldBe` 20
    it "12 ..." $ transaccion2 pepe 10 `shouldBe` 15
    it "13 ..." $ transaccion2 pepe 50 `shouldBe` 55
    it "14 ..." $ transaccion3 lucho 10 `shouldBe` 0
    it "15 ..." $ transaccion4 lucho 10 `shouldBe` 34

--Nuevos Eventos
tocoYMeVoy :: Evento
tocoYMeVoy = cierreDeCuenta.upGrade .depositar 15
ahorranteErrante :: Evento
ahorranteErrante = depositar 10.upGrade.depositar 8.extraccion 1.depositar 2.depositar 1
