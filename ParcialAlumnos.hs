--Parcial Alumnos
import Text.Show.Functions

type Nombre = String
type Horas = Float
type MateriaAprobada = Alumno -> Alumno
type Conceptos = String
type CantReentrega = Float

data Alumno = Alumno {nombre::Nombre,dedicacion::Horas,materiasAprobadas::[MateriaAprobada],conceptos::[Conceptos]}deriving Show
santi = Alumno "Santiago" 0 [] []

agregarConcepto concepto (Alumno nombre horas materias conceptos) = Alumno nombre horas materias (concepto:conceptos)
aumentarDedicacion horitas (Alumno nombre horas materias conceptos) = Alumno nombre (horitas+horas) materias conceptos
agregarMateriaAprobada materia (Alumno nombre horas materias conceptos) = Alumno nombre horas (materia:materias) conceptos
agregarPrefijo prefijo (Alumno nombre horas materias conceptos) = Alumno (prefijo++nombre) horas materias conceptos

aprobar :: MateriaAprobada -> Alumno -> Alumno
aprobar materia = agregarMateriaAprobada materia . materia
paradigmas :: MateriaAprobada
paradigmas = aumentarDedicacion 100 . agregarConcepto "orden superior" . agregarConcepto "polimorfismo"

sistemasOperativos :: CantReentrega -> Alumno -> Alumno
sistemasOperativos reentregas = agregarPrefijo "Excelentísim@ ".aumentarDedicacion (1000*reentregas)
