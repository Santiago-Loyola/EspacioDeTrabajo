--Parcial Alumnos
import Text.Show.Functions

type Nombre = String
type Horas = Float
type MateriaAprobada = Alumno -> Alumno
type EfectoAlumno = Alumno -> Alumno
type Conceptos = String
type CantReentrega = Float

data Alumno = Alumno {nombre::Nombre,dedicacion::Horas,materiasAprobadas::[MateriaAprobada],conceptos::[Conceptos]}deriving Show
data Materia = Materia {asignatura::Nombre, efecto::EfectoAlumno } deriving Show

santi = Alumno "Santiago" 0 [] []

paradigmas = Materia {
asignatura = "paradigmas",
efecto = (aumentarDedicacion 100 . aprenderConcepto "orden superior" . aprenderConcepto "polimorfismo") }

sistemasOperativos reentregas = Materia {
asignatura = "sistemas Operativos",
efecto = agregarPrefijo "Excelentísim@ ".aumentarDedicacion (1000*reentregas)}

recursividadAFull = Materia {
asignatura = "Recursividad a Full",
efecto = nuevoConcepto recursividad
}


nuevoConcepto sigConcepto alumno = alumno {conceptos = sigConcepto}
aprenderConcepto unConcepto alumno = nuevoConcepto (unConcepto: conceptos alumno) alumno

aumentarDedicacion horitas (Alumno nombre horas materias conceptos) = Alumno nombre (horitas+horas) materias conceptos
agregarMateriaAprobada materia (Alumno nombre horas materias conceptos) = Alumno nombre horas (materia:materias) conceptos
agregarPrefijo prefijo (Alumno nombre horas materias conceptos) = Alumno (prefijo++nombre) horas materias conceptos

aprobar :: MateriaAprobada -> Alumno -> Alumno
aprobar materia = agregarMateriaAprobada materia . materia
paradigma :: MateriaAprobada
paradigma = aumentarDedicacion 100 . aprenderConcepto "orden superior" . aprenderConcepto "polimorfismo"

sistemasOperativo :: CantReentrega -> Alumno -> Alumno
sistemasOperativo reentregas = agregarPrefijo "Excelentísim@ ".aumentarDedicacion (1000*reentregas)


recursividad = map  ((++) "Recursividad " .show) [1..]
