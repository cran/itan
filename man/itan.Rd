% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itan.R
\docType{package}
\name{itan}
\alias{itan}
\title{itan: Paquete para el análisis de ítems de pruebas objetivas}
\description{
El paquete itan incluye funciones que permiten calcular el puntaje y calificación
obtenido por estudiantes en una prueba objetiva. Además, incorpora funciones para
analizar los ítems del test y sus distractores. Entre estos últimos destaca el
\href{http://www.educacionbc.edu.mx/departamentos/evaluacion/eacademicos/archivos/jornadasBC/MANUAL_PAGI.pdf}{análisis gráfico de ítems}
que permite visualizar las características técnicas del ítem y determinar
rápidamente su calidad.
}
\details{
El paquete itan incluye datos para probar las funciones del paquete.
El data frame \code{\link{datos}} contiene las respuestas seleccionadas
por 39 estudiantes en una prueba objetiva de 50 ítems de selección múltiple.
Las alternativas posibles a cada ítem son 'A', 'B', 'C', 'D' y 'E', mientras
que las respuestas omitidas se indican mediante un '*'.
Cada estudiante tiene asociado un id único que figura en la columna 1 del data
frame. Las columnas que representan los ítems están rotuladas como 'i01',
'i02', ..., 'i50'.
Por otro lado, el data frame \code{\link{clave}} contiene las alternativas
correctas para cada ítem de la prueba.
}
\section{Funciones del paquete itan}{


La función \code{\link{corregirRespuestas}} permite determinar si las alternativas
seleccionadas por los estudiantes son correctas o incorrectas. Se asigna un 1
si la respuesta es correcta y un 0 si es incorrecta. El data frame con valores
binarios devuelto por esta función puede ser utilizado por la función
\code{\link{calcularPuntajes}} para determinar el puntaje obtenido en la prueba.

A partir de los puntajes obtenidos en la prueba se puede calcular la calificación
de cada estudiante con la función \code{\link{calcularNotas}}. Esta última función
utiliza el \href{https://es.wikipedia.org/wiki/Calificaciones_escolares_en_Chile}{sistema de calificación usado en Chile}:
notas de 1.0 a 7.0, con nota de aprobación 4.0 y nivel de exigencia del 60\\%.

El data frame binario devuelto por la función \code{\link{corregirRespuestas}}
también puede ser usado para calcular el índice de dificultad y dos tipos de
índices de discriminación. Estas funciones son \code{\link{calcularIndiceDificultad}}
y \code{\link{calcularIndiceDiscriminacion}}, respectivamente.

Las respuestas de los estudiantes sin procesar, junto con la clave de corrección,
pueden utilizarse para hacer dos tipos de análisis de distractores con las funciones
\code{\link{calcularFrecuenciaAlternativas}} y \code{\link{analizarAlternativas}}.
También se puede calcular la correlación biserial puntual de cada alternativa
con respecto al puntaje obtenido en la prueba con la función \code{\link{pBis}}.

Por último, con la función \code{\link{agi}} se puede analizar gráficamente la
frecuencia de estudiantes que seleccionó cada alternativa en función de su
desempeño en la prueba. Esta función devuelve una lista con los datos y gráficos
generados para cada ítem. La inspección de las gráficas permite rápidamente determinar
la calidad del ítem.
}

\references{
Morales, P. (2009). Análisis de ítem en las pruebas objetivas. Madrid.
Recuperado de \href{https://educrea.cl/wp-content/uploads/2014/11/19-nov-analisis-de-items-en-las-pruebas-objetivas.pdf}{análisis de ítems}

Guadalupe de los Santos (2010). Manual para el análisis gráfico de ítems. Universidad Autónoma de Baja California.
Recuperado de \href{http://www.educacionbc.edu.mx/departamentos/evaluacion/eacademicos/archivos/jornadasBC/MANUAL_PAGI.pdf}{manual_pagi.pdf}
}