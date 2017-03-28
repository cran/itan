\name{itan}
\alias{itan}
\alias{itan}
\docType{package}
\title{Análisis y calificación de pruebas objetivas.}
\description{
El paquete itan incluye funciones para analizar los ítems del test y sus distractores así como para calcular el puntaje y la calificación obtenido por estudiantes en una prueba objetiva.
}
\details{
\tabular{ll}{
  Package: \tab itan\cr
  Type: \tab Package\cr
  Version: \tab 1.0\cr
  Depends: \tab ggplot2, reshape\cr
  Date: \tab 2017-03-28\cr
  License: \tab GPL (>= 2)
  Repository: CRAN
  }
Se incluye dos datos simulados para probar las funciones del paquete. La hoja de datos \code{\link{datos}} contiene las respuestas seleccionadas por 100 estudiantes en una prueba objetiva de 20 ítems de selección múltiple. Los estudiantes están individualizados por su RUN (Rol Único Nacional). Por otro lado, la hoja de datos \code{\link{clave}} contiene las alternativas correctas para los ítems del test.

La función \code{\link{corregirItems}} permite determinar si las alternativas seleccionadas por los estudiantes son correctas o incorrectas. Se asigna un 1 si la respuesta es correcta y un 0 si es incorreta. La matriz binaria devuelta por esta función puede ser utilizada por la función \code{\link{calcularPuntajes}} para determinar el puntaje obtenido en la prueba y a partir de éste calcular su respectiva calificación con la función \code{\link{calcularNotas}}. Esta última función utiliza el sistema de calificación usado en Chile: notas de 1.0 a 7.0 con nota de aprobación 4.0 y nivel de exigendia del 60\%.

La matriz binaria devuelta por la función \code{\link{corregirItems}} también puede ser usada para calcular el índice de dificultad y dos tipos de índices de discriminación con la función \code{\link{analizarItems}}.

Las respuestas de los estudiantes sin procesar junto con la clave de corrección pueden utilizarse para hacer dos tipos de análisis de distractores con las funciones \code{\link{analizarDistractores}} y \code{\link{frecuenciaDistractores}}. También se puede calcular la correlación biserial puntual de cada alternativa con respecto al puntaje obtenido en la prueba con la función \code{\link{pBis}}.

Por último, con la función \code{\link{agi}} se puede analizar gráficamente la frecuencia de estudiantes que seleccionó cada alternativa en función de su desempeño en la prueba.
}
\author{Ariel Armijo Quiroz <arielarmijo@yahoo.es>}
\references{
Morales, P. (2009). Análisis de ítem en las pruebas objetivas. Madrid. Recuperado de \url{https://educrea.cl/wp-content/uploads/2014/11/19-nov-analisis-de-items-en-las-pruebas-objetivas.pdf}

Ensenada, B. (2010). Manual para el análisis gráfico de ítems. Baja California. Recuperado de \url{http://www.educacionbc.edu.mx/departamentos/evaluacion/eacademicos/archivos/jornadasBC/MANUAL_PAGI.pdf}
}
\keyword{package}
\examples{
data(datos)
data(clave)
# Muestra los data frames incorporados en el paquete itan.
head(datos)
clave

# Selecciona las respuestas de los estudiantes a cada ítem.
respuestas <- subset(datos, select = -id )

# Calcula el puntaje y asigna una calificación.
respCorregidas <- corregirItems(respuestas, clave)
puntajes <- calcularPuntajes(respCorregidas)
notas <- calcularNotas(puntajes)
cbind(datos$id,pje=puntajes, nota=notas)

# Muestra información sobre la distribución de las notas.
summary(notas)

# Calcula el índice de dificultad y de discriminación y
# realiza análisis de distractores.
items <- analizarItems(respCorregidas)
distractores <- frecuenciaDistractores(respuestas, clave, frecuencia = FALSE)
data.frame(items, distractores)
analizarDistractores(respuestas, clave)

# Calcula la correlación biserial puntual de los distractores.
pBis(respuestas, clave)

# Realiza el análisis gráfico de ítems.
 plots <- agi(respuestas, clave, ncol(respuestas)/2)
 plots[[1]][[1]]
}
