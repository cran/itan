% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itan.R
\name{analizarAlternativas}
\alias{analizarAlternativas}
\title{Análisis de alternativas}
\usage{
analizarAlternativas(respuestas, clave, alternativas, proporcion = 0.25)
}
\arguments{
\item{respuestas}{Un data frame con las alternativas seleccionadas por los
estudiantes en cada ítem.}

\item{clave}{Un data frame con las alternativas correctas para cada ítem.}

\item{alternativas}{Un vector con las alternativas posibles para cada ítem.}

\item{proporcion}{Proporción del total de estudiantes que constituyen los
grupos superior e inferior.}
}
\value{
Una lista en la cual cada elemento corresponde a un ítem. Para cada
ítem se calcula la frecuencia o proporción de las alternativas seleccionadas
por el grupo superior y por el grupo inferior de estudiantes.
}
\description{
Calcula la frecuencia o proporción de las alternativas seleccionadas por el
grupo superior e inferior de estudiantes en cada ítem.
}
\examples{
respuestas <- datos[,-1]
alternativas <- LETTERS[1:5]
analizarAlternativas(respuestas, clave, alternativas)

}
\references{
Morales, P. (2009). Análisis de ítem en las pruebas objetivas.
Madrid. Recuperado de \url{https://educrea.cl/wp-content/uploads/2014/11/19-nov-analisis-de-items-en-las-pruebas-objetivas.pdf}
}
\seealso{
\code{\link{calcularFrecuenciaAlternativas}}, \code{\link{datos}} y \code{\link{clave}}.
}
