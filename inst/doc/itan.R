## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ---- setup-------------------------------------------------------------------
library(itan)

## -----------------------------------------------------------------------------
datos.csv <- system.file("extdata", "datos.csv", package = "itan", mustWork = TRUE)
datos <- read.csv(datos.csv, na = c("*"))

clave.csv <- system.file("extdata", "clave.csv", package = "itan", mustWork = TRUE)
clave <- read.csv(clave.csv)

## -----------------------------------------------------------------------------
head(datos)

## -----------------------------------------------------------------------------
clave

## ----paged.print=TRUE---------------------------------------------------------
respuestas <- datos[,-1]
respuestasCorregidas <- corregirRespuestas(respuestas, clave)
head(respuestasCorregidas)

## ----paged.print=TRUE---------------------------------------------------------
puntaje <- calcularPuntajes(respuestasCorregidas)
nota <- calcularNotas(puntaje)
resultados <- cbind(id=datos$id, puntaje, nota)
resultados <- resultados[order(resultados[,3], decreasing = TRUE),]
head(resultados)
summary(nota)

## -----------------------------------------------------------------------------
p <- calcularIndiceDificultad(respuestasCorregidas, proporcion = 0.25)
p

## -----------------------------------------------------------------------------
# Se toma el 27% de los estudiantes con mejor y peor desempeño
# para conformar los grupos superior e inferior.
dc1 <- calcularIndiceDiscriminacion(respuestasCorregidas, tipo = "dc1", proporcion = 0.25) 
dc1

## -----------------------------------------------------------------------------
dc2 <- calcularIndiceDiscriminacion(respuestasCorregidas, tipo = "dc2", proporcion = 0.25)
dc2

## -----------------------------------------------------------------------------
indices <- cbind(p, dc1, dc2)
head(indices)

## -----------------------------------------------------------------------------
alternativas <- LETTERS[1:5]
pb <- pBis(respuestas, clave, alternativas)
head(pb)

## -----------------------------------------------------------------------------
alternativas <- LETTERS[1:5]
fa <- calcularFrecuenciaAlternativas(respuestas, alternativas, clave, frecuencia = FALSE)
head(fa)

## ----fig.show='hold', fig.dim=c(3, 3), out.width="45%"------------------------
g <- graficarFrecuenciaAlternativas(respuestas, alternativas, clave)
g$i01
g$i02
g$i03
g$i04

## -----------------------------------------------------------------------------
ad <- analizarAlternativas(respuestas, clave, alternativas, proporcion = 0.25)
ad$i01
ad$i25
ad$i50

## ----out.height="100%"--------------------------------------------------------
item <- agi(respuestas, clave, alternativas)

item$i01$datos
item$i25$datos
item$i50$datos


## ----fig.show='hold', fig.dim=c(7,5)------------------------------------------
item$i01$plot
item$i25$plot
item$i50$plot

