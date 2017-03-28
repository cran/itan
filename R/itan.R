#
#                   ANALISIS DE ITEMS PARA PRUEBAS OBJETIVAS.
#
# El paquete itan incluye funciones que permiten calcular el puntaje y calificacion
# obtenido por estudiantes en una prueba objetiva. Ademas incorpora funciones para
# analizar los items del test y sus distractores.

# Funcion interna para comprobar que la clave tenga la estructura adecuada.
validarClave <- function(respuestas, clave){
  # verifica que la clave sea una matriz u hoja de datos.
  if (is.vector(clave)){
    clave <- as.data.frame(matrix(clave, nrow=1), stringsAsFactors=FALSE)
    colnames(clave) <- colnames(respuestas)
  }
  # verifica que el numero de items coincida entre las respuestas y la clave.
  if (ncol(respuestas)!=ncol(clave)){
    stop("El n\u00FAmero de \u00EDtems no coincide entre los argumentos.")
  }
  # Verifica que la clave tenga una sola fila
  if (nrow(clave)>1){
    stop("Hay m\u00E1s de una clave")
  }
  return(clave)
}

#' @title Correci\enc{ó}{o}n de \enc{í}{i}tems.
#' @description Transforma una matriz con las alternativas seleccionadas por
#' los estudiantes en cada \enc{í}{i}tem en una matriz binaria (correcto/incorrecto) segun
#' la clave de correcci\enc{ó}{o}n.
#' @param respuestas Una matriz con las alternativas seleccionadas por los
#' estudiantes a cada \enc{í}{i}tem
#' @param clave Una matriz con la alternativa correcta para cada \enc{í}{i}tem.
#' @return Una matriz binaria con los aciertos (1) o errores (0) de
#' cada estudianta en cada \enc{í}{i}tem.
#' @seealso \code{\link{analizarItems}}
#' @export
#' @examples
#' data(datos)
#' data(clave)
#' respuestas <- subset(datos, select = -id )
#' corregirItems(respuestas, clave)
#'
corregirItems <- function(respuestas, clave){
  # Verifica que la clave tenga una estructura apropiada.
  clave <- validarClave(respuestas, clave)
  # Los datos NA se transforman a 0.
  respuestas[is.na(respuestas)] <- 0
  # Crea una matriz vacia para contener el resultado y nombra sus indices.
  output <- matrix(NA, nrow(respuestas), ncol(respuestas))
  colnames(output) <- colnames(respuestas)
  rownames(output) <- rownames(respuestas)
  # Realiza la correccion por columna( item por item). Esto es mas efectivo que hacerlo por
  # fila en caso de haber muchos estudiantes.
  for (i in 1:ncol(respuestas)){
    output[,i] <- (respuestas[,i] == clave[1,i]) * 1
  }
  # Devuelve la matriz binaria.
  return(output)
}

#' @title An\enc{á}{a}lisis de \enc{í}{i}tems.
#' @description Calcula el \enc{í}{i}ndice de dificultad y dos tipos de \enc{í}{i}ndices de
#' discriminaci\enc{ó}{o}n para cada \enc{í}{i}tem a partir de una matriz de respuestas binaria.
#' @param respCorregidas Una matriz con las respuestas de los estudiantes a cada \enc{í}{i}tem
#' en forma binaria.
#' @param proporcion Proporci\enc{ó}{o}n de estudiantes que forman parte de los grupos superior
#' e inferior. Valores habituales son 0.25, 0.27 y 0.33.
#' @param digitos La cantidad de d\enc{í}{i}gitos significativos que tendr\enc{á}{a} el resultado.
#' @return Una matriz con los \enc{í}{i}tems como filas y los \enc{í}{i}ndices como
#' columnas.
#' @details El \enc{í}{i}ndice de dificultad p correponde a la proporci\enc{ó}{o}n de estudiantes
#' que responde correctamente el \enc{í}{i}tem. Puede tomar valores entre 0 y 1.
#' A mayor valor, el \enc{í}{i}tem es más f\enc{á}{a}cil.
#'
#' Los \enc{í}{i}ndices de discriminaci\enc{ó}{o}n permiten determinar si el \enc{í}{i}tem diferencia
#' entre estudiantes con alta o baja habilidad. Se calculan a partir del
#' grupo de estudiantes con mejor y peor puntuaci\enc{ó}{o}n en el test.
#'
#' El \enc{í}{i}ndice de discriminaci\enc{ó}{o}n 1 (Dc1) corresponde a la diferencia entre
#' la proporci\enc{ó}{o}n de aciertos del grupo superior y la proporci\enc{ó}{o}n de aciertos
#' del grupo inferior. Los valores extremos que puede alcanzar este \enc{í}{i}ndice
#' son 0 y +/-1. Los \enc{í}{i}tems con discriminaci\enc{ó}{o}n negativa favorecen a los
#' estudiantes con baja puntuaci\enc{ó}{o}n en el test y en principio deben ser revisados.
#' Este \enc{í}{i}ndice se ve influenciado por el \enc{í}{i}ndice de dificultad, por lo que a
#' veces es conveniente compararlo con el \enc{í}{i}ndice de discriminaci\enc{ó}{o}n 2 (Dc2).
#'
#' El \enc{í}{i}ndice de discriminaci\enc{ó}{o}n 2 (Dc2) corresponde a la proporci\enc{ó}{o}n de
#' aciertos del grupo superior en relaci\enc{ó}{o}n al total de aciertos de ambos grupos.
#' Los valores de este \enc{í}{i}ndice van de 0 a 1. Pueden considerarse satisfactorios
#' valores mayores a 0.5. Este \enc{í}{i}ndice es independiente del nivel de dificultad
#' de la pregunta.
#' @references Morales, P. (2009). An\enc{á}{a}lisis de \enc{í}{i}tem en las pruebas objetivas.
#' Madrid. Recuperado de \url{https://educrea.cl/wp-content/uploads/2014/11/19-nov-analisis-de-items-en-las-pruebas-objetivas.pdf}
#' @seealso \code{\link{corregirItems}}, \code{\link{datos}} y \code{\link{clave}}.
#' @export
#' @examples
#' data(datos)
#' data(clave)
#' respuestas <- subset(datos, select = -id )
#' analizarItems(corregirItems(respuestas, clave))
#'
analizarItems <- function(respCorregidas, proporcion=0.27, digitos=2){
  # El indice de dificultad p es la proporcion de estudiantes que respondio el item
  # correctamente
  p <- round(apply(respCorregidas, 2, mean), digitos)
  # Para calcular el indice de discriminacion es necesario separar a los estudiantes
  # en dos grupos: grupo superior y grupo inferior segun el puntaje obtenido en
  # la prueba. El tamano de cada grupo lo define el usuario. Valores comunes son 25%,
  # 27% y 33% del total de estudiantes.
  if (proporcion>=0.5){
    warning("La proporci\u00F3n no puede ser mayor o igual a 0.5. Usando valor por defecto (0.27).")
    proporcion <- 0.27
  }
  nGrupos <- round(nrow(respCorregidas)*proporcion)
  puntajes <- apply(respCorregidas, 1, sum)
  datos <- cbind(respCorregidas, pje=puntajes)
  datos <- datos[order(datos[,"pje"]),]
  grupoInferior <- datos[1:nGrupos, -ncol(datos)]
  grupoSuperior <- datos[nrow(datos):(nrow(datos)-nGrupos+1), -ncol(datos)]
  # El indice de discriminacion DC1 es la diferencia entre la proporcion de estudiantes
  # del grupo superior y del grupo inferior que acierta el item.
  Dc1 <- round((apply(grupoSuperior, 2, mean) - apply(grupoInferior, 2, mean)), digitos)
  # El indice de discrimiacion DC2 es la proporcion de aciertos del grupo superior
  # sobre el total de aciertos de ambos grupos.
  Dc2 <- round(apply(grupoSuperior, 2, sum) / (apply(grupoSuperior, 2, sum) + apply(grupoInferior, 2, sum)), digitos)
  # La funcion devuelve una matrix con los valores de p, DC1 y DC2.
  return(cbind(p, Dc1, Dc2))
}

#' @title Frecuencia de distractores.
#' @description Calcula la frecuencia o proporci\enc{ó}{o}n de cada alternativa seleccionada
#' en cada \enc{í}{i}tem.
#' @param respuestas Una hoja de datos con las alternativas que seleccion\enc{ó}{o} cada
#' estudiante en cada \enc{í}{i}tem de la prueba.
#' @param clave Una hoja de datos con la alternativa correcta para cada \enc{í}{i}tem.
#' @param frecuencia Un valor l\enc{ó}{o}gico que determina si la informacin\enc{ó}{o}
#' se presenta como frecuencia (TRUE) o proporci\enc{ó}{o}n (FALSE).
#' @param digitos La cantidad de d\enc{í}{i}gitos significativos que tendr\enc{á}{a} el resultado.
#' @return Una matriz con los \enc{í}{i}tems como filas y las frecuencias de las
#' alternativas como columnas. Si se entrega como argumento la clave de la prueba, se entrega una
#' hoja de datos con la frecuencia o proporci\enc{ó}{o}n de las alternativas y la alternativa
#' correcta para cada \enc{í}{i}tem.
#' @seealso \code{\link{analizarDistractores}}.
#' @export
#' @examples
#' data(datos)
#' respuestas <- subset(datos, select = -id)
#' frecuenciaDistractores(respuestas)
#'
frecuenciaDistractores <- function(respuestas, clave = NULL, frecuencia=TRUE, digitos=2){
  # output es una tabla de contingencia que relaciona cada item con sus posibles respuestas
  # incluyendo las respuestas omitidas.
  output <- t(sapply(respuestas, table, useNA="always"))
  colnames(output)[ncol(output)] <- "O"
  # Si la variable frecuencia es FALSE, se calcula la proporcion.
  if (!frecuencia){
    # total es el numero de personas que respondieron cada item.
    total <- apply(output, 1, sum)
    # Devuelve los datos en forma de proporcion.
    output <- round(output/total, digitos)
  }
  if (!is.null(clave)){
    # Verifica que la clave tenga una estructura apropiada.
    clave <- validarClave(respuestas, clave)
    # Realiza las trasnformaciones necesarias para unir la frecuencia de distractores con la
    # clave.
    clave <- as.data.frame(t(clave), stringsAsFactors=FALSE)
    names(clave) <- "Key"
    output <- as.data.frame(output)
    output <- cbind(output, clave)
  }
  return(output)
}

#' @title An\enc{á}{a}lisis de distractores.
#' @description Calcula la frecuencia o proporci\enc{ó}{o}n de las alternativas
#' seleccionadas por el grupo superior e inferior en cada \enc{í}{i}tem.
#' @param respuestas Una matriz con las alternativas seleccionadas por los estudiantes
#' en cada \enc{í}{i}tem.
#' @param clave  Una matriz con las alternativas correctas para cada \enc{í}{i}tem.
#' @param nOpciones La cantidad de alternativas posibles para cada \enc{í}{i}tem.
#' @param proporcion Proporción del total de estudiantes que contituyen los grupos superior e inferior.
#' @param frecuencia Un valor l\enc{ó}{o}gico para determinar si la
#' informaci\enc{ó}{o}n se presenta como frecuencia (TRUE) o proporci\enc{ó}{o}n (FALSE).
#' @param digitos La cantidad de d\enc{í}{i}gitos significativos que tendr\enc{á}{a} el resultado.
#' @return Una lista en la cual cada elemento corresponde a un \enc{í}{i}tem.
#' Para cada \enc{í}{i}tem se calcula la frecuencia o proporci\enc{ó}{o}n de las
#' alternativas seleccionadas por el grupo superior y por el grupo inferior de estudiantes.
#' @references Morales, P. (2009). An\enc{á}{a}lisis de \enc{í}{i}tem en las pruebas objetivas.
#' Madrid. Recuperado de \url{https://educrea.cl/wp-content/uploads/2014/11/19-nov-analisis-de-items-en-las-pruebas-objetivas.pdf}
#' @seealso \code{\link{frecuenciaDistractores}} y \code{\link{analizarItems}}.
#' @export
#' @examples
#' data(datos)
#' data(clave)
#' respuestas <- subset(datos, select = -id )
#' analizarDistractores(respuestas, clave)
#'
analizarDistractores <- function(respuestas, clave, nOpciones=4, proporcion=0.27, frecuencia=TRUE, digitos=2){
  nItems <- ncol(respuestas)
  opciones <- LETTERS[1:nOpciones]
  # Los valores recomendados de proporcion son 0.25, 0.27 o 0.33. Valores sobre 0.5 provoca
  # que los grupos se solapen. La idea de este analisis es compara los grupos extremos, descartando
  # descartando la influencia de los valores medios.
  if (proporcion>=0.5){
    warning("La proporci\u00F3n no puede ser mayor o igual a 0.5. Usando valor por defecto (0.27).")
    proporcion <- 0.27
  }
  # Calcula el tamano de los grupos superior e inferior.
  nGrupos <- round(nrow(respuestas)*proporcion)
  respCorregidas <- corregirItems(respuestas, clave)
  puntajes <- apply(respCorregidas, 1, sum)
  # verifica que la clave tenga la estructura apropiada.
  clave <- validarClave(respuestas, clave)
  # Coloca un asterisco en la alternativa correcta.
  for(i in 1:nItems){
    levels(respuestas[,i]) <- ifelse(opciones == clave[1,i],
                                     paste("*", opciones, sep=""),
                                     paste(" ", opciones, sep=""))
  }
  # Ordena a los estudiantes por puntaje y crea los grupos superior e inferior.
  datos <- cbind(respuestas, pje=puntajes)
  datos <- datos[order(datos[,"pje"]),]
  gInf <- datos[1:nGrupos, -ncol(datos)]
  gSup <- datos[nrow(datos):(nrow(datos)-nGrupos+1), -ncol(datos)]
  # output es la lista que contendra la informacion devuelta por la funcion.
  output <- list()
  # Calcula las respuestas selecionadas por los grupos superior e inferior.
  for (i in 1:nItems){
    output[[i]] <- rbind(gSup=table(gSup[,i]), gInf=table(gInf[,i]))
    if (!frecuencia){
      output[[i]] <- round(output[[i]]/nGrupos, digitos)
    }
  }
  # Nombra los elementos de la lista output con los nombres de los items.
  names(output) <- colnames(respuestas)
  # Devuelve una lista con la frecuencia de seleccion de cada grupo para cada item.
  return(output)
}

#' @title C\enc{á}{a}lculo de puntajes.
#' @description Calcula el puntaje total obtenido en la prueba por cada estudiante.
#' @param respCorregidas Una matriz con las respuestas de los estudiantes a cada \enc{í}{i}tem
#' en forma binaria.
#' @return Un vector con los puntajes obtenidos por los estudiantes.
#' @seealso \code{\link{calcularNotas}} y \code{\link{corregirItems}}.
#' @export
#' @examples
#' data(datos)
#' data(clave)
#' respuestas <- subset(datos, select = -id )
#' puntajes <- calcularPuntajes(corregirItems(respuestas, clave))
#' cbind(respuestas, pje=puntajes)
#'
calcularPuntajes <- function(respCorregidas){
  apply(respCorregidas, 1, sum)
}

#' @title C\enc{á}{a}lculo de notas.
#' @description Calcula la nota obtenida por cada estudiante en funci\enc{ó}{o}n de
#' su puntaje alcanzado en la prueba seg\enc{ú}{u}n el sistema de calificaci\enc{ó}{o}n
#' utilizado en Chile.
#' @param puntajes Un vector con los puntajes obtenidos por los estudiantes
#' en la prueba.
#' @param pjeMax El puntaje m\enc{á}{a}ximo posible de alcanzar en la prueba.
#' @param notaMin La nota m\enc{í}{i}nima otorgada al estudiante sin puntaje.
#' @param notaMax La nota m\enc{á}{a}xima otorgada al estudiante con mejor puntaje.
#' @param notaAprob La nota necesaria para aprobar la prueba.
#' @param prema Porcentaje de rendimiento m\enc{í}{i}nimo aceptable. Corresponde
#' a la proporci\enc{ó}{o}n del puntaje m\enc{á}{a}ximo necesario para obtener
#' la nota de aprobaci\enc{ó}{o} en la prueba.
#' @return Un vector con las notas obtenidas por los estudiantes en la prueba.
#' @seealso \code{\link{calcularPuntajes}} y \code{\link{corregirItems}}.
#' @export
#' @examples
#' data(datos)
#' data(clave)
#' respuestas <- subset(datos, select = -id)
#' puntajes <- calcularPuntajes(corregirItems(respuestas, clave))
#' notas <- calcularNotas(puntajes)
#' cbind(datos$id, pje=puntajes, nota=notas)
#'
calcularNotas <- function(puntajes, pjeMax=max(puntajes), notaMin=1, notaMax=7, notaAprob=4, prema=0.6){
  output <- c()
  reprobados <- puntajes[puntajes<prema*pjeMax] * (notaAprob-notaMin)/(prema*pjeMax) + notaMin
  aprobados <-  (puntajes[puntajes>=prema*pjeMax]-prema*pjeMax) * (notaMax-notaAprob)/(pjeMax-prema*pjeMax) + notaAprob
  output[puntajes<prema*pjeMax] <- reprobados
  output[puntajes>=prema*pjeMax] <- aprobados
  return(round(output,1))
}

#' @title Correlaci\enc{ó}{o}n biserial puntual.
#' @description  Calcula la correlaci\enc{ó}{o}n biserial puntual para cada alternativa de cada \enc{í}{i}tem con respecto
#' al puntaje obtenido en la prueba.
#' @param respuestas Una matriz con las alternativas seleccionadas por los estudiantes
#' a cada \enc{í}{i}tem de la prueba.
#' @param clave Una hoja de datos con la alternativa correcta para cada \enc{í}{i}tem.
#' @param correccionPje Un valor l\enc{ó}{o}gico para usar o no la correcci\enc{ó}{o}n de puntaje.
#' La correcci\enc{ó}{o}n de puntaje consiste en restar del puntaje total obtenido en la prueba
#' el punto obtenido por el \enc{í}{i}tem analizado.
#' @param nOpciones La cantidad de alternativas posibles para cada \enc{í}{i}tem.
#' @param digitos La cantidad de d\enc{í}{i}gitos significativos que tendr\enc{á}{a} el resultado.
#' @return Una hoja de datos con la correlaci\enc{ó}{o}n biserial puntual de las alternativas
#' y la alternativa correcta para cada \enc{í}{i}tems.
#' @seealso \code{\link{analizarDistractores}}, \code{\link{frecuenciaDistractores}}
#' y \code{\link{analizarItems}}.
#' @export
#' @importFrom stats cor
#' @examples
#' data(datos)
#' data(clave)
#' respuestas <- subset(datos, select = -id)
#' pBis(respuestas, clave)
#'
pBis <- function(respuestas, clave, correccionPje=TRUE, nOpciones=4, digitos=2){
  # verifica que la clave tenga la estructura apropiada.
  clave <- validarClave(respuestas, clave)
  # matriz con las respuestas en forma binaria (correcto/incorrecto).
  respCorregidas <- corregirItems(respuestas, clave)
  # opciones son las alternativas posibles como respuestas.
  opciones <- LETTERS[1:nOpciones]
  # output es la matriz que tendra las correlaciones de cada alternativa para cada item.
  nItems <- ncol(respuestas)
  output <- matrix(NA, nItems, nOpciones)
  rownames(output) <- colnames(respuestas)
  colnames(output) <- opciones
  for (i in 1:nItems){
    for (a in 1:nOpciones){
      # tmp almacena la seleccion de la alternativa en cuestion por cada estudiantes
      # con un 1 y la seleccion de otra de las opciones posibles con un 0.
      tmp <- ifelse(respuestas[,i]==opciones[a], 1, 0)
      # Los valores NA se consideran como 0.
      tmp[is.na(tmp)] <- 0
      if (correccionPje){
        # Calcula la correlacion biserial puntual con correccion de puntaje.
        output[i,a]=cor(tmp,rowMeans(respCorregidas[,-i],na.rm=TRUE)*(ncol(respCorregidas)-1),use="pairwise.complete.obs")

      } else {
        # Calcula la correlacion biserial puntual sin correccion de puntaje.
        output[i,a]=cor(tmp,respCorregidas,use="pairwise.complete.obs")
      }
    }
  }
  # Realiza los ajustes necesarios para combinar las correlaciones biseriales puntuales con la
  # clave.
  output <- round(output, digitos)
  clave <- as.data.frame(t(clave), stringsAsFactors=FALSE)
  names(clave) <- "Key"
  output <- as.data.frame(output)
  output <- cbind(output, clave)
  # Devuelve un data frame con las coeficientes de cada alternativa y la clave para cada item.
  return(output)
}

#' @title An\enc{á}{a}lisis gr\enc{á}{a}fico de \enc{í}{i}tems.
#' @description El an\enc{á}{a}lisis gr\enc{á}{a}fico de \enc{í}{i}tems permite visualizar
#' las alternativas que eligen los estudiantes seg\enc{ú}{u}n su desempe\enc{ñ}{n}o
#' general en la prueba. A partir de una matriz de respuestas y de un vector de puntajes
#' se puede agrupar a los estudiantes seg\enc{ú}{u}n su puntaje y calcular la
#' proporci\enc{ó}{o}n en que cada grupo seleccion\enc{ó}{o} cada alternativa.
#' Esta funci\enc{ó}{o}n devuelve un gr\enc{á}{a}fico por cada \enc{í}{i}tem que relaciona
#' la frecuencia de selecci\enc{ó}{o}n de cada alternatica con los grupos de puntajes.
#' Requiere los paquetes \pkg{\link{ggplot2}} y \pkg{\link{reshape}}.
#' @param respuestas Una matriz con las alternativas seleccionada por los estudiantes
#' a cada \enc{í}{i}tem de la prueba.
#' @param clave Una matriz con la alternativa correcta para cada \enc{í}{i}tem.
#' @param nGrupos N\enc{ú}{u}mero de grupos en los que se categorizar\enc{á}{a}n los puntajes.
#' @param nOpciones N\enc{ú}{u}mero de alternativas posibles para cada \enc{í}{i}tem.
#' @return Una matriz con la correlaci\enc{ó}{o}n biserial puntual para cada \enc{í}{i}tem y para cada
#' alternativa.
#' @references Ensenada, B. (2010). Manual para el an\enc{á}{a}lisis gr\enc{á}{a}fico
#' de \enc{í}{i}tems. Baja California. Recuperado de \url{http://www.educacionbc.edu.mx/departamentos/evaluacion/eacademicos/archivos/jornadasBC/MANUAL_PAGI.pdf}
#' @seealso \code{\link{corregirItems}}, \code{\link{analizarDistractores}},
#' \code{\link{frecuenciaDistractores}}, \code{\link{analizarItems}}, \code{\link{pBis}} y
#' \code{\link{calcularPuntajes}}
#' @export
#' @importFrom reshape melt
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point ggtitle theme element_text
#' scale_x_continuous scale_y_continuous labs
#' @examples
#' data(datos)
#' data(clave)
#' respuestas <- subset(datos, select=-id)
#' plots <- agi(respuestas, clave, nGrupos=ncol(respuestas)/2)
#' plots[[1]][[1]]
#' plots[[2]][[1]]
#'
agi <- function(respuestas, clave, nGrupos=4, nOpciones=4){
  # verifica que la clave tenga la estructura apropiada.
  clave <- validarClave(respuestas, clave)
  #
  opciones<-LETTERS[1:nOpciones]
  respCorregidas <- corregirItems(respuestas, clave)
  pBiserial <- pBis(respuestas, clave, nOpciones = nOpciones)[,-(nOpciones+1)]
  puntajes <- calcularPuntajes(corregirItems(respuestas, clave))
  # scoreGroups contiene el rango de puntaje al que pertenece cada estudiante.
  scoreGroups <- cut(puntajes,breaks=nGrupos)
  # sgLevels son los rangos de puntajes.
  sgLevels <- levels(scoreGroups)
  # Los limites superior e inferior de cada rango.
  lowerLimits <- as.numeric( sub("\\((.+),.*", "\\1", sgLevels) )
  upperLimits <- as.numeric( sub("[^,]*,([^]]*)\\]","\\1",sgLevels))
  # El valor promedio de los limites de cada rango se usa para identificarlo porteriormente.
  sgMeans <- rowMeans(cbind(lowerLimits, upperLimits))
  # Los limites de cada rango que se usaran para rotular el eje x del grafico.
  limites <- unique( c(lowerLimits, upperLimits) )
  # sgIndexes contiene los indices de posicion en el vector scoreGroups de los estudiantes de
  # cada grupo.
  sgIndexes <- vector("list",nGrupos)
  for(j in 1:nGrupos){
    sgIndexes[[j]]=which(scoreGroups==sgLevels[j])
  }
  # tmp almacena las proporciones con que cada grupo selecciono cada alternativa.
  tmp <- matrix(nrow=nOpciones,ncol=nGrupos)
  colnames(tmp) <- sgLevels
  rownames(tmp) <- opciones
  plots <- vector("list",ncol(respuestas))
  datos <- vector("list",ncol(respuestas))
  nItems<-ncol(respuestas)
  for (i in 1:nItems){
    for(g in 1:nGrupos){
      for(o in 1:nOpciones){
        tmp[o,g]=length(which(respuestas[sgIndexes[[g]],i]==opciones[o]))/length(sgIndexes[[g]])
      }
    }
    proportions <- as.data.frame(t(tmp))
    names(proportions) <- ifelse(opciones == clave[,i],
                          paste(c("*"), names(proportions), c(" ("), pBiserial[i,], c(")"), sep = ""),
                          paste(c(" "), names(proportions), c(" ("), pBiserial[i,], c(")"), sep = ""))
    df <- cbind(proportions,sgMeans)
    datos[[i]] <- df
    df <- melt(data=df, id.vars="sgMeans")
    plots[[i]] <- ggplot(df, aes_string(x="sgMeans", y="value", colour="variable")) +
                  geom_line() +
                  geom_point() +
                  labs(title=paste("\u00CDtem ", i), x="Grupo de puntaje",
                       y="Proporci\u00F3n de estudiantes", colour="Alternativa (pBis)") +
                  theme(legend.position=c(0.12, 0.75),
                        legend.text=element_text(size=11, face="bold", hjust=0.5)) +
                  scale_x_continuous(limits = c(min(limites),max(limites)),
                                     breaks=round(limites,1)) +
                  scale_y_continuous(limits = c(0,1))
  }
  return(list(plots, datos))
}
