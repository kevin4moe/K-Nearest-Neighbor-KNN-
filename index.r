
library(caret)

#' Usando las coordenadas de 2 cuatro puntos se calcula la distancia entre ambos puntos
#' 
#' @param x1 Valor en el eje x del punto 1
#' @param x2 Valor en el eje x del punto 2
#' @param y1 Valor en el eje y del punto 1
#' @param y2 Valor en el eje y del punto 2
#' @return La distancia entre el punto 1 y el punto 2
distancia <- function(x1, x2, y1, y2) {
  resultado = sqrt((x1 - x2)^2 + (y1 - y2)^2)
  return(resultado)
}

#' Se crea una lista de {q} numeros aleatorios entre 1 y 150
#' 
#' @param q Numero de valores aleatorios
#' @return Un vector de valores aleatorios de tamaño {q}
random_num_list <- function(q) {
  lista = sort(as.integer(runif(q, min = 1, max = 149)))
  for (i in 2:q) {
    prev = i - 1
    if (lista[prev] == lista[i]) {
      lista[i] = lista[i] + 1
    } else if (lista[prev] > lista[i]) {
      lista[i] = lista[prev] + 1
    }
  }
  return(lista)
}

k_n_cerca <- function(data) {
  species = data$Species[1:5]
  muestra = as.data.frame(table(species))
  muestra = muestra[order(muestra$Freq, decreasing = TRUE), ]
  return(as.vector(muestra$species)[1])
}

# Crea las carpetas para el analisis con sepal y petal
dir.create(file.path("./", "tests"), showWarnings = FALSE)

# Lista de presición
precisionTable.sepal = data.frame(k_n = c(1), precision = c(1))
precisionTable.petal = data.frame(k_n = c(1), precision = c(1))
for (prueba in 1:20) {

  # Lista de candidatos a evaluar
  candidatos = random_num_list(20)
  # Data frame con los datos correspondientes a cada candidato
  conejos = iris[candidatos, ]
  # Data frame con todos los datos excepto los candidatos seleccionados
  flores = iris[-candidatos, ]
  conejosTest.sepal = data.frame(original = conejos$Species, nuevo = "")
  conejosTest.petal = data.frame(original = conejos$Species, nuevo = "")
  
  # Crea las carpetas para el analisis con sepal y petal
  dir.create(file.path("./tests/", paste("k_", prueba, sep = "")), showWarnings = FALSE)

  # Ciclo por cada cada candidato (20)
  for (test in 1:length(candidatos)) {
    # Calculo de la distancia entre la muestra y todo el conjunto
    distancia_sepal = distancia(
      conejos$Sepal.Length[test],
      flores$Sepal.Length,
      conejos$Sepal.Width[test],
      flores$Sepal.Width
    )
    distancia_petal = distancia(
      conejos$Petal.Length[test],
      flores$Petal.Length,
      conejos$Petal.Width[test],
      flores$Petal.Width
    )

    # Creación de un data frame con las distancias y sus respectivas especies
    d_sepal = data.frame(
      distancia_sepal,
      Species = flores$Species
    )
    d_petal = data.frame(
      distancia_petal,
      Species = flores$Species
    )

    # Ordenar de menor a mayor los datos segun su distancia
    d_sepal = d_sepal[order(d_sepal$distancia_sepal),]
    d_petal = d_petal[order(d_petal$distancia_petal),]
    # Crea las carpetas para el analisis con sepal y petal
    dir.create(file.path(paste("./tests/k_", prueba, sep = ""), "sepal"), showWarnings = FALSE)
    dir.create(file.path(paste("./tests/k_", prueba, sep = ""), "petal"), showWarnings = FALSE)
    # Escribe los un archivo csv con los 10 primeros elementos de los candidatos segun su distancia sepal y petal
    write.csv(d_sepal[1:10, ], paste("./tests/k_", prueba, "/sepal/candidato_", test, ".csv", sep = ""))
    write.csv(d_petal[1:10, ], paste("./tests/k_", prueba, "/petal/candidato_", test, ".csv", sep = ""))

    # Ciclo para hacer un analisis con los primeras 5 muestras más cercanas segun sepal
    for (q in seq(from = 0.2, to = 1, by = 0.05)) {
      k_n_eval = d_sepal[d_sepal$distancia_sepal < q, ]
      if (length(k_n_eval[ ,1]) >= 5) {
        conejosTest.sepal$nuevo[test] = k_n_cerca(k_n_eval)
        break
      }
    }
    # Ciclo para hacer un analisis con los primeras 5 muestras más cercanas segun petal
    for (q in seq(from = 0.2, to = 1, by = 0.05)) {
      k_n_eval = d_petal[d_petal$distancia_petal < q, ]
      if (length(k_n_eval[ ,1]) >= 5) {
        conejosTest.petal$nuevo[test] = k_n_cerca(k_n_eval)
        break
      }
    }
  }

  # Escribe un resumen de lo analisado con los datos de los candidatos segun sepal y petal
  write.csv(conejosTest.sepal, paste("./tests/k_", prueba, "/resumen_sepal", ".csv", sep = ""))
  write.csv(conejosTest.petal, paste("./tests/k_", prueba, "/resumen_petal", ".csv", sep = ""))

  # Los resumenes son transformados en factores para su correcto analisis para la función {confusionMatrix}
  conejosTest.sepal$original = factor(conejosTest.sepal$original, levels = c('setosa', 'versicolor', 'virginica'))
  conejosTest.sepal$nuevo = factor(conejosTest.sepal$nuevo, levels = c('setosa', 'versicolor', 'virginica'))
  conejosTest.petal$original = factor(conejosTest.petal$original, levels = c('setosa', 'versicolor', 'virginica'))
  conejosTest.petal$nuevo = factor(conejosTest.petal$nuevo, levels = c('setosa', 'versicolor', 'virginica'))
  # Asignación de la matriz de confusion a una variable para su posterior escritura
  matriz.sepal = confusionMatrix(conejosTest.sepal$nuevo, conejosTest.sepal$original)
  matriz.petal = confusionMatrix(conejosTest.petal$nuevo, conejosTest.petal$original)
  write.csv(matriz.sepal$table, paste("./tests/k_", prueba, "/confusion_matrix_sepal", ".csv", sep = ""))
  write.csv(matriz.petal$table, paste("./tests/k_", prueba, "/confusion_matrix_petal", ".csv", sep = ""))
  precisionTable.sepal[prueba, ] = c(prueba, matriz.sepal$overall[1])
  precisionTable.petal[prueba, ] = c(prueba, matriz.petal$overall[1])

  point_size = 1
  petal_plot = paste("./tests/k_", prueba, "/Petal_plot", ".png", sep = "")
  png(file=petal_plot, width = 2000, height = 2000, units = "px", pointsize = 30)
  plot(
    flores$Petal.Length,
    flores$Petal.Width,
    main="Petal",
    xlab="Petal Length",
    ylab="Petal Width",
    xlim=c(0, 7),
    ylim=c(0, 2.5)
  )
  points(
    conejos$Petal.Length,
    conejos$Petal.Width, 
    type = "p",
    pch = 19,
    col = "red",
  )
  points(
    flores$Petal.Length[flores$Species == "setosa"],
    flores$Petal.Width[flores$Species == "setosa"], 
    type = "p",
    pch = 11,
    col = "#e89eff",
    cex = point_size,
  )
  points(
    flores$Petal.Length[flores$Species == "virginica"],
    flores$Petal.Width[flores$Species == "virginica"], 
    type = "p",
    pch = 11,
    col = "#ffd900",
    cex = point_size,
  )
  points(
    flores$Petal.Length[flores$Species == "versicolor"],
    flores$Petal.Width[flores$Species == "versicolor"], 
    type = "p",
    pch = 11,
    col = "#bbff00",
    cex = point_size,
  )
  dev.off()

  sepal_plot = paste("./tests/k_", prueba, "/Sepal_plot", ".png", sep = "")
  png(file=sepal_plot, width = 2000, height = 2000, units = "px", pointsize = 30)
  plot(
    flores$Sepal.Length,
    flores$Sepal.Width,
    main="Sepal",
    xlab="Sepal Length",
    ylab="Sepal Width",
    xlim=c(4, 8),
    ylim=c(2, 4.5)
  )
  points(
    conejos$Sepal.Length,
    conejos$Sepal.Width, 
    type = "p",
    pch = 19,
    col = "red",
  )
  points(
    flores$Sepal.Length[flores$Species == "setosa"],
    flores$Sepal.Width[flores$Species == "setosa"], 
    type = "p",
    pch = 11,
    col = "#e89eff",
    cex = point_size,
  )
  points(
    flores$Sepal.Length[flores$Species == "virginica"],
    flores$Sepal.Width[flores$Species == "virginica"], 
    type = "p",
    pch = 11,
    col = "#ffd900",
    cex = point_size,
  )
  points(
    flores$Sepal.Length[flores$Species == "versicolor"],
    flores$Sepal.Width[flores$Species == "versicolor"], 
    type = "p",
    pch = 11,
    col = "#bbff00",
    cex = point_size,
  )
  dev.off()
}


write.csv(precisionTable.sepal, "./tests/accuracy_sepal.csv")
write.csv(precisionTable.petal, "./tests/accuracy_petal.csv")

png(file="sepal_accuracy.png", width = 2000, height = 2000, units = "px", pointsize = 30)

plot(
  precisionTable.sepal$k_n,
  precisionTable.sepal$precision,
  main="Accuracy",
  xlab="K_n",
  ylab="Accuracy",
  xlim=c(1, 21),
  ylim=c(0.5, 1),
  type = "n",
  xaxt = "n",
  yaxt = "n"
)
puntos.sepal = precisionTable.sepal[order(precisionTable.sepal$precision),]
tabla_puntos = as.data.frame(table(puntos.sepal$precision))
axis(1, at = c(1, 5, 10, 15, 20))
axis(2, at = format(round(as.numeric(as.vector(tabla_puntos$Var1)), 2), nsmall = 2), las = 1,)
paleta = hcl.colors(
  length(table(puntos.sepal$precision)),
  palette = "Temps"
)
paleta_per = NULL
for (punto in 1:length(table(puntos.sepal$precision))) {
  times = tabla_puntos$Freq[punto]
  paleta_per = append(paleta_per, rep(paleta[punto], times))
}
points(
  puntos.sepal$k_n,
  puntos.sepal$precision,
  type = "p",
  pch = 15,
  col= paleta_per,
  cex = 2,
)
text(
  puntos.sepal$k_n,
  puntos.sepal$precision,
  labels = puntos.sepal$k_n,
  cex= 0.9,
  pos= 3
)

dev.off()


png(file="petal_accuracy.png", width = 2000, height = 2000, units = "px", pointsize = 30)

plot(
  precisionTable.petal$k_n,
  precisionTable.petal$precision,
  main="Accuracy",
  xlab="K_n",
  ylab="Accuracy",
  xlim=c(1, 21),
  ylim=c(0.5, 1),
  type = "n",
  xaxt = "n",
  yaxt = "n"
)
puntos.petal = precisionTable.petal[order(precisionTable.petal$precision),]
tabla_puntos = as.data.frame(table(puntos.petal$precision))
axis(1, at = c(1, 5, 10, 15, 20))
axis(2, at = format(round(as.numeric(as.vector(tabla_puntos$Var1)), 2), nsmall = 2), las = 1,)
paleta = hcl.colors(
  length(table(puntos.petal$precision)),
  palette = "Temps"
)
paleta_per = NULL
for (punto in 1:length(table(puntos.petal$precision))) {
  times = tabla_puntos$Freq[punto]
  paleta_per = append(paleta_per, rep(paleta[punto], times))
}
points(
  puntos.petal$k_n,
  puntos.petal$precision,
  type = "p",
  pch = 15,
  col= paleta_per,
  cex = 2,
)
text(
  puntos.petal$k_n,
  puntos.petal$precision,
  labels = puntos.petal$k_n,
  cex= 0.9,
  pos= 3
)

dev.off()
