library(caret)

#Para calcular la distancia entre dos puntos, requiero de sus coordenadas de tal manera que: 
#x1 Valor en el eje x del punto 1
#x2 Valor en el eje x del punto 2
#y1 Valor en el eje y del punto 1
#y2 Valor en el eje y del punto 2

distancia <- function(x1, x2, y1, y2) {       #Genero una funci�n que me arroje las distancias, seg�n las coordenadas que le asigne.
  resultado = sqrt((x1 - x2)^2 + (y1 - y2)^2)
  return(resultado)
}

#Creo una lista de "q" n�meros aleatorios entre 1 y 150 (registros de mi data frame)
#Para esto gener� una funci�n que me regresar� un vector de valores aleatorios de tama�o "q"
random_num_list <- function(q) {
  lista = sort(as.integer(runif(q, min = 1, max = 149))) #runif n�meros aleatorios con distribuci�n normal. 
  for (i in 2:q) {
    prev = i - 1 
    if (lista[prev] == lista[i]) {             #condiciono mi lista de n�meros aleatorios de tal forma que no se repitan, orden�ndolos de menor a mayor desde el mero principio y restringiendo. 
      lista[i] = lista[i] + 1
    } else if (lista[prev] > lista[i]) {
      lista[i] = lista[prev] + 1
    }
  }
  return(lista)
}

#Funci�n Principal, o algortimo KNN. 
k_n_cerca <- function(data, k) {  #data es un data frame que contiene las especies de las evaluaciones propuestas m�s adelante.
  species = data$Species[1:k]
  muestra = as.data.frame(table(species)) #transformar la tabla a un data frame para manipular de mejor manera.
  muestra = muestra[order(muestra$Freq, decreasing = TRUE), ]
  return(as.vector(muestra$species)[1]) #El c�digo ordena las frecuencias de mayor a menor y an�lisa a qu� especie corresponden la mayor�a.  
}

#Creo las carpetas para el analisis con sepal y petal
dir.create(file.path("./", "tests"), showWarnings = FALSE) #dir. hace carpetas y las aloja desde donde trabaja R. 

#Lista de presici�n
precisionTable.sepal = data.frame(k_n = c(1), precision = c(1)) #Data frame inicializado con uno, para llenar con datos.
precisionTable.petal = data.frame(k_n = c(1), precision = c(1))
for (prueba in 1:20) {
  
  #Lista de candidatos a evaluar
  candidatos = random_num_list(20) #Utilizo mi funci�n de datos aleatorios
  
  #Data frame con los datos correspondientes a cada candidato
  Datos_Candidatos = iris[candidatos, ]
  
  #Data frame con todos los datos excepto los candidatos seleccionados
  flores = iris[-candidatos, ]
  
  Candidatos_Test.sepal = data.frame(original = Datos_Candidatos$Species, nuevo = "")
  Candidatos_Test.petal = data.frame(original = Datos_Candidatos$Species, nuevo = "")
  
  #Creo las carpetas para el analisis con sepal y petal, para cada prueba de K. 
  dir.create(file.path("./tests/", paste("k_", prueba, sep = "")), showWarnings = FALSE)
  
  #Ciclo por cada candidato (20)
  for (test in 1:length(candidatos)) {
    #Calculo de la distancia Sepal entre la muestra y todo el conjunto. Utilizo mi funci�n de distancia.con el largo como x y lo ancho como y.
    distancia_sepal = distancia(
      Datos_Candidatos$Sepal.Length[test],
      flores$Sepal.Length,
      Datos_Candidatos$Sepal.Width[test],
      flores$Sepal.Width
    )
    #Calculo de la distancia Petal entre la muestra y todo el conjunto. Utilizo mi funci�n de distancia.con el largo como x y lo ancho como y.
    distancia_petal = distancia(
      Datos_Candidatos$Petal.Length[test],
      flores$Petal.Length,
      Datos_Candidatos$Petal.Width[test],
      flores$Petal.Width
    )
    
    # Creo de un data frame con las distancias y sus respectivas especies.
    d_sepal = data.frame(
      distancia_sepal,
      Species = flores$Species
    )
    d_petal = data.frame(
      distancia_petal,
      Species = flores$Species #Asignado a una lista, quiero solamente el vector de las especies. 
    )
    
    #Ordeno de menor a mayor los datos seg�n su distancia.
    d_sepal = d_sepal[order(d_sepal$distancia_sepal),]
    d_petal = d_petal[order(d_petal$distancia_petal),]
    
    #Creo las carpetas para el an�lisis con sepal y petal.
    dir.create(file.path(paste("./tests/k_", prueba, sep = ""), "sepal"), showWarnings = FALSE)
    dir.create(file.path(paste("./tests/k_", prueba, sep = ""), "petal"), showWarnings = FALSE)
    
    #Escribo un archivo csv con los k(prueba) primeros elementos de los candidatos seg�n su distancia sepal y petal.
    write.csv(d_sepal[1:prueba, ], paste("./tests/k_", prueba, "/sepal/candidato_", test, ".csv", sep = ""))
    write.csv(d_petal[1:prueba, ], paste("./tests/k_", prueba, "/petal/candidato_", test, ".csv", sep = ""))

    Candidatos_Test.sepal$nuevo[test] = k_n_cerca(d_sepal, prueba) 
    Candidatos_Test.petal$nuevo[test] = k_n_cerca(d_petal, prueba)
  }
  
  #Escribo un resumen de lo analizado con los datos de los candidatos seg�n sepal y petal.
  write.csv(Candidatos_Test.sepal, paste("./tests/k_", prueba, "/resumen_sepal", ".csv", sep = ""))
  write.csv(Candidatos_Test.petal, paste("./tests/k_", prueba, "/resumen_petal", ".csv", sep = ""))
  
  #Los res�menes son transformados en factores para su correcto an�lisis para la funci�n {confusionMatrix}.
  Candidatos_Test.sepal$original = factor(Candidatos_Test.sepal$original, levels = c('setosa', 'versicolor', 'virginica'))
  Candidatos_Test.sepal$nuevo = factor(Candidatos_Test.sepal$nuevo, levels = c('setosa', 'versicolor', 'virginica'))
  Candidatos_Test.petal$original = factor(Candidatos_Test.petal$original, levels = c('setosa', 'versicolor', 'virginica'))
  Candidatos_Test.petal$nuevo = factor(Candidatos_Test.petal$nuevo, levels = c('setosa', 'versicolor', 'virginica'))
  
  #Asignaci�n de la matriz de confusi�n a una variable para su posterior escritura
  matriz.sepal = confusionMatrix(Candidatos_Test.sepal$nuevo, Candidatos_Test.sepal$original)
  matriz.petal = confusionMatrix(Candidatos_Test.petal$nuevo, Candidatos_Test.petal$original)
  
  #Genero un excel con las matrices de confusi�n. 
  write.csv(matriz.sepal$table, paste("./tests/k_", prueba, "/confusion_matrix_sepal", ".csv", sep = ""))
  write.csv(matriz.petal$table, paste("./tests/k_", prueba, "/confusion_matrix_petal", ".csv", sep = ""))
  precisionTable.sepal[prueba, ] = c(prueba, matriz.sepal$overall[1])
  precisionTable.petal[prueba, ] = c(prueba, matriz.petal$overall[1])
  
  #Plot de los candidatos y clases en Petal. Para cada k (prueba que va de 1 a 20)
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
    Datos_Candidatos$Petal.Length,
    Datos_Candidatos$Petal.Width, 
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
  
  #Plot de los candidatos y clases en Petal. Para cada k (prueba que va de 1 a 20)
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
    Datos_Candidatos$Sepal.Length,
    Datos_Candidatos$Sepal.Width, 
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

#Genero un excel donde resumo la accuracy para cada valor de k.
write.csv(precisionTable.sepal, "./tests/accuracy_sepal.csv")
write.csv(precisionTable.petal, "./tests/accuracy_petal.csv")


#Plot para analizar los accuracy seg�n el valor de k, con una paleta de colores seg�n el nivel de accuracy. Sepal. 
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

#Plot para analizar los accuracy seg�n el valor de k, con una paleta de colores seg�n el nivel de accuracy. Petal. 
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
axis(2, at = format(round(as.numeric(as.vector(tabla_puntos$Var1)), 2), nsmall = 2), las = 1,) #Color por nivel
paleta = hcl.colors(
  length(table(puntos.petal$precision)),
  palette = "Temps"
)
paleta_per = NULL
for (punto in 1:length(table(puntos.petal$precision))) {
  times = tabla_puntos$Freq[punto]
  paleta_per = append(paleta_per, rep(paleta[punto], times)) #Repetici�n de colores para uniformidad visual. 
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

#Gracias.:)
