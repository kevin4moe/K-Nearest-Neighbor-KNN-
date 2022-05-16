
flores = iris

distancia <- function(x1, x2, y1, y2) {
  resultado = sqrt((x1 - x2)^2 + (y1 - y2)^2)
  return(resultado)
}
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

candidatos = random_num_list(20)
conejos = flores[candidatos, ]
flores = flores[-candidatos, ]
conejosTest = data.frame(original = conejos$Species, nuevo = "")

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

  # Creación de un data frame con las distancias y sus respectivas distancias
  k_n = data.frame(
    distancia_sepal,
    distancia_petal,
    Species = flores$Species
  )
  k_n = k_n[order(k_n$distancia_sepal),]
  write.csv(k_n[1:10, ], paste("./tests/", "k_", test, ".csv", sep = ""))

  for (q in seq(from = 0.2, to = 1, by = 0.05)) {
    k_n_cerca = k_n[k_n$distancia_sepal < q, ]
    if (length(k_n_cerca[ ,1]) >= 5) {
      species = k_n_cerca$Species[1:5]
      muestra = as.data.frame(table(species))
      muestra = muestra[order(muestra$Freq, decreasing = TRUE), ]
      conejosTest$nuevo[test] = as.vector(muestra$species)[1]
      break
    }
  }
}
write.csv(conejosTest, paste("./tests/", "resumen", ".csv", sep = ""))

point_size = 1
png(file="Petal_plot.png", width = 2000, height = 2000, units = "px", pointsize = 30)
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

png(file="Sepal_plot.png", width = 2000, height = 2000, units = "px", pointsize = 30)
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
