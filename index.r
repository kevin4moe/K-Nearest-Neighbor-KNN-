
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
  distancia_k = distancia(
    conejos$Sepal.Length[test],
    flores$Sepal.Length,
    conejos$Sepal.Width[test],
    flores$Sepal.Width
  )

  k_1 = data.frame(
    distancia_k,
    Species = flores$Species
  )
  k_1 = k_1[order(k_1$distancia_k),]
  write.csv(k_1[1:10, ], paste("./final/tests/", "k_", test, ".csv", sep = ""))

  for (q in seq(from = 0.2, to = 1, by = 0.05)) {
    k_1_cerca = k_1[k_1$distancia_k < q, ]
    if (length(k_1_cerca[ ,1]) >= 5) {
      species = k_1_cerca$Species[1:5]
      muestra = as.data.frame(table(species))
      muestra = muestra[order(muestra$Freq, decreasing = TRUE), ]
      conejosTest$nuevo[test] = as.vector(muestra$species)[1]
      break()
    }
  }
}
write.csv(conejosTest, paste("./final/tests/", "resumen", ".csv", sep = ""))

# View(flores[conejos,])
plot(
  flores$Sepal.Length,
  flores$Sepal.Width,
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
  conejos$Sepal.Length[1],
  conejos$Sepal.Width[1], 
  type = "p",
  pch = 19,
  col = "blue",
)
# lines(flores[,1:2], type="p", col="green")
# lines(conejos[2,1:2], type="l", col="green")

# plot(flores$Petal.Length, flores$Petal.Width)
