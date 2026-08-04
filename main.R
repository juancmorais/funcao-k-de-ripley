# Bibliotecas 
library(sf)
library(spatstat)
library(spatstat.explore)
library(aspace)

# Carregando a base em .CSV
dados <- read.csv("crimesTraficoDenso.csv", header = T, sep = ";", fileEncoding = "latin1", dec = ",")

# Mapeamento das coordenadas
sf_dados <- st_as_sf(dados, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = 31983)
st_crs(sf_dados)

coordenadas <- st_coordinates(sf_dados)

retangulo = owin(xrange = c(370680.3, 586149.1), yrange = c(7333063, 7522459))

pontos_ppp <- ppp(coordenadas[, 1], coordenadas[, 2], window = retangulo)

# Função Kinhom
Kinhom <- envelope(pontos_ppp, fun=Kinhom, nsim = 99, correction = "border", verbose = T)
plot(Kinhom)
plot(Kinhom, .-r~r)

# Função Linhom
Linhom <- envelope(pontos_ppp, Linhom, correction = "isotropic", nsim = 9999, global = FALSE)
par(cex = 0.8)
plot(Linhom, .-r ~ r, main="", cex.axis=1.2, cex.lab=1, lwd=2)

# Para validar o padrão não homogêneo (não compõe o TCC)
ts<-quadrat.test(pontos_ppp, method = "MonteCarlo")
ts
plot(ts)