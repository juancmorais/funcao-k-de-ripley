# Bibliotecas
library(sf)
library(spatstat)
library(spatstat.explore)
library(aspace)
library(geobr)

# Carregando a base em .CSV
dados <- read.csv("crimesTraficoDenso.csv", header = T, sep = ";", fileEncoding = "latin1", dec = ",")

# Mapeamento das coordenadas
sf_dados <- st_as_sf(dados, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = 31983)
st_crs(sf_dados)

coordenadas <- st_coordinates(sf_dados)

sf_dados$POPULAÇÃO <- as.numeric(as.character(sf_dados$POPULAÇÃO))
pesos <- sf_dados$POPULAÇÃO
View(pesos)

calc_sdd(points = coordenadas, weighted = TRUE, weights = pesos, options(max.print = 1938))

##-------------------------------------------##

# Testes - Janela
vale <- read_intermediate_region(code_intermediate = 3511) %>%
  st_transform(crs = 31983)
class(vale)
vale_win <- as.owin(vale)

retangulo = owin(xrange = c(370680.3, 586149.1), yrange = c(7333063, 7522459))
plot(retangulo)

pontos_ppp <- ppp(coordenadas[, 1], coordenadas[, 2], window = retangulo)
plot(pontos_ppp)

# Summary
summary(vale_win)

# Help (documentação)
help(envelope)

##-------------------------------------------##

# Função L
L <- envelope(pontos_ppp, fun=Lest, nsim = 99, sigma = 25554.08, correction = "isotropic", verbose = T)
plot(L)
plot(L, .-r~r) #mudando a visualização

# Função Kinhom
Kinhom <- envelope(pontos_ppp, fun=Kinhom, nsim = 99, sigma = 25554.08, correction = "isotropic", verbose = T)
plot(Kinhom)
plot(Kinhom, .-r~r)

# Função Linhom
Linhom <- envelope(pontos_ppp, Linhom, nsim = 99, sigma = 25554.08, correction = "isotropic", global = FALSE ,verbose = T)
plot(Linhom)
par(cex = 0.8)
plot(Linhom, .-r~r, main = "")