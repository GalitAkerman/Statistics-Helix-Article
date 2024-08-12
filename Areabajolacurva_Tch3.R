#Area bajo la Curva de Window Current para TCH3

library(tidyverse)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(pracma)

sheet_url<-"https://docs.google.com/spreadsheets/d/1AHwvJDlp8dhcNATPd3DYnlDMVsSkOWS_fHKCC7VgAEo/edit?gid=613871045#gid=613871045"
taos1_wc <- googlesheets4::read_sheet(sheet_url, 12) # se necesita el paquete googlesheets4
taos1_wc 


################# TIEMPO 0
#Grafico de mis datos Probabildiad de apertura para tiempo 5 TAOS1
p <- taos1_wc |>
  filter(tiempo == 0) |> #cambiar tiempos
  ggplot(aes(x = vm)) +
  geom_line(aes(y = activacion), color = "blue", size = 1) +
  geom_line(aes(y = inactivacion), color = "red", size = 1) +
  theme_minimal()
p

tiempo_0 <- taos1_wc |>
  filter(tiempo == 0)


# aproximar dos funciones matemáticas  a activación e inactivación

fun_activacion <- approxfun(tiempo_0$vm, tiempo_0$activacion)
fun_inactivacion <- approxfun(tiempo_0$vm, tiempo_0$inactivacion)

# Qué tan buenas son las aproximaciones? Quiero asegurarme que la funcion que hice
#se adecue perfectamente a mis datos

f <- approxfun(tiempo_0$vm, tiempo_0$activacion)
curve(f(x), -60, 0, col = "green2")
points(tiempo_0$vm, tiempo_0$activacion)

f <- approxfun(tiempo_0$vm, tiempo_0$inactivacion)
curve(f(x), -60, 0, col = "green2")
points(tiempo_0$vm, tiempo_0$inactivacion)

# Intentar aproximar un punto medio basado en dos funciones y un intervalo donde buscar

find_intersection <- function(f, g, interval) {
  # Differencia
  diff_func <- function(x) f(x) - g(x)
  
  # Find the root of the difference function within the specified interval
  root <- uniroot(diff_func, interval = interval)$root
  
  # Return the intersection point
  return(root)
}


intersection <- find_intersection(fun_activacion, fun_inactivacion, interval = c(-50, -20))
cat("Intersection point:", intersection)


# Nuestros primeros limites de integracion  van a ser de -60 hasta el punte de interseccion que
# aproximamos

# Define the integration range
integration_range <- c(-60, intersection)

# Perform the integration
result_activacion <- integrate(fun_activacion, integration_range[1], integration_range[2])
result_inactivacion <- integrate(fun_inactivacion, integration_range[1], integration_range[2])

# Buscamos inactivacion hasta el punto de interseccion

lado_izquierdo <- result_activacion$value
lado_izquierdo

# Definimos nuevo rango de integracion (lado derecho)

integration_range_right <- c(intersection, 0)

# Resta opuesta

result_activacion_derecho <- integrate(fun_activacion, integration_range_right[1], integration_range_right[2])
result_inactivacion_derecho <- integrate(fun_inactivacion, integration_range_right[1], integration_range_right[2])


# Restamos inactivacion a activacion

lado_derecho <- result_inactivacion_derecho$value
lado_derecho

# Sumamos para encontrar las areas deseadas


area <- lado_derecho + lado_izquierdo # este resultado es subestimado porque la activacion baja de 0 en algun momento
area


################# TIEMPO 10
#Grafico de mis datos Probabildiad de apertura para tiempo 5 TAOS4
p <- taos1_wc |>
  filter(tiempo == 10) |> #cambiar tiempos
  ggplot(aes(x = vm)) +
  geom_line(aes(y = activacion), color = "blue", size = 1) +
  geom_line(aes(y = inactivacion), color = "red", size = 1) +
  theme_minimal()
p

tiempo_10 <- taos1_wc |>
  filter(tiempo == 10)


# aproximar dos funciones matemáticas  a activación e inactivación

fun_activacion <- approxfun(tiempo_10$vm, tiempo_10$activacion)
fun_inactivacion <- approxfun(tiempo_10$vm, tiempo_10$inactivacion)

# Qué tan buenas son las aproximaciones? Quiero asegurarme que la funcion que hice
#se adecue perfectamente a mis datos

f <- approxfun(tiempo_10$vm, tiempo_10$activacion)
curve(f(x), -60, 0, col = "green2")
points(tiempo_10$vm, tiempo_10$activacion)

f <- approxfun(tiempo_10$vm, tiempo_10$inactivacion)
curve(f(x), -60, 0, col = "green2")
points(tiempo_10$vm, tiempo_10$inactivacion)

# Intentar aproximar un punto medio basado en dos funciones y un intervalo donde buscar

intersection <- find_intersection(fun_activacion, fun_inactivacion, interval = c(-50, -20))
cat("Intersection point:", intersection)


# Nuestros primeros limites de integracion  van a ser de -60 hasta el punte de interseccion que
# aproximamos

# Define the integration range
integration_range <- c(-60, intersection)

# Perform the integration
result_activacion <- integrate(fun_activacion, integration_range[1], integration_range[2])
result_inactivacion <- integrate(fun_inactivacion, integration_range[1], integration_range[2])

# Buscamos inactivacion hasta el punto de interseccion

lado_izquierdo <- result_activacion$value
lado_izquierdo

# Definimos nuevo rango de integracion (lado derecho)

integration_range_right <- c(intersection, 0)

# Resta opuesta

result_activacion_derecho <- integrate(fun_activacion, integration_range_right[1], integration_range_right[2])
result_inactivacion_derecho <- integrate(fun_inactivacion, integration_range_right[1], integration_range_right[2])


# Restamos inactivacion a activacion

lado_derecho <- result_inactivacion_derecho$value
lado_derecho

# Sumamos para encontrar las areas deseadas


area <- lado_derecho + lado_izquierdo # este resultado es subestimado porque la activacion baja de 0 en algun momento
area



################# TIEMPO 20
#Grafico de mis datos Probabildiad de apertura para tiempo 5 TAOS4
p <- taos1_wc |>
  filter(tiempo == 20) |> #cambiar tiempos
  ggplot(aes(x = vm)) +
  geom_line(aes(y = activacion), color = "blue", size = 1) +
  geom_line(aes(y = inactivacion), color = "red", size = 1) +
  theme_minimal()
p

tiempo_20 <- taos1_wc |>
  filter(tiempo == 20)


# aproximar dos funciones matemáticas  a activación e inactivación

fun_activacion <- approxfun(tiempo_20$vm, tiempo_20$activacion)
fun_inactivacion <- approxfun(tiempo_20$vm, tiempo_20$inactivacion)

# Qué tan buenas son las aproximaciones? Quiero asegurarme que la funcion que hice
#se adecue perfectamente a mis datos

f <- approxfun(tiempo_20$vm, tiempo_20$activacion)
curve(f(x), -60, 0, col = "green2")
points(tiempo_20$vm, tiempo_20$activacion)

f <- approxfun(tiempo_0$vm, tiempo_0$inactivacion)
curve(f(x), -60, 0, col = "green2")
points(tiempo_0$vm, tiempo_0$inactivacion)

# Intentar aproximar un punto medio basado en dos funciones y un intervalo donde buscar

intersection <- find_intersection(fun_activacion, fun_inactivacion, interval = c(-50, -20))
cat("Intersection point:", intersection)


# Nuestros primeros limites de integracion  van a ser de -60 hasta el punte de interseccion que
# aproximamos

# Define the integration range
integration_range <- c(-60, intersection)

# Perform the integration
result_activacion <- integrate(fun_activacion, integration_range[1], integration_range[2])
result_inactivacion <- integrate(fun_inactivacion, integration_range[1], integration_range[2])

# Buscamos inactivacion hasta el punto de interseccion

lado_izquierdo <- result_activacion$value
lado_izquierdo

# Definimos nuevo rango de integracion (lado derecho)

integration_range_right <- c(intersection, 0)

# Resta opuesta

result_activacion_derecho <- integrate(fun_activacion, integration_range_right[1], integration_range_right[2])
result_inactivacion_derecho <- integrate(fun_inactivacion, integration_range_right[1], integration_range_right[2])


# Restamos inactivacion a activacion

lado_derecho <- result_inactivacion_derecho$value
lado_derecho

# Sumamos para encontrar las areas deseadas


area <- lado_derecho + lado_izquierdo # este resultado es subestimado porque la activacion baja de 0 en algun momento
area

