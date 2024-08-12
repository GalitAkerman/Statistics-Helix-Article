library(dplyr)
library(googlesheets4)
library(ggplot2)
library(pracma)

sheet_url<-"https://docs.google.com/spreadsheets/d/1zwJky-N-a_k3Z5vkbJ-rNAVkt4oC4dhoHU16K1MLg-Y/edit#gid=0" 
Na_wc <- read_sheet(sheet_url, 1) 
Na_wc 

p <- Na_wc |>
  ggplot(aes(x = vm)) +
  geom_point(aes(y = activacion), color = "blue", size = 1) +
  geom_point(aes(y = inactivacion), color = "red", size = 1) +
  theme_minimal()
p

fun_activacion <- approxfun(Na_wc$vm, Na_wc$activacion)
fun_inactivacion <- approxfun(Na_wc$vm, Na_wc$inactivacion)

f <- approxfun(Na_wc$vm, Na_wc$activacion)
curve(f(x), -60, 0, col = "green2")
points(Na_wc$vm, Na_wc$activacion)

f <- approxfun(Na_wc$vm, Na_wc$inactivacion)
curve(f(x), -60, 0, col = "green2")
points(Na_wc$vm, Na_wc$inactivacion)

find_intersection <- function(f, g, interval) {
  # Busca la diferencia entre las curvas
  diff_func <- function(x) f(x) - g(x)
  
  # Encuentra 0's en un intervalo
  root <- uniroot(diff_func, interval = interval)$root
  
  # Devuelva el punto de intersección
  return(root)
}


intersection <- find_intersection(fun_activacion, fun_inactivacion, interval = c(-50, -20))
cat("Intersection point:", intersection)

#Aquí definimos el rango
integration_range <- c(-60, intersection)

# Aquí hacemos la integral
result_activacion <- integrate(fun_activacion, integration_range[1], integration_range[2])

#Imprimimos el resultado
lado_izquierdo <- result_activacion$value
lado_izquierdo

#Aquí definimos el rango
integration_range_right <- c(intersection, 0)

# Aquí hacemos la integral
result_inactivacion_derecho <- integrate(fun_inactivacion, integration_range_right[1], integration_range_right[2])

#Imprimimos el resultado
lado_derecho <- result_inactivacion_derecho$value
lado_derecho
