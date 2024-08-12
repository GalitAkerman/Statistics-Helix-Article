#Area bajo la Curva de Window Current TCH4

library(dplyr)
library(googlesheets4)
library(ggplot2)
library(pracma)

sheet_url<-"https://docs.google.com/spreadsheets/d/18K_X5SrlpTDJMrf7kePmlAo8F_XZo_4Ny-kpJd98qmo/edit#gid=450749327"
taos4_wc <- read_sheet(sheet_url, 8) # se necesita el paquete googlesheets4
taos4_wc 

p <- taos4_wc |> 
  filter(tiempo == 5) |> #cambiar tiempos
  ggplot(aes(x = vm)) +
  geom_line(aes(y = activacion), color = "blue", size = 1) +
  geom_line(aes(y = inactivacion), color = "red", size = 1) +
  theme_minimal()

subset_5 <- taos4_wc |>
  filter(tiempo == 5)

area_inact_5<-trapz(subset_0$vm, subset_5$inactivacion)
area_act_5<-trapz(subset_0$vm, subset_5$activacion)

C <- area_act_5+area_inact_5
X1 <- C - area_act_5
X2 <- C - area_inact_5 

Xf <- X1 + X2

WindowCurrent_5 <- area_act_5 - area_inact_5
abs(WindowCurrent_5)

