library(readxl)
library(dplyr)
cuantias_23 <- read_excel("Trabajo Final 2024 Base de Datos .xlsx", 
                          col_types = c("date", "numeric"))
cer23 <- read_excel("cer23.xlsx", 
                    col_types = c("date", "numeric"))



cuantias_23 <- cuantias_23 %>%
  left_join(cer23, by = "Fecha")

cuantias_23 <- cuantias_23 %>%
  mutate(cuantia24 = `Cuantía` * (cer23$ValorCER[366]/ValorCER))

######################## DESCRIPTIVO ##################################
library(ggplot2)
library(lubridate)

# Crear columna con el mes en formato año-mes

cuantias_23 <- cuantias_23 %>%
  mutate(mes = format(as.Date(Fecha), "%Y-%m"))
ggplot(cuantias_23, aes(x = mes)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Cantidad de cuantías pagadas por mes",
       x = "Mes",
       y = "Cantidad de pagos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####################

ggplot(cuantias_23, aes(x = cuantia24)) +
  geom_histogram(fill = "darkgreen", bins = 30, color = "white") +
  labs(title = "Distribución de cuantia24",
       x = "cuantia24",
       y = "Frecuencia") +
  theme_minimal()


