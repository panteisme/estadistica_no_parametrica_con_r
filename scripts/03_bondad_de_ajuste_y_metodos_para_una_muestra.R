# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 3: Bondad de ajuste y métodos para una muestra
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(janitor)

# ==============================================================================
# 1. DATOS
# ==============================================================================

sexo <- c(2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 
          1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
          1, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 
          2, 1, 2, 2, 1, 1)

edad <- c(62, 40, 41, 39, 59, 43, 60, 36, 58, 40, 40, 83, 61, 52, 33, 51, 
          43, 54, 74, 64, 47, 64, 54, 37, 32, 33, 47, 36, 28, 27, 25, 61, 
          49, 47, 24, 49, 45, 41, 53, 59, 34, 65, 56, 33, 29, 33, 70, 74, 
          20, 39, 52, 42, 43, 63, 57, 50, 45, 58, 54, 49, 26, 47, 48, 36, 
          24, 25)

imc <- c(28.52, 23.33, 20.79, 23.23, 22.54, 29.68, 34.67, 24.7, 31.14, 23.48, 
         26.31, 24.64, 36.03, 30.91, 36.65, 31.18, 26.48, 28.36, 27.84, 29.98, 
         31.66, 27.94, 30.21, 28.15, 23.5, 27.6, 28.5, 27.8, 24.6, 20.7, 24.7, 
         26, 23.3, 30, 23, 28.9, 21.4, 37, 28, 20, 26, 24, 23, 37.6, 28, 24, 
         21, 29, 25, 29, 26, 28, 38, 29, 33, 32.7, 36, 26, 27, 28, 27, 39, 30, 
         21.86, 25.17, 24.7)

estado_nutricional <- c(2, 1, 1, 1, 1, 2, 3, 1, 3, 1, 2, 1, 4, 3, 4, 3, 2, 2, 
                        2, 2, 3, 2, 3, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 3, 1, 2, 
                        1, 4, 2, 1, 2, 1, 1, 4, 2, 1, 1, 2, 2, 2, 2, 2, 4, 2, 
                        3, 3, 4, 2, 2, 2, 2, 4, 3, 1, 2, 1)

indice_homa <- c(1.14, 1.55, 1.4, 1.16, 1.08, 1.42, 3.66, 0.88, 0.23, 0.88, 
                 3.17, 0.45, 3.299, 3.01, 11.9, 1.79, 1.06, 1.77, 3.19, 2.71, 
                 4.87, 1.84, 6.12, 3.64, 2.96, 1.27, 2.84, 2.61, 4.24, 0.48, 
                 1.16, 1.23, 0.51, 4.49, 1.05, 1.64, 0.51, 2.933, 1.32, 0.64, 
                 1.27, 1.79, 3.41, 19.839, 2.48, 1.32, 2.82, 0.52, 2.5, 0.66, 
                 2.51, 1.48, 2.99, 2.59, 0.49, 0.91, 4.18, 0.95, 3.86, 1.18, 
                 0.43, 3.86, 4.08, 0.67, 1.3, 1.16)

insulinorresistencia <- c(2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 
                          1, 2, 1, 2, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 
                          2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                          2, 2, 1, 2, 1, 2, 2, 1, 1, 2, 2, 2)

data <- tibble(
  sexo = sexo,
  edad = edad,
  imc = imc,
  estado_nutricional = estado_nutricional,
  indice_homa = indice_homa,
  insulinorresistencia = insulinorresistencia
) %>%
  mutate(
    sexo = fct_recode(
      factor(sexo),
      "Masculino" = "1",
      "Femenino" = "2"
    ),
    estado_nutricional = fct_recode(
      factor(estado_nutricional),
      "Normal" = "1",
      "Sobrepeso" = "2",
      "Obesidad 1" = "3",
      "Obesidad 2" = "4"
    ),
    insulinorresistencia = fct_recode(
      factor(insulinorresistencia),
      "Si" = "1",
      "No" = "2"
    )
  )

# ==============================================================================
# 2. PRUEBAS DE BONDAD DE AJUSTE
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Prueba de Shapiro-Wilk
# ------------------------------------------------------------------------------

# Ho: Los datos provienen de una población distribuida de forma normal
# Ha: Los datos provienen de una población no distribuida de forma normal

# Función nativa de R
shapiro.test(indice_homa)

# Función del paquete rstatix
data %>%
  shapiro_test(indice_homa)

# ------------------------------------------------------------------------------
# 2.2 Otras pruebas para evaluar normalidad
# ------------------------------------------------------------------------------

nortest::ad.test(indice_homa)

DescTools::JarqueBeraTest(indice_homa, robust = TRUE, method = c("chisq"), na.rm = T)

ks.test(indice_homa, "pnorm")

moments::agostino.test((indice_homa))

# ------------------------------------------------------------------------------
# 2.3 Pruebas para evaluar parámetro de locación
# ------------------------------------------------------------------------------

# Ho: La mediana de la variable de interés es igual a X
# Ha: La mediana de la variable de interés es diferente de X

data %>%
  summarize(Mediana=median(indice_homa), Q1=quantile(indice_homa, probs = 0.25), 
      Q3=quantile(indice_homa, probs = 0.75))

data %>% 
  wilcox_test(indice_homa ~ 1, mu = 2.9)

data %>% 
  sign_test(indice_homa ~ 1, mu = 2.9)

data %>%
    wilcox_effsize(indice_homa ~ 1, mu = 2.9)

# ------------------------------------------------------------------------------
# 2.4 Prueba de chi cuadrado de bondad de ajuste
# ------------------------------------------------------------------------------

datos_produccion = c(1, 2, 3, 2, 2, 3, 1, 1, 1, 2, 3, 1, 2, 3, 3, 2, 1, 1, 2, 
                     1, 2, 3, 3, 2, 2, 1, 2, 2, 1, 2, 1, 2, 1, 3, 2, 3, 1, 2, 
                     2, 3, 2, 1, 1, 1, 2)

ggplot(data = NULL, aes(x = factor(datos_produccion))) +
  geom_bar(fill = "skyblue") +
  labs(x = "Operario",
       y = "Frecuencia llamadas contestadas")

# Ho:Los 3 operarios responden a la misma cantidad de llamadas
# Ha: Los 3 operarios responden una cantidad diferente de llamadas

chisq.test(table(datos_produccion), p = c(1/3, 1/3, 1/3)) 

tabla = tabyl(datos_produccion)

chisq_test(tabla$n, p = c(1/3, 1/3, 1/3))

# ------------------------------------------------------------------------------
# 2.5 Prueba de Rachas
# ------------------------------------------------------------------------------

# ¿Los pacientes ingresaron aleatoriamente o hay algún patrón por sexo?

# Ho: Los pacientes ingresaron aleatoriamente (sin patrón por sexo)
# Ha: Hay un patrón en el orden de ingreso (ej: bloques de hombres/mujeres)

tseries::runs.test(data$sexo)

# ------------------------------------------------------------------------------
# 2.6 Prueba para detectar valores atípicos
# ------------------------------------------------------------------------------

data %>%
  identify_outliers(indice_homa)

ggplot(data = data, aes(x = "", y = indice_homa)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Boxplot de Índice HOMA", y = "Valor") +
  theme_minimal()

# Ho: No existe un valor atípico
# Ha: El valor más pequeño o más grande de los datos es un valor atípico

outliers::grubbs.test(indice_homa, two.sided = TRUE)

rstatix::is_outlier(indice_homa)
rstatix::is_extreme(indice_homa)

tabyl(rstatix::is_outlier(indice_homa))
tabyl(rstatix::is_extreme(indice_homa))

# ------------------------------------------------------------------------------
# 2.7 Prueba Binomial

# Evalúa si una proporción observada difiere de una proporción esperada

# ¿La proporción de pacientes CON insulinorresistencia es 30%?

data %>% 
  tabyl(insulinorresistencia) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

# Ho: La proporción de pacientes con insulinorresistencia es 30% (p = 0.30)
# Ha: La proporción de pacientes con insulinorresistencia es diferente de 30%

n_total <- length(data$insulinorresistencia)
n_si <- sum(data$insulinorresistencia == "Si")

binom.test(
  x = n_si,              # Número de "éxitos" (pacientes CON insulinorresistencia)
  n = n_total,           # Tamaño de muestra total
  p = 0.30,              # Proporción esperada según hipótesis nula (30%)
  alternative = "two.sided"  # Prueba bilateral
)

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
