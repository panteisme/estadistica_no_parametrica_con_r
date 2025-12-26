# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 6: Pruebas no paramétricas para dos muestras relacionadas
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(ggstatsplot)

# ==============================================================================
# 1. DATOS
# ==============================================================================

puntuacion_inventario_depresion_antes_tratamiento <- c(15, 11, 5, 9, 5, 6, 5, 8, 3, 3, 3, 4, 1, 8, 3, 2, 0, 3, 3, 23, 7, 24, 16, 14, 13, 25, 11, 5, 10, 9, 16, 18, 26, 15, 6, 19, 5, 17)

puntuacion_inventario_depresion_despues_tratamiento <- c(4, 4, 5, 9, 5, 6, 5, 8, 3, 3, 23, 4, 1, 8, 3, 2, 0, 3, 3, 11, 5, 9, 8, 6, 5, 7, 7, 2, 3, 4, 9, 8, 7, 6, 4, 8, 3, 9)

edad <- c(19, 19, 19, 18, 19, 17, 17, 24, 19, 23, 25, 16, 18, 16, 16, 24, 18, 19, 17, 17, 24, 27, 19, 21, 18, 17, 16, 23, 19, 16, 23, 25, 19, 16, 16, 20, 19, 20)

sexo <- c(1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

# Crear tibble
data_depresion <- tibble(
  antes = puntuacion_inventario_depresion_antes_tratamiento,
  despues = puntuacion_inventario_depresion_despues_tratamiento,
  edad = edad,
  sexo = sexo
) %>%
  mutate(
    sexo = fct_recode(
      factor(sexo),
      "Masculino" = "1",
      "Femenino" = "0"
    )
  )

# Verificar estructura
glimpse(data_depresion)

# ==============================================================================

# ==============================================================================
# 2. EXPLORACIÓN DE DATOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Crear variable de diferencia
# ------------------------------------------------------------------------------

data_depresion <- data_depresion %>%
  mutate(diferencia = antes - despues)

# ------------------------------------------------------------------------------
# 2.2 QQ Plots
# ------------------------------------------------------------------------------

# QQ Plot: Puntuación antes del tratamiento
ggplot(data_depresion, aes(sample = antes)) +
  stat_qq(color = "#3498db", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Puntuación Antes del Tratamiento",
    subtitle = "Evaluación de normalidad",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# QQ Plot: Puntuación después del tratamiento
ggplot(data_depresion, aes(sample = despues)) +
  stat_qq(color = "#3498db", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Puntuación Después del Tratamiento",
    subtitle = "Evaluación de normalidad",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# QQ Plot: Diferencia de puntuaciones
ggplot(data_depresion, aes(sample = diferencia)) +
  stat_qq(color = "#3498db", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Diferencia de Puntuaciones",
    subtitle = "Evaluación de normalidad",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 2.3 Subconjunto de datos y resumen con summarytools
# ------------------------------------------------------------------------------

# Seleccionar variables de puntuaciones
data_puntuaciones <- data_depresion %>%
  select(antes, despues, diferencia)

# Resumen descriptivo con summarytools
library(summarytools)
descr(data_puntuaciones)

# ==============================================================================

# ==============================================================================
# 3. PRUEBA DE WILCOXON PARA MUESTRAS PAREADAS
# ==============================================================================

# Ho: La mediana de las diferencias en las puntuaciones de depresión es cero
# Ha: La mediana de las diferencias en las puntuaciones de depresión es diferente de cero

# ------------------------------------------------------------------------------
# 3.1 Prueba de Wilcoxon con base R
# ------------------------------------------------------------------------------

wilcox.test(data_depresion$antes, 
            data_depresion$despues, 
            paired = TRUE, 
            conf.int = TRUE)

# ------------------------------------------------------------------------------
# 3.2 Prueba de Wilcoxon con rstatix
# ------------------------------------------------------------------------------

# Convertir datos a formato largo
library(tidyr)

data_depresion_largo <- data_depresion %>%
  select(antes, despues) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(antes, despues), 
               names_to = "momento", 
               values_to = "puntuacion")

# Prueba de Wilcoxon pareada
data_depresion_largo %>%
  wilcox_test(puntuacion ~ momento, paired = TRUE)

# Tamaño del efecto
data_depresion_largo %>%
  wilcox_effsize(puntuacion ~ momento, paired = TRUE)

# ==============================================================================

# ==============================================================================
# 4. VISUALIZACIÓN CON GGSTATSPLOT
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Gráfica con ggwithinstats
# ------------------------------------------------------------------------------

data_depresion_largo %>%
  ggwithinstats(
    x = momento,
    y = puntuacion,
    type = "nonparametric",
    xlab = "Momento de Evaluación",
    ylab = "Puntuación Inventario de Depresión",
    title = "Prueba de Wilcoxon: Puntuación de Depresión Antes y Después del Tratamiento"
  )

# ==============================================================================

# ==============================================================================
# 5. PRUEBA DE LOS SIGNOS
# ==============================================================================

# Ho: La mediana de las diferencias es cero
# Ha: La mediana de las diferencias es diferente de cero

# ------------------------------------------------------------------------------
# 5.1 Prueba de los Signos con DescTools
# ------------------------------------------------------------------------------

library(DescTools)

SignTest(data_depresion$antes, 
         data_depresion$despues, 
         alternative = "two.sided")

# ------------------------------------------------------------------------------
# 5.2 Prueba de los Signos con BSDA
# ------------------------------------------------------------------------------

library(BSDA)

SIGN.test(data_depresion$antes, 
          data_depresion$despues, 
          alternative = "two.sided")

# ------------------------------------------------------------------------------
# 5.3 Ejemplo con datos ordinales (Escala Likert)
# ------------------------------------------------------------------------------

# Ventaja: La prueba de signos también funciona con datos ordinales

# Crear datos de ejemplo (escala Likert 1-5)
Likert <- c(3, 3, 4, 2, 4, 5, 4, 4, 3, 2, 3, 4, 5, 5, 4, 4, 3, 2, 4, 5)
Prueba <- c(rep(1, 10), rep(2, 10))
Prueba <- factor(Prueba, labels = c("Antes", "Despues"))

# Crear dataframe
data_likert <- data.frame(Likert, Prueba)

# Prueba de signos con rstatix
data_likert %>%
  sign_test(Likert ~ Prueba, alternative = "two.sided")

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================