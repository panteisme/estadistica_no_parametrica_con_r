# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 8: Pruebas de asociación para variables categóricas
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(janitor)
library(coin)
library(ggstatsplot)
library(DescTools)

# ==============================================================================
# 1. EJEMPLO 1: TABLAS 2x2 - BAJO PESO PRENATAL VS PARTO PREMATURO
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Crear tabla de contingencia
# ------------------------------------------------------------------------------

bajo_peso <- matrix(c(57, 81, 54, 166),
                    nrow = 2, byrow = TRUE,
                    dimnames = list(
                      "Bajo_peso_prenatal" = c("Si", "No"),
                      "Parto_prematuro" = c("Si", "No")
                    ))

# Visualizar tabla con totales
addmargins(bajo_peso)

# Verificar frecuencias esperadas
chisq.test(bajo_peso)$expected

# ------------------------------------------------------------------------------
# 1.2 Pruebas de asociación
# ------------------------------------------------------------------------------

# Chi-cuadrado de Pearson (sin corrección)
chisq.test(bajo_peso, correct = FALSE)

# Chi-cuadrado con corrección de Yates
chisq.test(bajo_peso, correct = TRUE)

# ==============================================================================
# 2. EJEMPLO 2: TABLAS 2x2 - MUTACIÓN BRCA1 VS RESPUESTA A TERAPIA
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Crear tabla de contingencia
# ------------------------------------------------------------------------------

mutacion_brca <- matrix(c(8, 2, 2, 6), 
                        nrow = 2, byrow = TRUE,
                        dimnames = list(
                          "Mutacion_BRCA1" = c("Si", "No"),
                          "Respuesta" = c("Si", "No")
                        ))

addmargins(mutacion_brca)

# Verificar frecuencias esperadas
chisq.test(mutacion_brca)$expected

# ------------------------------------------------------------------------------
# 2.2 Prueba Exacta de Fisher (frecuencias bajas)
# ------------------------------------------------------------------------------

fisher.test(mutacion_brca)

# ==============================================================================
# 3. EJEMPLO 3: TABLAS rxc - ESTADO NUTRICIONAL VS INSULINORRESISTENCIA
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Datos
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# 3.2 Exploración de datos
# ------------------------------------------------------------------------------

# Tabla de contingencia con porcentajes
data %>%
  tabyl(estado_nutricional, insulinorresistencia) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Crear matriz de contingencia
tabla_contingencia <- table(data$estado_nutricional, data$insulinorresistencia)
addmargins(tabla_contingencia)

# Verificar frecuencias esperadas
chisq.test(tabla_contingencia)$expected

# ------------------------------------------------------------------------------
# 3.3 Visualización
# ------------------------------------------------------------------------------

# Assocplot
assocplot(tabla_contingencia, 
          main = "Gráfico de Asociación")

# Barplot con ggplot2
data %>%
  ggplot(aes(x = estado_nutricional, fill = insulinorresistencia)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("Si" = "#e74c3c", "No" = "#3498db")) +
  labs(
    title = "Asociación entre Estado Nutricional e Insulinorresistencia",
    x = "Estado Nutricional", 
    y = "Proporción",
    fill = "Insulinorresistencia"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# 3.4 Pruebas de asociación
# ------------------------------------------------------------------------------

# Chi-cuadrado de Pearson
chisq.test(tabla_contingencia, correct = FALSE)

# Prueba Exacta de Fisher con simulación Monte Carlo
fisher.test(tabla_contingencia, simulate.p.value = TRUE, B = 10000)

# ==============================================================================
# 4. PRUEBA DE TENDENCIA LINEAL
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Test de tendencia
# ------------------------------------------------------------------------------

# Preparar datos
scores <- c(1, 2, 3, 4)
casos <- tabla_contingencia[, "Si"]
totales <- rowSums(tabla_contingencia)

# Prueba de tendencia con base R
prop.trend.test(casos, totales, scores)

# Test de linealidad con coin
lbl_test(tabla_contingencia)

# Test de tendencia con rstatix
tabla_para_tendencia <- data %>%
  tabyl(estado_nutricional, insulinorresistencia) %>%
  select(-estado_nutricional) %>%
  as.matrix()

prop_trend_test(tabla_para_tendencia)

# ------------------------------------------------------------------------------
# 4.2 Visualización de tendencia
# ------------------------------------------------------------------------------

# Calcular proporciones
prop_ir <- casos / totales * 100

# Gráfico con base R
par(mar = c(5, 5, 4, 2))
bp <- barplot(prop_ir, 
              names.arg = c("Normal", "Sobrepeso", "Obesidad 1", "Obesidad 2"),
              ylab = "Proporción con IR (%)",
              xlab = "Estado Nutricional",
              main = "Tendencia de Insulinorresistencia",
              col = c("#E8F4F8", "#B3D9E8", "#7FB3D5", "#4A90C4"),
              ylim = c(0, 100))

points(bp, prop_ir, pch = 19, col = "red", cex = 1.5)
lines(bp, prop_ir, col = "red", lwd = 2, lty = 2)
text(bp, prop_ir + 5, paste0(round(prop_ir, 1), "%"), font = 2)
grid(nx = NA, ny = NULL)

# Gráfico con ggplot2
data_tendencia <- data.frame(
  estado = c("Normal", "Sobrepeso", "Obesidad 1", "Obesidad 2"),
  proporcion = prop_ir
)

ggplot(data_tendencia, aes(x = factor(estado, levels = estado), y = proporcion, group = 1)) +
  geom_bar(stat = "identity", fill = "#3498db", alpha = 0.7) +
  geom_line(color = "#e74c3c", linewidth = 1.2, linetype = "dashed") +
  geom_point(color = "#e74c3c", size = 3) +
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -1, fontface = "bold") +
  labs(
    title = "Tendencia de Insulinorresistencia según Estado Nutricional",
    x = "Estado Nutricional",
    y = "Proporción con Insulinorresistencia (%)"
  ) +
  ylim(0, max(prop_ir) * 1.2) +
  theme_minimal()

# ------------------------------------------------------------------------------
# 4.3 Visualización con ggstatsplot
# ------------------------------------------------------------------------------

data %>%
  ggbarstats(
    x = insulinorresistencia,
    y = estado_nutricional,
    xlab = "Insulinorresistencia",
    ylab = "Estado Nutricional",
    title = "Asociación entre Estado Nutricional e Insulinorresistencia",
    results.subtitle = FALSE,
    bf.message = FALSE
  )

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
