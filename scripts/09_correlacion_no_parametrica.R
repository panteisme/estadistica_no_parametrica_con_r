# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 9: Correlación no paramétrica
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(ggstatsplot)
library(ggpubr)

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
# 2. EXPLORACIÓN DE DATOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Diagrama de dispersión
# ------------------------------------------------------------------------------

data %>%
  ggplot(aes(x = indice_homa, y = imc)) + 
  geom_point(alpha = 0.6, size = 3, color = "#3498db") +
  geom_smooth(method = "lm", se = TRUE, color = "#e74c3c") +
  labs(
    title = "Relación entre Índice HOMA e IMC",
    x = "Índice HOMA",
    y = "IMC (kg/m²)"
  ) +
  theme_minimal()

# ==============================================================================
# 3. CORRELACIÓN DE SPEARMAN
# ==============================================================================

# Ho: El coeficiente de correlación poblacional entre X e Y es igual a 0
# Ha: El coeficiente de correlación poblacional entre X e Y es diferente de 0

# ------------------------------------------------------------------------------
# 3.1 Correlación con rstatix
# ------------------------------------------------------------------------------

data %>%
  cor_test(imc, indice_homa, method = "spearman")

# ------------------------------------------------------------------------------
# 3.2 Correlación con base R
# ------------------------------------------------------------------------------

cor.test(data$imc, data$indice_homa, method = "spearman")

# ------------------------------------------------------------------------------
# 3.3 Visualización con ggstatsplot
# ------------------------------------------------------------------------------

data %>% 
  ggscatterstats(
    x = indice_homa,
    y = imc,
    xlab = "Índice HOMA",
    ylab = "IMC (kg/m²)",
    type = "nonparametric",
    title = "Correlación de Spearman: Índice HOMA vs IMC"
  )

# ==============================================================================
# 4. ANÁLISIS ESTRATIFICADO POR SEXO
# ==============================================================================

data %>% 
  grouped_ggscatterstats(
    x = indice_homa,
    y = imc,
    grouping.var = sexo,
    xlab = "Índice HOMA",
    ylab = "IMC (kg/m²)",
    type = "nonparametric",
    title.text = "Correlación de Spearman: Índice HOMA vs IMC por Sexo"
  )

# ==============================================================================
# 5. MATRIZ DE CORRELACIONES
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Correlograma con ggstatsplot
# ------------------------------------------------------------------------------

data %>% 
  select(edad, imc, indice_homa) %>%
  ggcorrmat(
    type = "nonparametric",
    colors = c("#B2182B", "white", "#4D4D4D"),
    title = "Matriz de Correlación de Spearman"
  )

# ------------------------------------------------------------------------------
# 5.2 Matriz de correlaciones con rstatix
# ------------------------------------------------------------------------------

data %>%
  select(edad, imc, indice_homa) %>%
  cor_mat(method = "spearman")

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
