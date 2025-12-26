# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 4: Pruebas no paramétricas para dos muestras independientes
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(ggstatsplot)
library(ggpubr)
library(DescTools)
library(kSamples)
library(RVAideMemoire)

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
# 2.1 Estadísticos descriptivos por grupo
# ------------------------------------------------------------------------------

# IMC según Insulinorresistencia
data %>%
  group_by(insulinorresistencia) %>%
  summarize(
    n = n(),
    Mediana = median(imc),
    Q1 = quantile(imc, probs = 0.25),
    Q3 = quantile(imc, probs = 0.75),
    Media = mean(imc),
    DE = sd(imc)
  )

# ------------------------------------------------------------------------------
# 2.2 Prueba de normalidad por grupos
# ------------------------------------------------------------------------------

# Shapiro-Wilk para IMC según Insulinorresistencia
data %>%
  group_by(insulinorresistencia) %>%
  shapiro_test(imc)

# ------------------------------------------------------------------------------
# 2.3 Visualización exploratoria
# ------------------------------------------------------------------------------

# Boxplot: IMC según Insulinorresistencia
data %>%
  ggplot(aes(x = insulinorresistencia, y = imc, fill = insulinorresistencia)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("Si" = "#e74c3c", "No" = "#3498db")) +
  labs(
    title = "IMC según Insulinorresistencia",
    x = "Insulinorresistencia",
    y = "IMC (kg/m²)"
  ) +
  theme_minimal()

# ==============================================================================
# 3. PRUEBA DE MANN-WHITNEY (WILCOXON RANK SUM TEST)
# ==============================================================================

# Ho: La mediana poblacional del IMC en ambos grupos no varía significativamente
# Ha: La mediana poblacional del IMC en ambos grupos sí varía significativamente

# ------------------------------------------------------------------------------
# 3.1 Prueba de Mann-Whitney: IMC según Insulinorresistencia
# ------------------------------------------------------------------------------

# Con el paquete rstatix
data %>%
  wilcox_test(imc ~ insulinorresistencia)

# Tamaño del efecto
data %>%
  wilcox_effsize(imc ~ insulinorresistencia)

# Con comandos de base R
wilcox.test(imc ~ insulinorresistencia, data = data, 
            paired = FALSE, conf.int = TRUE)

# ------------------------------------------------------------------------------
# 3.2 Gráfica con ggpubr
# ------------------------------------------------------------------------------

data %>%
  ggboxplot(
    x = "insulinorresistencia", 
    y = "imc",
    color = "insulinorresistencia", 
    palette = "jco",
    add = "jitter",
    ylim = c(15, 45)
  ) +
  labs(
    title = "Comparación de IMC según Insulinorresistencia",
    x = "Insulinorresistencia",
    y = "IMC (kg/m²)"
  )

# ------------------------------------------------------------------------------
# 3.3 Gráfica con ggstatsplot
# ------------------------------------------------------------------------------

data %>% 
  ggbetweenstats(
    x = insulinorresistencia,
    y = imc,
    xlab = "Insulinorresistencia",
    ylab = "IMC (kg/m²)",
    type = "nonparametric",
    title = "Prueba de Mann-Whitney: IMC según Insulinorresistencia"
  )

# ==============================================================================
# 4. VERIFICACIÓN DE SUPUESTOS
# ==============================================================================

# La prueba de Wilcoxon/Mann-Whitney es válida bajo el supuesto que:
# 1. Las distribuciones en ambos grupos son similares
# 2. Existe homocedasticidad (varianzas homogéneas)

# ------------------------------------------------------------------------------
# 4.1 Pruebas para evaluar la equivalencia de distribuciones
# ------------------------------------------------------------------------------

# Filtrar grupos
d_si <- data %>% filter(insulinorresistencia == "Si")
d_no <- data %>% filter(insulinorresistencia == "No")

# 1. Kolmogorov-Smirnov Test
ks.test(d_si$imc, d_no$imc)

# 2. Anderson-Darling Test
kSamples::ad.test(d_si$imc, d_no$imc)

# ------------------------------------------------------------------------------
# 4.2 Prueba de homocedasticidad
# ------------------------------------------------------------------------------

DescTools::LeveneTest(imc ~ insulinorresistencia, data = data)

data %>%
  levene_test(imc ~ insulinorresistencia)

# ==============================================================================
# 5. PRUEBA DE FLIGNER-POLICELLO
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Test Fligner-Policello: IMC según Insulinorresistencia
# ------------------------------------------------------------------------------

# Ventaja: No requiere que las distribuciones sean idénticas (más robusto que Mann-Whitney)

# Filtrar grupos
d_si <- data %>% filter(insulinorresistencia == "Si")
d_no <- data %>% filter(insulinorresistencia == "No")

# Prueba de Fligner-Policello
RVAideMemoire::fp.test(d_si$imc, d_no$imc)

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
