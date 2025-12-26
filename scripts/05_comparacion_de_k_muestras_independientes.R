# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 5: Pruebas no paramétricas para k muestras independientes
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(ggstatsplot)
library(ggpubr)
library(DescTools)
library(lattice)
library(dunn.test)
library(FSA)
library(agricolae)
library(asbio)
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

# Índice HOMA según Estado Nutricional
data %>%
  group_by(estado_nutricional) %>%
  summarize(
    n = n(),
    Mediana = median(indice_homa),
    Q1 = quantile(indice_homa, probs = 0.25),
    Q3 = quantile(indice_homa, probs = 0.75),
    Media = mean(indice_homa),
    DE = sd(indice_homa)
  )

# ------------------------------------------------------------------------------
# 2.2 Prueba de normalidad por grupos
# ------------------------------------------------------------------------------

# Shapiro-Wilk para Índice HOMA según Estado Nutricional
data %>%
  group_by(estado_nutricional) %>%
  shapiro_test(indice_homa)

# ------------------------------------------------------------------------------
# 2.3 Visualización exploratoria
# ------------------------------------------------------------------------------

# Boxplot: Índice HOMA según Estado Nutricional
data %>%
  ggplot(aes(x = estado_nutricional, y = indice_homa, fill = estado_nutricional)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c(
    "Normal" = "#27ae60",
    "Sobrepeso" = "#f39c12",
    "Obesidad 1" = "#e67e22",
    "Obesidad 2" = "#e74c3c"
  )) +
  labs(
    title = "Índice HOMA según Estado Nutricional",
    x = "Estado Nutricional",
    y = "Índice HOMA"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Histogramas por grupo con lattice
histogram(~ indice_homa | estado_nutricional,
          data = data,
          layout = c(1, 4),
          xlab = "Índice HOMA",
          main = "Distribución del Índice HOMA por Estado Nutricional")

# ------------------------------------------------------------------------------
# 2.4 Prueba de homocedasticidad
# ------------------------------------------------------------------------------

# Levene Test
DescTools::LeveneTest(indice_homa ~ estado_nutricional, data = data)

data %>%
  levene_test(indice_homa ~ estado_nutricional)

# ==============================================================================
# 3. PRUEBA DE KRUSKAL-WALLIS
# ==============================================================================

# Ho: Las medianas poblacionales de Índice HOMA no varían según estado nutricional
# Ha: Las medianas poblacionales de Índice HOMA varían según estado nutricional

# ------------------------------------------------------------------------------
# 3.1 Prueba de Kruskal-Wallis
# ------------------------------------------------------------------------------

# Con el paquete rstatix
data %>%
  kruskal_test(indice_homa ~ estado_nutricional)

# Tamaño del efecto
data %>%
  kruskal_effsize(indice_homa ~ estado_nutricional)

# Con comandos de base R
kruskal.test(indice_homa ~ estado_nutricional, data = data)

# ==============================================================================
# 4. COMPARACIONES MÚLTIPLES POST-HOC
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Test de Dunn con el paquete FSA (con corrección de Bonferroni)
# ------------------------------------------------------------------------------

dunnTest(data$indice_homa, data$estado_nutricional, 
          kw = TRUE, 
          method = "bonferroni")

# Otros métodos de corrección disponibles:
# method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

# ------------------------------------------------------------------------------
# 4.2 Test de Dunn con paquete agricolae
# ------------------------------------------------------------------------------

resultado_agricolae <- kruskal(data$indice_homa, 
                               data$estado_nutricional, 
                               group = FALSE)
resultado_agricolae

# ------------------------------------------------------------------------------
# 4.3 Comparaciones pareadas de Kruskal-Wallis con paquete asbio
# ------------------------------------------------------------------------------

pairw.kw(data$indice_homa, data$estado_nutricional)

# ==============================================================================
# 5. PRUEBA DE LA MEDIANA (MOOD'S MEDIAN TEST)
# ==============================================================================

# Alternativa a Kruskal-Wallis cuando hay muchos empates o datos extremos

RVAideMemoire::mood.medtest(indice_homa ~ estado_nutricional, data = data)


# ==============================================================================
# 6. USO DE ggtatsplot
# ==============================================================================

data %>% 
  ggbetweenstats(
    x = estado_nutricional,
    y = indice_homa,
    xlab = "Estado Nutricional",
    ylab = "Índice HOMA",
    type = "nonparametric",
    title = "Prueba de Kruskal-Wallis: Índice HOMA según Estado Nutricional"
  )

# ==============================================================================
# 7. PRUEBA DE JONCKHEERE-TERPSTRA (TEST DE TENDENCIA)
# ==============================================================================

# Evalúa si existe una tendencia monotónica entre grupos ordenados
# Más potente que Kruskal-Wallis cuando hay orden natural en los grupos

# ------------------------------------------------------------------------------
# 7.1 Aplicación: Índice HOMA según Estado Nutricional
# ------------------------------------------------------------------------------

# Ho: No existe tendencia monotónica en el Índice HOMA según estado nutricional
# Ha: Existe tendencia creciente en el Índice HOMA según estado nutricional

JonckheereTerpstraTest(
  data$indice_homa,
  data$estado_nutricional,
  alternative = "increasing"
)

# ------------------------------------------------------------------------------
# 7.2 Visualización de la tendencia
# ------------------------------------------------------------------------------

data %>%
  ggplot(aes(x = estado_nutricional, y = indice_homa, fill = estado_nutricional)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  stat_summary(fun = median, geom = "line", aes(group = 1), 
               color = "red", linewidth = 1.2, linetype = "dashed") +
  stat_summary(fun = median, geom = "point", 
               color = "red", size = 3) +
  scale_fill_manual(values = c(
    "Normal" = "#27ae60",
    "Sobrepeso" = "#f39c12",
    "Obesidad 1" = "#e67e22",
    "Obesidad 2" = "#e74c3c"
  )) +
  labs(
    title = "Tendencia del Índice HOMA según Estado Nutricional",
    subtitle = "Prueba de Jonckheere-Terpstra",
    x = "Estado Nutricional",
    y = "Índice HOMA"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================