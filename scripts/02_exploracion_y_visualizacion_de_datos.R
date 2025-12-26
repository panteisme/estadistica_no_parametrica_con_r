# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 2: Exploración y visualización de datos
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)

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
# 2. VISUALIZACIÓN DE DATOS CON GGPLOT2
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 EDAD: Histograma con densidad
# ------------------------------------------------------------------------------

ggplot(data, aes(x = edad)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 5, 
                 fill = "#3498db", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#e74c3c", 
               linewidth = 1.2) +
  labs(
    title = "Distribución de la Edad",
    subtitle = "Histograma con función de densidad",
    x = "Edad (años)",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 2.2 EDAD: Boxplot + Violin + Puntos + Media/Mediana
# ------------------------------------------------------------------------------

media_edad <- mean(data$edad)
mediana_edad <- median(data$edad)

ggplot(data, aes(x = "", y = edad)) +
  geom_violin(fill = "#3498db", alpha = 0.3, color = "#2c3e50") +
  geom_boxplot(width = 0.2, fill = "#3498db", alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#34495e", size = 2) +
  geom_hline(yintercept = media_edad, 
             color = "#e74c3c", 
             linetype = "dashed", 
             linewidth = 1) +
  geom_hline(yintercept = mediana_edad, 
             color = "#27ae60", 
             linetype = "dashed", 
             linewidth = 1) +
  annotate("text", x = 1.35, y = media_edad, 
           label = paste("Media =", round(media_edad, 2)), 
           color = "#e74c3c", fontface = "bold") +
  annotate("text", x = 1.35, y = mediana_edad + 0.3, 
           label = paste("Mediana =", round(mediana_edad, 2)), 
           color = "#27ae60", fontface = "bold") +
  labs(
    title = "Distribución de la Edad",
    subtitle = "Boxplot con violin, puntos, media y mediana",
    x = "",
    y = "Edad (años)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 2.3 EDAD: QQ Plot
# ------------------------------------------------------------------------------

ggplot(data, aes(sample = edad)) +
  stat_qq(color = "#3498db", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Edad",
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
# 2.4 IMC: Histograma con densidad
# ------------------------------------------------------------------------------

ggplot(data, aes(x = imc)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#9b59b6", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#e74c3c", 
               linewidth = 1.2) +
  labs(
    title = "Distribución del Índice de Masa Corporal",
    subtitle = "Histograma con función de densidad",
    x = "IMC (kg/m²)",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 2.5 IMC: Boxplot + Violin + Puntos + Media/Mediana
# ------------------------------------------------------------------------------

media_imc <- mean(data$imc)
mediana_imc <- median(data$imc)

ggplot(data, aes(x = "", y = imc)) +
  geom_violin(fill = "#9b59b6", alpha = 0.3, color = "#2c3e50") +
  geom_boxplot(width = 0.2, fill = "#9b59b6", alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#34495e", size = 2) +
  geom_hline(yintercept = media_imc, 
             color = "#e74c3c", 
             linetype = "dashed", 
             linewidth = 1) +
  geom_hline(yintercept = mediana_imc, 
             color = "#27ae60", 
             linetype = "dashed", 
             linewidth = 1) +
  annotate("text", x = 1.35, y = media_imc, 
           label = paste("Media =", round(media_imc, 2)), 
           color = "#e74c3c", fontface = "bold") +
  annotate("text", x = 1.35, y = mediana_imc + 0.5, 
           label = paste("Mediana =", round(mediana_imc, 2)), 
           color = "#27ae60", fontface = "bold") +
  labs(
    title = "Distribución del IMC",
    subtitle = "Boxplot con violin, puntos, media y mediana",
    x = "",
    y = "IMC (kg/m²)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 2.6 IMC: QQ Plot
# ------------------------------------------------------------------------------

ggplot(data, aes(sample = imc)) +
  stat_qq(color = "#9b59b6", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - IMC",
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
# 2.7 ÍNDICE HOMA: Histograma con densidad
# ------------------------------------------------------------------------------

ggplot(data, aes(x = indice_homa)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#f39c12", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#e74c3c", 
               linewidth = 1.2) +
  labs(
    title = "Distribución del Índice HOMA",
    subtitle = "Histograma con función de densidad",
    x = "Índice HOMA",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 2.8 ÍNDICE HOMA: Boxplot + Violin + Puntos + Media/Mediana
# ------------------------------------------------------------------------------

media_homa <- mean(data$indice_homa)
mediana_homa <- median(data$indice_homa)

ggplot(data, aes(x = "", y = indice_homa)) +
  geom_violin(fill = "#f39c12", alpha = 0.3, color = "#2c3e50") +
  geom_boxplot(width = 0.2, fill = "#f39c12", alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#34495e", size = 2) +
  geom_hline(yintercept = media_homa, 
             color = "#e74c3c", 
             linetype = "dashed", 
             linewidth = 1) +
  geom_hline(yintercept = mediana_homa, 
             color = "#27ae60", 
             linetype = "dashed", 
             linewidth = 1) +
  annotate("text", x = 1.35, y = media_homa, 
           label = paste("Media =", round(media_homa, 2)), 
           color = "#e74c3c", fontface = "bold") +
  annotate("text", x = 1.35, y = mediana_homa + 0.2, 
           label = paste("Mediana =", round(mediana_homa, 2)), 
           color = "#27ae60", fontface = "bold") +
  labs(
    title = "Distribución del Índice HOMA",
    subtitle = "Boxplot con violin, puntos, media y mediana",
    x = "",
    y = "Índice HOMA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 2.9 ÍNDICE HOMA: QQ Plot
# ------------------------------------------------------------------------------

ggplot(data, aes(sample = indice_homa)) +
  stat_qq(color = "#f39c12", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Índice HOMA",
    subtitle = "Evaluación de normalidad",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ==============================================================================
# 3. RESUMEN ESTADÍSTICO CON MODELSUMMARY
# ==============================================================================

# Paquete model summary
library(modelsummary)

# Resumen general de todas las variables
datasummary_skim(data)

# Solo las variables categóricas
datasummary_skim(data, type = "categorical")

# Solo las variables numéricas
datasummary_skim(data, type = "numeric")

# ==============================================================================
# 4. ANÁLISIS BIVARIADO
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Análisis de Diferencia: Variable numérica según categórica
# Índice HOMA según Estado Nutricional
# ------------------------------------------------------------------------------

ggplot(data, aes(x = estado_nutricional, y = indice_homa, fill = estado_nutricional)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = c(
    "Normal" = "#27ae60",
    "Sobrepeso" = "#f39c12",
    "Obesidad 1" = "#e67e22",
    "Obesidad 2" = "#e74c3c"
  )) +
  labs(
    title = "Índice HOMA según Estado Nutricional",
    subtitle = "Análisis de diferencias entre grupos",
    x = "Estado Nutricional",
    y = "Índice HOMA",
    fill = "Estado Nutricional"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

# ------------------------------------------------------------------------------
# 4.2 Análisis de Asociación: Ambas variables categóricas
# Insulinorresistencia vs Estado Nutricional
# ------------------------------------------------------------------------------

# Crear tabla de frecuencias y porcentajes
tabla_asociacion <- data %>%
  count(estado_nutricional, insulinorresistencia) %>%
  group_by(estado_nutricional) %>%
  mutate(
    total = sum(n),
    porcentaje = n / total * 100,
    etiqueta = paste0(n, " (", round(porcentaje, 1), "%)")
  )

ggplot(tabla_asociacion, aes(x = estado_nutricional, y = porcentaje, fill = insulinorresistencia)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.5) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 3.5) +
  scale_fill_manual(
    values = c("Si" = "#3498db", "No" = "#e67e22"),
    name = "Insulinorresistencia"
  ) +
  labs(
    title = "Asociación entre Estado Nutricional e Insulinorresistencia",
    subtitle = "Distribución porcentual",
    x = "Estado Nutricional",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right"
  )

# ------------------------------------------------------------------------------
# 4.3 Análisis de Correlación: Ambas variables numéricas
# IMC e Índice HOMA
# ------------------------------------------------------------------------------

ggplot(data, aes(x = imc, y = indice_homa)) +
  geom_point(alpha = 0.6, size = 3, color = "#3498db") +
  geom_smooth(method = "lm", se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
  labs(
    title = "Relación entre IMC e Índice HOMA",
    subtitle = "Diagrama de dispersión con línea de tendencia",
    x = "IMC (kg/m²)",
    y = "Índice HOMA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
