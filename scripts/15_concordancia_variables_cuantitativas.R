# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 15: Concordancia: Variables numéricas
# ==============================================================================

# Cargar librerías necesarias
library(DescTools)
library(ggplot2)
library(mcr)  # Para Passing-Bablok

# ==============================================================================
# 1. DATOS: Estimación de Grasa Corporal
# ==============================================================================
# Comparación de dos métodos de medición:
# - Método 1: Pliegues cutáneos (%)
# - Método 2: Bioimpedancia (%)
# n = 134 sujetos

grasa_pliegues <- c(
  21,13,21,8,8,25,16,16,25,12,16,16,21,16,16,16,25,21,12,21,
  21,12,35,25,22,33,16,13,21,21,30,15,12,16,16,21,16,16,16,21,
  17,21,12,35,22,25,21,16,16,21,24,27,25,30,25,40,25,12,34,21,
  25,21,25,12,21,21,30,21,22,21,26,30,16,29,25,12,21,35,14,25,
  35,21,30,17,25,24,25,16,21,21,35,40,30,28,21,25,21,21,30,21,
  25,35,25,33,21,30,25,30,30,35,25,25,25,40,25,35,38,35,40,35,
  21,25,35,25,40,40,40,26,26,21,40,40,42,21
)

grasa_bioimpedancia <- c(
  19.6,5.2,4.1,6.5,8.9,17.7,14.8,10.9,14.8,8.7,14.5,10.4,11.9,11.1,12.1,13.7,18.8,14.2,13.8,20.7,
  13.4,29.3,21.3,17.4,15,19.9,14.6,17.1,17.1,13.8,34.3,14.9,12.6,15.7,19.9,16.5,16.2,14.6,15.6,16.2,
  15.9,8.2,14.8,27.6,16.1,23,17.1,18.8,15.8,13.9,16.5,26.1,28.3,24.8,19.9,27.2,16.9,18,34.4,15.8,
  22.2,20.6,19.8,16.9,20.2,20.5,18.9,21.2,16.8,20.5,20.5,27.5,21.2,27.3,21.2,14.5,18.4,27.1,22.7,25.1,
  30.3,20.5,25.3,19.8,25.1,18.6,26.9,19.1,30.8,21.3,30.7,33.6,32.5,23.7,24,24.4,22,22.4,26.2,25.5,
  25.2,29.4,28.3,25.8,21.7,26.7,27,25.6,23,38.9,26.2,23.2,27.9,36.4,24.4,33.6,36.5,28.7,34.6,40.7,
  27.8,24.6,36.6,24,36.4,41.5,43.9,31.6,28,41.4,44.1,40.8,43.7,20.9
)

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

# Estadísticas descriptivas
data.frame(
  Método = c("Pliegues cutáneos", "Bioimpedancia"),
  n = c(length(grasa_pliegues), length(grasa_bioimpedancia)),
  Media = round(c(mean(grasa_pliegues), mean(grasa_bioimpedancia)), 2),
  DE = round(c(sd(grasa_pliegues), sd(grasa_bioimpedancia)), 2),
  Mediana = round(c(median(grasa_pliegues), median(grasa_bioimpedancia)), 2),
  Mínimo = round(c(min(grasa_pliegues), min(grasa_bioimpedancia)), 2),
  Máximo = round(c(max(grasa_pliegues), max(grasa_bioimpedancia)), 2)
)

# ==============================================================================
# 3. REGRESIÓN DE PASSING-BABLOK
# ==============================================================================
# Método no paramétrico para comparar dos métodos de medición.
# Estima pendiente e intercepto usando medianas de todas las pendientes
# posibles entre pares de puntos.
# 
# Interpretación:
# - Intercepto: Si el IC 95% incluye 0, no hay sesgo sistemático constante
# - Pendiente: Si el IC 95% incluye 1, no hay sesgo proporcional
# - Si ambos se cumplen: los métodos son intercambiables

# Ajustar regresión de Passing-Bablok
pb_reg <- mcreg(grasa_pliegues, grasa_bioimpedancia, 
                method.reg = "PaBa",  # Passing-Bablok
                method.ci = "analytical")

# Resumen del modelo
printSummary(pb_reg)

# ------------------------------------------------------------------------------
# 3.1 Gráfico de Passing-Bablok
# ------------------------------------------------------------------------------
plot(pb_reg,
     main = "Regresión de Passing-Bablok\nPliegues cutáneos vs Bioimpedancia",
     x.lab = "Pliegues cutáneos (%)",
     y.lab = "Bioimpedancia (%)")

# ------------------------------------------------------------------------------
# 3.2 Gráfico con ggplot2 (versión mejorada)
# ------------------------------------------------------------------------------

# Extraer coeficientes
coefs <- getCoefficients(pb_reg)
intercepto <- coefs[1, 1]
pendiente <- coefs[2, 1]

# Crear dataframe
datos_grasa <- data.frame(
  pliegues = grasa_pliegues,
  bioimpedancia = grasa_bioimpedancia
)

ggplot(datos_grasa, aes(x = pliegues, y = bioimpedancia)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "gray50") +  # Línea de identidad
  geom_abline(intercept = intercepto, slope = pendiente, 
              color = "#E74C3C", linewidth = 1) +  # Línea Passing-Bablok
  labs(
    title = "Regresión de Passing-Bablok",
    subtitle = paste0("Intercepto = ", round(intercepto, 2), 
                      " | Pendiente = ", round(pendiente, 2)),
    x = "Pliegues cutáneos (%)",
    y = "Bioimpedancia (%)"
  ) +
  theme_minimal() +
  annotate("text", x = 10, y = 40, 
           label = "— Passing-Bablok\n--- Línea de identidad",
           hjust = 0, size = 3)

# ==============================================================================
# 4. BLAND-ALTMAN CON PERCENTILES (No paramétrico)
# ==============================================================================
# En lugar de usar ±1.96 DE (que asume normalidad de las diferencias),
# se usan los percentiles 2.5 y 97.5 para los límites de acuerdo.

# Calcular diferencias y promedios
diferencias <- grasa_pliegues - grasa_bioimpedancia
promedios <- (grasa_pliegues + grasa_bioimpedancia) / 2

# Estadísticos no paramétricos
mediana_dif <- median(diferencias)
lim_sup <- quantile(diferencias, 0.975)
lim_inf <- quantile(diferencias, 0.025)

# Gráfico Bland-Altman con percentiles
datos_ba <- data.frame(
  promedio = promedios,
  diferencia = diferencias
)

ggplot(datos_ba, aes(x = promedio, y = diferencia)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = mediana_dif, color = "#2980B9", linewidth = 1) +
  geom_hline(yintercept = lim_sup, color = "#E74C3C", 
             linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = lim_inf, color = "#E74C3C", 
             linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Bland-Altman con límites no paramétricos",
    subtitle = paste0("Mediana = ", round(mediana_dif, 2),
                      " | P2.5 = ", round(lim_inf, 2),
                      " | P97.5 = ", round(lim_sup, 2)),
    x = "Promedio de ambos métodos (%)",
    y = "Diferencia (Pliegues - Bioimpedancia)"
  ) +
  theme_minimal()

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================