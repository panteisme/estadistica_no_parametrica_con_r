# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 10: Tamaño del efecto para variables categóricas
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(DescTools)
library(psych)
library(scales)

# ==============================================================================
# 1. MEDIDAS DE ASOCIACIÓN PARA TABLAS 2x2
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Datos: Bajo peso prenatal vs Parto prematuro
# ------------------------------------------------------------------------------

bajo_peso <- matrix(c(57, 81, 54, 166),
                    nrow = 2, byrow = TRUE,
                    dimnames = list(
                      "Bajo_peso_prenatal" = c("Si", "No"),
                      "Parto_prematuro" = c("Si", "No")
                    ))

addmargins(bajo_peso)

# ------------------------------------------------------------------------------
# 1.2 Coeficiente Phi
# ------------------------------------------------------------------------------

# Con psych
phi(bajo_peso)

# Con psych
Phi(bajo_peso)

# ------------------------------------------------------------------------------
# 1.3 Coeficiente de Contingencia
# ------------------------------------------------------------------------------

# Con DescTools
ContCoef(bajo_peso)

# ==============================================================================
# 3. MEDIDAS DE ASOCIACIÓN PARA TABLAS rxc
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Datos: Estado nutricional vs Insulinorresistencia
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

# Crear tabla de contingencia
tabla_estado_nutricional <- table(data$estado_nutricional, data$insulinorresistencia)
addmargins(tabla_estado_nutricional)

# ------------------------------------------------------------------------------
# 3.2 V de Cramer
# ------------------------------------------------------------------------------

# Con DescTools
CramerV(tabla_estado_nutricional)

# Con rstatix (desde la tabla)
cramer_v(tabla_estado_nutricional)

# ------------------------------------------------------------------------------
# 3.3 Coeficiente de Contingencia
# ------------------------------------------------------------------------------

# Con DescTools
ContCoef(tabla_estado_nutricional)

# ==============================================================================
# 4. MEDIDAS DE ASOCIACIÓN PARA VARIABLES ORDINALES
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Datos: CD4 vs Carga Viral en pacientes con VIH
# ------------------------------------------------------------------------------

CD4_cat <- factor(rep(c("1 - 100", "101 - 200", "201 - 350", "351 - 500", "> 500"), each = 5),
                  levels = c("1 - 100", "101 - 200", "201 - 350", "351 - 500", "> 500"))

CargaViral_cat <- factor(rep(c("< 40", "40 - 1000", "1001 - 50,000", "50,001 - 100,000", "> 100,000"), times = 5),
                         levels = c("< 40", "40 - 1000", "1001 - 50,000", "50,001 - 100,000", "> 100,000"))

Frecuencia <- c(
  # CD4 1-100
  6, 12, 58, 46, 435,
  # CD4 101-200
  22, 9, 75, 60, 225,
  # CD4 201-350
  29, 18, 146, 79, 228,
  # CD4 351-500
  34, 19, 134, 59, 130,
  # CD4 >500
  84, 30, 118, 23, 62
)

tabla_vih <- xtabs(Frecuencia ~ CD4_cat + CargaViral_cat)
addmargins(tabla_vih)

# ------------------------------------------------------------------------------
# 4.2 Visualización
# ------------------------------------------------------------------------------

datos_vih <- as.data.frame(tabla_vih)
colnames(datos_vih) <- c("CD4", "CargaViral", "Frecuencia")

datos_prop <- datos_vih %>%
  group_by(CD4) %>%
  mutate(Proporcion = Frecuencia / sum(Frecuencia))

ggplot(datos_prop, aes(x = CD4, y = Proporcion, fill = CargaViral)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Recuento de linfocitos T-CD4 y carga viral en pacientes con VIH",
    x = "Categorías de CD4 (células/µL)",
    y = "Distribución porcentual de carga viral",
    fill = "Carga Viral (copias/mL)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# ------------------------------------------------------------------------------
# 4.3 Gamma de Goodman-Kruskal
# ------------------------------------------------------------------------------

# Con DescTools
GoodmanKruskalGamma(tabla_vih)

# ------------------------------------------------------------------------------
# 4.4 Tau-b de Kendall
# ------------------------------------------------------------------------------

# Con DescTools
KendallTauB(tabla_vih)

# ==============================================================================
# 5. INTERPRETACIÓN DE MEDIDAS DE ASOCIACIÓN
# ==============================================================================

# Phi y V de Cramer:
# - 0.00 - 0.10: Asociación muy débil
# - 0.10 - 0.30: Asociación débil
# - 0.30 - 0.50: Asociación moderada
# - 0.50 - 1.00: Asociación fuerte

# Gamma y Tau-b (variables ordinales):
# - -1.00 a -0.50: Asociación negativa fuerte
# - -0.50 a -0.30: Asociación negativa moderada
# - -0.30 a -0.10: Asociación negativa débil
# - -0.10 a 0.10: Asociación muy débil o nula
# - 0.10 a 0.30: Asociación positiva débil
# - 0.30 a 0.50: Asociación positiva moderada
# - 0.50 a 1.00: Asociación positiva fuerte

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================