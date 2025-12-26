# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 18: ANOVA factorial no paramétrico
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(ARTool)
library(Rfit)

# ==============================================================================
# 1. DATOS
# ==============================================================================

# ------------------------------------------------------------------------------
# Vectores
# ------------------------------------------------------------------------------

# Modalidad analgésica (0 = Bloqueo regional, 1 = Analgesia convencional)
modalidad_analgesica <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 
                          1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1)

# Tipo de cesárea (0 = Cesárea sola, 1 = Cesárea + Pomeroy)
tipo_cesarea <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 
                  1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# Índice de masa corporal (IMC)
imc <- c(23.9, 24.8, 25.0, 25.2, 25.7, 25.8, 26.8, 28.8, 28.9, 29.1, 
         29.3, 29.4, 30.9, 31.1, 32.0, 32.0, 21.8, 23.3, 24.3, 24.8, 
         25.0, 26.4, 26.9, 27.0, 27.9, 29.3, 30.0, 30.5, 30.9, 33.3, 
         34.2, 37.1, 37.3, 38.1, 25.6, 29.1, 29.9, 32.4, 34.9, 27.1)

# EVA a las 8 horas
eva_8h <- c(3, 3, 3, 7, 1, 1, 4, 5, 5, 5, 7, 2, 6, 2, 5, 4, 1, 1, 8, 1, 
            8, 4, 8, 3, 9, 7, 9, 5, 4, 8, 8, 3, 10, 9, 4, 8, 8, 8, 3, 4)

# ------------------------------------------------------------------------------
# Crear tibble
# ------------------------------------------------------------------------------

data_cesarea <- tibble(
  id_paciente = 1:40,
  modalidad_analgesica = modalidad_analgesica,
  tipo_cesarea = tipo_cesarea,
  imc = imc,
  eva_8h = eva_8h
) %>%
  mutate(
    modalidad_analgesica = fct_recode(factor(modalidad_analgesica),
                                      "Bloqueo regional" = "0",
                                      "Analgesia convencional" = "1"),
    tipo_cesarea = fct_recode(factor(tipo_cesarea),
                              "Cesárea sola" = "0",
                              "Cesárea + Pomeroy" = "1")
  )

# Verificar estructura
glimpse(data_cesarea)

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Estadísticos descriptivos por modalidad analgésica y tipo de cesárea
# ------------------------------------------------------------------------------
data_cesarea %>%
  group_by(modalidad_analgesica, tipo_cesarea) %>%
  summarise(
    n = n(),
    mediana = median(eva_8h),
    Q1 = quantile(eva_8h, 0.25),
    Q3 = quantile(eva_8h, 0.75),
    .groups = "drop"
  )

# ==============================================================================
# 3. VISUALIZACIÓN
# ==============================================================================

# Boxplot de EVA a las 8 horas
data_cesarea %>%
  ggplot(aes(x = modalidad_analgesica, y = eva_8h, fill = tipo_cesarea)) + 
  geom_boxplot(alpha = 0.7) + 
  labs(
    title = "EVA a las 8 horas según modalidad analgésica y tipo de cesárea",
    x = "Modalidad analgésica", 
    y = "EVA (8 horas)",
    fill = "Tipo de cesárea"
  ) +
  theme_minimal()

# ==============================================================================
# 4. MODELO DE TRANSFORMACIÓN DE RANGOS ALINEADOS
# ==============================================================================
# ARTool (Aligned Rank Transform) permite analizar diseños factoriales
# no paramétricos evaluando efectos principales e interacciones.

# ------------------------------------------------------------------------------
# 4.1 Modelo de Transformación de Rangos Alineados
# ------------------------------------------------------------------------------

modelo_art_con_int <- art(eva_8h ~ modalidad_analgesica * tipo_cesarea, 
                          data = data_cesarea)

# Tabla ANOVA
anova(modelo_art_con_int)

# ==============================================================================
# 5. MODELO DE REGRESIÓN BASADA EN RANGOS (Rfit)
# ==============================================================================
# Rfit permite incluir covariables numéricas sin necesidad de categorizarlas.

# ------------------------------------------------------------------------------
# 5.1 Modelo de Rfit con IMC (sin interacción entre factores)
# ------------------------------------------------------------------------------

modelo_rfit_imc <- rfit(eva_8h ~ modalidad_analgesica + tipo_cesarea + imc, 
                        data = data_cesarea)

summary(modelo_rfit_imc)

# ------------------------------------------------------------------------------
# 5.2 Modelo Rfit con IMC e interacción entre factores categóricos
# ------------------------------------------------------------------------------

modelo_rfit_imc_int <- rfit(eva_8h ~ modalidad_analgesica * tipo_cesarea + imc, 
                            data = data_cesarea)

summary(modelo_rfit_imc_int)

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================