# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 16: ANOVA con bloques no paramétrico
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(sanon)

# ==============================================================================
# 1. DATOS
# ==============================================================================
# Contexto: Se desea determinar si el dolor evaluado a las 8 horas varía en 
# función del tipo de analgesia utilizado, nulificando el efecto que tiene 
# la realización de cesárea + Pomeroy

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

# EVA a las 4 horas
eva_4h <- c(3, 3, 3, 2, 5, 1, 1, 4, 3, 4, 5, 1, 4, 3, 4, 4, 4, 2, 10, 4, 
            8, 4, 7, 5, 5, 5, 9, 3, 7, 9, 6, 3, 8, 7, 3, 7, 10, 8, 8, 5)

# Dolor a las 4 horas (0 = No, 1 = Sí)
dolor_4h <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
              1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1)

# EVA a las 6 horas
eva_6h <- c(3, 3, 2, 2, 2, 1, 2, 5, 5, 5, 5, 5, 6, 3, 5, 4, 2, 2, 8, 2, 
            8, 4, 8, 4, 7, 5, 9, 5, 6, 9, 8, 3, 9, 9, 3, 7, 8, 8, 5, 5)

# Dolor a las 6 horas (0 = No, 1 = Sí)
dolor_6h <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 
              1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1)

# EVA a las 8 horas
eva_8h <- c(3, 3, 3, 7, 1, 1, 4, 5, 5, 5, 7, 2, 6, 2, 5, 4, 1, 1, 8, 1, 
            8, 4, 8, 3, 9, 7, 9, 5, 4, 8, 8, 3, 10, 9, 4, 8, 8, 8, 3, 4)

# Dolor a las 8 horas (0 = No, 1 = Sí)
dolor_8h <- c(0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 
              1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0)

# EVA a las 12 horas
eva_12h <- c(3, 5, 2, 5, 1, 4, 5, 7, 5, 5, 8, 6, 8, 2, 6, 9, 3, 2, 9, 1, 
             8, 4, 9, 2, 9, 5, 9, 5, 2, 5, 7, 5, 7, 9, 5, 8, 7, 6, 2, 6)

# Dolor a las 12 horas (0 = No, 1 = Sí)
dolor_12h <- c(0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 
               1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)

# ------------------------------------------------------------------------------
# Crear tibble
# ------------------------------------------------------------------------------

data_cesarea <- tibble(
  id_paciente = 1:40,
  modalidad_analgesica = modalidad_analgesica,
  tipo_cesarea = tipo_cesarea,
  imc = imc,
  eva_4h = eva_4h,
  dolor_4h = dolor_4h,
  eva_6h = eva_6h,
  dolor_6h = dolor_6h,
  eva_8h = eva_8h,
  dolor_8h = dolor_8h,
  eva_12h = eva_12h,
  dolor_12h = dolor_12h
) %>%
  mutate(
    modalidad_analgesica = fct_recode(factor(modalidad_analgesica),
                                      "Bloqueo regional" = "0",
                                      "Analgesia convencional" = "1"),
    tipo_cesarea = fct_recode(factor(tipo_cesarea),
                              "Cesárea sola" = "0",
                              "Cesárea + Pomeroy" = "1"),
    dolor_4h = fct_recode(factor(dolor_4h), "No" = "0", "Sí" = "1"),
    dolor_6h = fct_recode(factor(dolor_6h), "No" = "0", "Sí" = "1"),
    dolor_8h = fct_recode(factor(dolor_8h), "No" = "0", "Sí" = "1"),
    dolor_12h = fct_recode(factor(dolor_12h), "No" = "0", "Sí" = "1")
  )

# Verificar estructura
glimpse(data_cesarea)

# ==============================================================================
# 2. ANÁLISIS EXPLORATORIO
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Estadísticos descriptivos por modalidad analgésica
# ------------------------------------------------------------------------------
data_cesarea %>%
  group_by(modalidad_analgesica) %>%
  summarise(
    n = n(),
    mediana = median(eva_8h),
    Q1 = quantile(eva_8h, 0.25),
    Q3 = quantile(eva_8h, 0.75),
    min = min(eva_8h),
    max = max(eva_8h)
  )

# ------------------------------------------------------------------------------
# 2.2 Estadísticos descriptivos por modalidad analgésica y tipo de cesárea
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

# Boxplot de EVA a las 8 horas por modalidad analgésica, estratificado por tipo de cesárea
data_cesarea %>%
  ggplot(aes(x = modalidad_analgesica, y = eva_8h, fill = modalidad_analgesica)) + 
  geom_boxplot(alpha = 0.7) + 
  facet_wrap(~ tipo_cesarea) + 
  labs(
    title = "EVA a las 8 horas según modalidad analgésica",
    subtitle = "Estratificado por tipo de cesárea",
    x = "Modalidad analgésica", 
    y = "EVA (8 horas)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ==============================================================================
# 4. ANOVA CON BLOQUES NO PARAMÉTRICO (Método de Stratified Mann-Whitney)
# ==============================================================================
# El paquete sanon implementa el método de estratificación para comparar grupos
# controlando por una variable de bloqueo (estrato)
#
# grp(): Define el factor de tratamiento (variable de interés)
# strt(): Define el factor de estratificación (bloque)

modelo_bloques <- sanon(eva_8h ~ grp(modalidad_analgesica) + strt(tipo_cesarea), 
                        data = data_cesarea)

summary(modelo_bloques)

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================