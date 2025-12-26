# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 18: ANOVA factorial de medidas repetidas no paramétrico
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(forcats)
library(ggplot2)
library(tidyr)
library(rstatix)
library(ggstatsplot)
library(nparLD)

# ==============================================================================
# 1. DATOS
# ==============================================================================
# Contexto: Estudio de dolor postoperatorio (EVA) en pacientes sometidos a 
# artroscopia, evaluando el efecto del tiempo, tipo de analgesia, tipo de 
# artroscopia y sexo.

EVA_postoperatorio_inmediato <- c(4, 5, 3, 4, 5, 4, 4, 4, 5, 4, 3, 3, 4, 3, 4, 3, 3, 3, 4, 3, 3, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 4, 4, 4, 3, 4, 4, 5, 5, 3, 5, 5, 4, 5, 5, 5, 5, 4, 5, 4, 4, 3, 3, 3, 4, 4, 5, 4, 4, 4, 5, 4, 3, 5, 5, 4, 4, 5, 5, 5, 4, 4, 5, 4, 3, 4, 4, 4, 3, 4, 4, 3, 3, 4)
EVA_6_horas <- c(2, 2, 1, 2, 3, 3, 3, 1, 3, 1, 1, 2, 2, 3, 3, 1, 3, 2, 2, 1, 2, 1, 1, 2, 3, 2, 4, 2, 2, 3, 2, 2, 2, 2, 1, 3, 3, 3, 1, 2, 2, 3, 2, 2, 3, 3, 2, 3, 3, 3, 1, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 3, 3, 2, 3, 1, 3, 2, 1, 2, 3, 3, 3, 2, 3, 2, 3, 1, 3, 2, 3, 2, 2, 3, 2, 1, 3)
EVA_12_horas <- c(2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 3, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 2, 1, 1, 1, 1, 1)
tipo_de_artroscopia <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
sexo <- c("Femenino", "Masculino", "Masculino", "Masculino", "Masculino", "Masculino", "Femenino", "Femenino", "Masculino", "Femenino", "Femenino", "Femenino", "Masculino", "Masculino", "Femenino", "Femenino", "Femenino", "Femenino", "Masculino", "Masculino", "Masculino", "Femenino", "Femenino", "Femenino", "Femenino", "Femenino", "Masculino", "Femenino", "Femenino", "Masculino", "Masculino", "Femenino", "Masculino", "Femenino", "Masculino", "Femenino", "Masculino", "Masculino", "Masculino", "Femenino", "Femenino", "Masculino", "Femenino", "Femenino", "Femenino", "Femenino", "Femenino", "Masculino", "Masculino", "Femenino", "Femenino", "Masculino", "Masculino", "Femenino", "Femenino", "Femenino", "Masculino", "Femenino", "Masculino", "Masculino", "Masculino", "Femenino", "Masculino", "Masculino", "Masculino", "Femenino", "Masculino", "Femenino", "Masculino", "Femenino", "Femenino", "Femenino", "Femenino", "Masculino", "Femenino", "Masculino", "Masculino", "Masculino", "Femenino", "Femenino", "Femenino", "Masculino", "Masculino", "Masculino", "Masculino", "Femenino", "Masculino", "Femenino")
analgesia_utilizada <- c(rep(0, 44), rep(1, 44))

# Crear tibble en formato ancho
data_artroscopia <- tibble(
  id_paciente = 1:88,
  EVA_postoperatorio = EVA_postoperatorio_inmediato,
  EVA_6h = EVA_6_horas,
  EVA_12h = EVA_12_horas,
  tipo_artroscopia = tipo_de_artroscopia,
  sexo = sexo, 
  analgesia = analgesia_utilizada
) %>%
  mutate(
    tipo_artroscopia = factor(tipo_artroscopia, 
                              levels = c(0, 1),
                              labels = c("Abierta", "Cerrada")),
    sexo = factor(sexo),
    analgesia = factor(analgesia, 
                       levels = c(0, 1),
                       labels = c("Bupivacaina", "Morfina"))
  )

# Verificar estructura
glimpse(data_artroscopia)

# ==============================================================================
# 2. PREPARACIÓN DE DATOS PARA ANÁLISIS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Transformar a formato largo (tidy) con pivot_longer
# ------------------------------------------------------------------------------
# Para el análisis de medidas repetidas necesitamos formato largo donde cada
# fila representa una observación (paciente x tiempo)
# NOTA: Las etiquetas de tiempo NO deben tener espacios para compatibilidad con nparLD

data_largo <- data_artroscopia %>%
  pivot_longer(
    cols = c(EVA_postoperatorio, EVA_6h, EVA_12h),
    names_to = "tiempo",
    values_to = "EVA"
  ) %>%
  mutate(
    tiempo = factor(tiempo,
                    levels = c("EVA_postoperatorio", "EVA_6h", "EVA_12h"),
                    labels = c("Inmediato", "6h", "12h"))
  )

# Verificar estructura
glimpse(data_largo)
head(data_largo, 10)

# Verificar número de observaciones: 88 pacientes x 3 tiempos = 264
nrow(data_largo)

# ==============================================================================
# 3. ANÁLISIS EXPLORATORIO
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Estadísticos descriptivos por tiempo
# ------------------------------------------------------------------------------
data_largo %>%
  group_by(tiempo) %>%
  summarise(
    n = n(),
    mediana = median(EVA),
    Q1 = quantile(EVA, 0.25),
    Q3 = quantile(EVA, 0.75),
    min = min(EVA),
    max = max(EVA)
  )

# ------------------------------------------------------------------------------
# 3.2 Estadísticos descriptivos por tiempo y analgesia
# ------------------------------------------------------------------------------
data_largo %>%
  group_by(tiempo, analgesia) %>%
  summarise(
    n = n(),
    mediana = median(EVA),
    Q1 = quantile(EVA, 0.25),
    Q3 = quantile(EVA, 0.75),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 3.3 Estadísticos descriptivos por tiempo y tipo de artroscopia
# ------------------------------------------------------------------------------
data_largo %>%
  group_by(tiempo, tipo_artroscopia) %>%
  summarise(
    n = n(),
    mediana = median(EVA),
    Q1 = quantile(EVA, 0.25),
    Q3 = quantile(EVA, 0.75),
    .groups = "drop"
  )

# ==============================================================================
# 4. VISUALIZACIÓN DE DATOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Boxplot: EVA por tiempo
# ------------------------------------------------------------------------------
ggplot(data_largo, aes(x = tiempo, y = EVA, fill = tiempo)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Evolución del dolor postoperatorio",
    x = "Tiempo postoperatorio",
    y = "Escala Visual Análoga (EVA)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# 4.2 Boxplot: EVA por tiempo según tipo de analgesia
# ------------------------------------------------------------------------------
ggplot(data_largo, aes(x = tiempo, y = EVA, fill = tiempo)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ analgesia) +
  labs(
    title = "Evolución del dolor según tipo de analgesia",
    x = "Tiempo postoperatorio",
    y = "Escala Visual Análoga (EVA)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# 4.3 Boxplot: EVA por tiempo según tipo de artroscopia
# ------------------------------------------------------------------------------
ggplot(data_largo, aes(x = tiempo, y = EVA, fill = tiempo)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ tipo_artroscopia) +
  labs(
    title = "Evolución del dolor según tipo de artroscopia",
    x = "Tiempo postoperatorio",
    y = "Escala Visual Análoga (EVA)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# 4.4 Boxplot: EVA por tiempo según sexo
# ------------------------------------------------------------------------------
ggplot(data_largo, aes(x = tiempo, y = EVA, fill = tiempo)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ sexo) +
  labs(
    title = "Evolución del dolor según sexo",
    x = "Tiempo postoperatorio",
    y = "Escala Visual Análoga (EVA)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ==============================================================================
# 5. VISUALIZACIÓN CON GGSTATSPLOT
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Análisis por tiempo agrupado por analgesia
# ------------------------------------------------------------------------------
data_largo %>%
  grouped_ggwithinstats(
    x = tiempo,
    y = EVA,
    xlab = "Tiempo postoperatorio",
    ylab = "Escala Visual Análoga (EVA)",
    type = "np",
    grouping.var = analgesia,
    pairwise.display = "significant"
  )

# ------------------------------------------------------------------------------
# 5.2 Análisis por tiempo agrupado por tipo de artroscopia
# ------------------------------------------------------------------------------
data_largo %>%
  grouped_ggwithinstats(
    x = tiempo,
    y = EVA,
    xlab = "Tiempo postoperatorio",
    ylab = "Escala Visual Análoga (EVA)",
    type = "np",
    grouping.var = tipo_artroscopia,
    pairwise.display = "significant"
  )

# ------------------------------------------------------------------------------
# 5.3 Análisis por tiempo agrupado por sexo
# ------------------------------------------------------------------------------
data_largo %>%
  grouped_ggwithinstats(
    x = tiempo,
    y = EVA,
    xlab = "Tiempo postoperatorio",
    ylab = "Escala Visual Análoga (EVA)",
    type = "np",
    grouping.var = sexo,
    pairwise.display = "significant"
  )

# ==============================================================================
# 6. ANOVA NO PARAMÉTRICO DE MEDIDAS REPETIDAS CON nparLD
# ==============================================================================
# El paquete nparLD implementa métodos no paramétricos para diseños longitudinales
# basados en rangos y efectos relativos de tratamiento.
#
# Ventajas:
# - No asume normalidad
# - No asume homocedasticidad
# - Robusto ante valores atípicos
# - Adecuado para variables ordinales (como EVA)

# ------------------------------------------------------------------------------
# 6.1 Modelo de medidas repetidas simple (solo efecto del tiempo)
# ------------------------------------------------------------------------------
# Diseño LD-F1: Un factor intra-sujeto (tiempo)

modelo_tiempo <- nparLD(
  EVA ~ tiempo, 
  data = data_largo,
  subject = data_largo$id_paciente,
  description = FALSE
)

# Resumen del modelo
summary(modelo_tiempo)

# Gráfico de efectos relativos de tratamiento
plot(modelo_tiempo, main = "Efecto del tiempo sobre EVA")

# ------------------------------------------------------------------------------
# 6.2 Modelo factorial: Tiempo x Analgesia
# ------------------------------------------------------------------------------
# Diseño F1-LD-F1: Un factor entre-sujetos (analgesia) x Un factor intra-sujeto (tiempo)

modelo_tiempo_analgesia <- nparLD(
  EVA ~ tiempo * analgesia,
  data = data_largo,
  subject = data_largo$id_paciente,
  description = FALSE
)

# Resumen del modelo
summary(modelo_tiempo_analgesia)

# Gráfico de efectos
plot(modelo_tiempo_analgesia, main = "Efecto Tiempo x Analgesia")

# ------------------------------------------------------------------------------
# 6.3 Modelo factorial: Tiempo x Tipo de artroscopia
# ------------------------------------------------------------------------------

modelo_tiempo_artroscopia <- nparLD(
  EVA ~ tiempo * tipo_artroscopia,
  data = data_largo,
  subject = data_largo$id_paciente,
  description = FALSE
)

# Resumen del modelo
summary(modelo_tiempo_artroscopia)

# Gráfico de efectos
plot(modelo_tiempo_artroscopia, main = "Efecto Tiempo x Tipo de artroscopia")

# ------------------------------------------------------------------------------
# 6.4 Modelo factorial: Tiempo x Sexo
# ------------------------------------------------------------------------------

modelo_tiempo_sexo <- nparLD(
  EVA ~ tiempo * sexo,
  data = data_largo,
  subject = data_largo$id_paciente,
  description = FALSE
)

# Resumen del modelo
summary(modelo_tiempo_sexo)

# Gráfico de efectos
plot(modelo_tiempo_sexo, main = "Efecto Tiempo x Sexo")

# ==============================================================================
# 7. INTERPRETACIÓN DE RESULTADOS
# ==============================================================================
# 
# La función nparLD proporciona:
#
# 1. ANOVA-Type Statistic (ATS): 
#    - Estadístico robusto para muestras pequeñas
#    - Se reporta con grados de libertad ajustados
#
# 2. Relative Treatment Effects (RTE):
#    - Valores entre 0 y 1
#    - RTE = 0.5 indica que el grupo está en la mediana global
#    - RTE > 0.5 indica valores mayores que la mediana global
#    - RTE < 0.5 indica valores menores que la mediana global
#
# Interpretación del ejemplo:
# - Si el efecto del tiempo es significativo: el dolor cambia a lo largo del tiempo
# - Si la interacción Tiempo x Analgesia es significativa: el cambio del dolor
#   a lo largo del tiempo difiere según el tipo de analgesia utilizada
#
# ==============================================================================

# ==============================================================================
# 8. COMPARACIONES POST-HOC (si es necesario)
# ==============================================================================
# Para comparaciones múltiples después de encontrar efectos significativos,
# se pueden usar pruebas de Wilcoxon pareadas con corrección de Bonferroni

# Ejemplo: comparaciones por tiempo
comparaciones_tiempo <- data_largo %>%
  pairwise_wilcox_test(
    EVA ~ tiempo,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

print(comparaciones_tiempo)

# Ejemplo: comparaciones por tiempo dentro de cada grupo de analgesia
comparaciones_por_analgesia <- data_largo %>%
  group_by(analgesia) %>%
  pairwise_wilcox_test(
    EVA ~ tiempo,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

print(comparaciones_por_analgesia)

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
