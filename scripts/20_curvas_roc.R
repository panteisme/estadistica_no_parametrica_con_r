# ==============================================================================
# Curso de Estadística no paramétrica con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Tema 20: Curvas ROC
# ==============================================================================

# Cargar librerías necesarias
library(pROC)
library(ggplot2)
library(knitr)

# ==============================================================================
# 1. DATOS DEL ESTUDIO
# ==============================================================================
# Contexto: Estudio para evaluar la capacidad predictiva de la elastografía
# hepática y esplénica en la detección de varices gastroesofágicas (n = 78)

# Variable respuesta: Varices gastroesofágicas (1 = Sí, 0 = No)
varices <- c(0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 
             1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 
             1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 
             1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1)

# Predictor 1: Rigidez hepática (kPa)
rigidez_hepatica <- c(26.1, 55.0, 3.4, 14.3, 57.4, 64.9, 13.1, 14.0, 11.8, 30.9, 
                      8.4, 32.8, 13.0, 37.7, 20.9, 17.5, 71.1, 33.8, 14.3, 21.2, 
                      41.9, 62.8, 26.5, 23.3, 75.0, 73.5, 10.2, 11.7, 19.5, 12.1, 
                      26.2, 19.7, 10.9, 34.1, 30.9, 7.6, 8.5, 17.4, 36.9, 16.7, 
                      57.2, 34.8, 26.7, 21.8, 12.7, 64.0, 17.0, 7.4, 25.5, 14.3, 
                      36.7, 10.4, 43.3, 27.5, 5.0, 16.0, 16.6, 35.9, 39.5, 24.7, 
                      41.4, 14.9, 15.4, 14.7, 16.0, 8.2, 10.3, 14.2, 28.4, 34.2, 
                      20.1, 43.8, 41.4, 36.9, 55.4, 71.3, 23.3, 47.8)

# Predictor 2: Rigidez esplénica (kPa)
rigidez_esplenica <- c(36.3, 74.6, 20.0, 50.8, 23.7, 45.0, 26.3, 28.6, 38.7, 29.0, 
                       28.7, 57.9, 23.3, 24.7, 24.8, 25.6, 36.6, 22.7, 23.4, 69.0, 
                       37.8, 53.8, 73.6, 75.0, 30.7, 74.6, 29.1, 24.5, 26.6, 21.3, 
                       75.0, 23.7, 24.2, 28.8, 47.6, 27.1, 21.8, 26.0, 75.0, 65.6, 
                       73.3, 41.4, 42.0, 75.0, 24.8, 69.9, 58.0, 23.9, 29.6, 26.7, 
                       21.8, 75.0, 20.6, 29.1, 19.1, 22.5, 24.1, 32.1, 75.0, 21.5, 
                       75.0, 42.3, 35.2, 33.0, 67.3, 22.5, 69.0, 24.5, 24.4, 23.4, 
                       28.5, 36.2, 27.2, 45.3, 44.6, 74.6, 63.9, 47.8)

# ==============================================================================
# 2. CONSTRUCCIÓN DE CURVAS ROC
# ==============================================================================

# Construir curvas ROC
roc_hepatica <- roc(varices, rigidez_hepatica, direction = "<", quiet = TRUE)
roc_esplenica <- roc(varices, rigidez_esplenica, direction = "<", quiet = TRUE)

# ==============================================================================
# 3. ÁREA BAJO LA CURVA (AUC) CON INTERVALOS DE CONFIANZA
# ==============================================================================

# Calcular AUC con IC 95%
auc_hepatica <- auc(roc_hepatica)
ci_hepatica <- ci.auc(roc_hepatica, conf.level = 0.95)

auc_esplenica <- auc(roc_esplenica)
ci_esplenica <- ci.auc(roc_esplenica, conf.level = 0.95)

# Tabla de resultados AUC
resultados_auc <- data.frame(
  Predictor = c("Elastografía Hepática", "Elastografía Esplénica"),
  AUC = round(c(auc_hepatica, auc_esplenica), 4),
  IC_inferior = round(c(ci_hepatica[1], ci_esplenica[1]), 4),
  IC_superior = round(c(ci_hepatica[3], ci_esplenica[3]), 4)
)

kable(resultados_auc, 
      align = c("l", "c", "c", "c"),
      caption = "Tabla 1. Área bajo la curva ROC con intervalos de confianza del 95%")

# ==============================================================================
# 4. PUNTO ÓPTIMO DE CORTE (ÍNDICE DE YOUDEN)
# ==============================================================================

# Determinar punto de corte óptimo para cada predictor
coords_hepatica <- coords(roc_hepatica, x = "best", best.method = "youden",
                          ret = c("threshold", "sensitivity", "specificity",
                                  "ppv", "npv", "accuracy"))

coords_esplenica <- coords(roc_esplenica, x = "best", best.method = "youden",
                           ret = c("threshold", "sensitivity", "specificity",
                                   "ppv", "npv", "accuracy"))

# ==============================================================================
# 5. INTERVALOS DE CONFIANZA PARA MÉTRICAS DIAGNÓSTICAS
# ==============================================================================

# Función para calcular IC 95% (método exacto Clopper-Pearson)
calcular_ic <- function(roc_obj, punto_corte) {
  coords_punto <- coords(roc_obj, x = punto_corte, 
                         ret = c("sensitivity", "specificity", "tp", "tn", "fp", "fn"))
  
  tp <- coords_punto$tp
  fn <- coords_punto$fn
  tn <- coords_punto$tn
  fp <- coords_punto$fp
  
  # IC para sensibilidad
  n_enfermos <- tp + fn
  ic_sens <- binom.test(tp, n_enfermos, conf.level = 0.95)$conf.int
  
  # IC para especificidad
  n_sanos <- tn + fp
  ic_spec <- binom.test(tn, n_sanos, conf.level = 0.95)$conf.int
  
  # IC para VPP
  n_positivos <- tp + fp
  ic_vpp <- if(n_positivos > 0) binom.test(tp, n_positivos, conf.level = 0.95)$conf.int else c(NA, NA)
  
  # IC para VPN
  n_negativos <- tn + fn
  ic_vpn <- if(n_negativos > 0) binom.test(tn, n_negativos, conf.level = 0.95)$conf.int else c(NA, NA)
  
  return(list(
    sensibilidad = coords_punto$sensitivity,
    ic_sens_inf = ic_sens[1],
    ic_sens_sup = ic_sens[2],
    especificidad = coords_punto$specificity,
    ic_spec_inf = ic_spec[1],
    ic_spec_sup = ic_spec[2],
    vpp = tp / n_positivos,
    ic_vpp_inf = ic_vpp[1],
    ic_vpp_sup = ic_vpp[2],
    vpn = tn / n_negativos,
    ic_vpn_inf = ic_vpn[1],
    ic_vpn_sup = ic_vpn[2]
  ))
}

# Calcular IC para ambos predictores
ic_hepatica <- calcular_ic(roc_hepatica, coords_hepatica$threshold)
ic_esplenica <- calcular_ic(roc_esplenica, coords_esplenica$threshold)

# ==============================================================================
# 6. TABLAS DE RENDIMIENTO DIAGNÓSTICO
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.1 Elastografía Hepática
# ------------------------------------------------------------------------------
resultados_hepatica <- data.frame(
  Medida = c("Punto de corte (kPa)", "Sensibilidad", "Especificidad", 
             "VPP", "VPN", "Exactitud"),
  Valor = round(c(coords_hepatica$threshold, 
                  ic_hepatica$sensibilidad,
                  ic_hepatica$especificidad,
                  ic_hepatica$vpp,
                  ic_hepatica$vpn,
                  coords_hepatica$accuracy), 4),
  IC_inferior = round(c(NA,
                        ic_hepatica$ic_sens_inf,
                        ic_hepatica$ic_spec_inf,
                        ic_hepatica$ic_vpp_inf,
                        ic_hepatica$ic_vpn_inf,
                        NA), 4),
  IC_superior = round(c(NA,
                        ic_hepatica$ic_sens_sup,
                        ic_hepatica$ic_spec_sup,
                        ic_hepatica$ic_vpp_sup,
                        ic_hepatica$ic_vpn_sup,
                        NA), 4)
)

kable(resultados_hepatica,
      align = c("l", "c", "c", "c"),
      caption = "Tabla 2. Rendimiento diagnóstico de elastografía hepática")

# ------------------------------------------------------------------------------
# 6.2 Elastografía Esplénica
# ------------------------------------------------------------------------------
resultados_esplenica <- data.frame(
  Medida = c("Punto de corte (kPa)", "Sensibilidad", "Especificidad", 
             "VPP", "VPN", "Exactitud"),
  Valor = round(c(coords_esplenica$threshold, 
                  ic_esplenica$sensibilidad,
                  ic_esplenica$especificidad,
                  ic_esplenica$vpp,
                  ic_esplenica$vpn,
                  coords_esplenica$accuracy), 4),
  IC_inferior = round(c(NA,
                        ic_esplenica$ic_sens_inf,
                        ic_esplenica$ic_spec_inf,
                        ic_esplenica$ic_vpp_inf,
                        ic_esplenica$ic_vpn_inf,
                        NA), 4),
  IC_superior = round(c(NA,
                        ic_esplenica$ic_sens_sup,
                        ic_esplenica$ic_spec_sup,
                        ic_esplenica$ic_vpp_sup,
                        ic_esplenica$ic_vpn_sup,
                        NA), 4)
)

kable(resultados_esplenica,
      align = c("l", "c", "c", "c"),
      caption = "Tabla 3. Rendimiento diagnóstico de elastografía esplénica")

# ==============================================================================
# 7. COMPARACIÓN DE CURVAS ROC (TEST DE DELONG)
# ==============================================================================

# Comparar curvas ROC
test_delong <- roc.test(roc_hepatica, roc_esplenica, method = "delong")

# Tabla de resultados
resultados_delong <- data.frame(
  Comparacion = "Elastografía Hepática vs Esplénica",
  Diferencia_AUC = round(auc_hepatica - auc_esplenica, 4),
  Estadistico_Z = round(as.numeric(test_delong$statistic), 4),
  Valor_p = round(test_delong$p.value, 4),
  Interpretacion = ifelse(test_delong$p.value < 0.05, 
                          "Diferencia significativa", 
                          "Sin diferencia significativa")
)

kable(resultados_delong,
      align = c("l", "c", "c", "c", "l"),
      caption = "Tabla 4. Comparación de curvas ROC mediante test de DeLong")

# ==============================================================================
# 8. VISUALIZACIÓN
# ==============================================================================

# ------------------------------------------------------------------------------
# 8.1 Curva ROC: Elastografía Hepática
# ------------------------------------------------------------------------------
plot(roc_hepatica, 
     col = "#E74C3C", 
     lwd = 2.5,
     main = "Curva ROC: Elastografía Hepática\nPredicción de Varices Gastroesofágicas",
     cex.main = 1.2)

# Agregar punto óptimo de corte
points(coords_hepatica$specificity, coords_hepatica$sensitivity, 
       pch = 19, col = "#C0392B", cex = 2)

# Agregar información de AUC e IC
text(0.55, 0.25, 
     paste0("AUC = ", round(auc_hepatica, 3), "\n",
            "IC 95%: ", round(ci_hepatica[1], 3), " - ", round(ci_hepatica[3], 3)),
     cex = 1.1, col = "#C0392B", font = 2)

# Leyenda con punto óptimo
legend("bottomright", 
       legend = paste0("Punto óptimo: ", round(coords_hepatica$threshold, 2), " kPa\n",
                       "Sens: ", round(coords_hepatica$sensitivity, 2), 
                       " | Esp: ", round(coords_hepatica$specificity, 2)),
       col = "#C0392B", pch = 19, cex = 0.9, bty = "n")

# ------------------------------------------------------------------------------
# 8.2 Curva ROC: Elastografía Esplénica
# ------------------------------------------------------------------------------
plot(roc_esplenica, 
     col = "#3498DB", 
     lwd = 2.5,
     main = "Curva ROC: Elastografía Esplénica\nPredicción de Varices Gastroesofágicas",
     cex.main = 1.2)

# Agregar punto óptimo de corte
points(coords_esplenica$specificity, coords_esplenica$sensitivity, 
       pch = 19, col = "#2980B9", cex = 2)

# Agregar información de AUC e IC
text(0.55, 0.25, 
     paste0("AUC = ", round(auc_esplenica, 3), "\n",
            "IC 95%: ", round(ci_esplenica[1], 3), " - ", round(ci_esplenica[3], 3)),
     cex = 1.1, col = "#2980B9", font = 2)

# Leyenda con punto óptimo
legend("bottomright", 
       legend = paste0("Punto óptimo: ", round(coords_esplenica$threshold, 2), " kPa\n",
                       "Sens: ", round(coords_esplenica$sensitivity, 2), 
                       " | Esp: ", round(coords_esplenica$specificity, 2)),
       col = "#2980B9", pch = 19, cex = 0.9, bty = "n")

# ------------------------------------------------------------------------------
# 8.3 Gráfico Comparativo de Ambas Curvas
# ------------------------------------------------------------------------------
plot(roc_hepatica, 
     col = "#E74C3C", 
     lwd = 3,
     main = "Comparación de Curvas ROC\nPredicción de Varices Gastroesofágicas",
     cex.main = 1.3)

# Agregar curva esplénica
plot(roc_esplenica, 
     col = "#3498DB", 
     lwd = 3,
     add = TRUE)

# Línea de referencia (diagonal)
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 1.5)

# Agregar puntos óptimos de corte
points(coords_hepatica$specificity, coords_hepatica$sensitivity, 
       pch = 19, col = "#C0392B", cex = 2.5)
points(coords_esplenica$specificity, coords_esplenica$sensitivity, 
       pch = 19, col = "#2980B9", cex = 2.5)

# Leyenda completa
legend("bottomright", 
       legend = c(
         paste0("Elastografía Hepática (AUC = ", round(auc_hepatica, 3), 
                "; IC 95%: ", round(ci_hepatica[1], 3), "-", round(ci_hepatica[3], 3), ")"),
         paste0("Elastografía Esplénica (AUC = ", round(auc_esplenica, 3), 
                "; IC 95%: ", round(ci_esplenica[1], 3), "-", round(ci_esplenica[3], 3), ")"),
         paste0("Test de DeLong: p = ", round(test_delong$p.value, 4))
       ),
       col = c("#E74C3C", "#3498DB", "black"), 
       lwd = c(3, 3, 0),
       cex = 0.85,
       bty = "n")

# ==============================================================================
# 9. INTERPRETACIÓN DE RESULTADOS
# ==============================================================================
#
# ÁREA BAJO LA CURVA (AUC):
# - AUC = 0.5: Sin capacidad discriminativa (equivalente al azar)
# - AUC 0.5-0.6: Discriminación fallida
# - AUC 0.6-0.7: Discriminación pobre
# - AUC 0.7-0.8: Discriminación aceptable
# - AUC 0.8-0.9: Discriminación buena
# - AUC > 0.9: Discriminación excelente
#
# ÍNDICE DE YOUDEN:
# - J = Sensibilidad + Especificidad - 1
# - Maximiza la suma de sensibilidad y especificidad
# - El punto óptimo es donde J es máximo
#
# ==============================================================================

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
