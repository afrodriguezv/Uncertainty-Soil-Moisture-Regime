# ============================================
# ANÁLISIS CON MUESTRA PEQUEÑA (n=8)
# ============================================

# Resultados de Newhall


df_eval <- data.frame(
  Id = c("ARBORADA", "CARIMAGUA", "LALIBERTAD", "MAVALLE", 
         "NUBES", "SHEEFER", "TALUMA", "VEGAS", "YUCAO"),
  campo = c("Ústico", "Ústico", "Údico", "Sin dato", "Ústico", 
            "Ústico", "Údico", "Údico", "Údico"),
  RH_2023 = c("Ústico", "Ústico", "Údico", "Ústico", "Ústico", 
              "Ústico", "Ústico", "Údico", "Ústico"),
  RH_2024 = c("Ústico", "Ústico", "Údico", "Ústico", "Ústico", 
              "Ústico", "Ústico", "Údico", "Ústico"),
  RH_NC = c("Ústico", "Ústico", "Údico", "Ústico", "Ústico", 
            "Ústico", "Ústico", "Údico", "Ústico"),
  CAD_40 = c("Ústico", "Ústico", "Údico", "Ústico", "Ústico", 
             "Ústico", "Ústico", "Údico", "Ústico"),
  CAD_100 = c("Ústico", "Údico", "Údico", "Údico", "Údico", 
              "Údico", "Údico", "Údico", "Údico"),
  CAD_150 = c("Údico", "Ústico", "Údico", "Ústico", "Údico", 
              "Ústico", "Ústico", "Údico", "Ústico"),
  CAD_200 = c("Údico", "Údico", "Údico", "Ústico", "Údico", 
              "Údico", "Ústico", "Údico", "Ústico")
)

niveles <- c("Ústico", "Údico")

df_filtrado <- df_eval %>%
  filter(if_all(-Id, ~ !grepl("Sin dato", .x)))

# Asegurar que sean factores ordenados
datos <- data.frame(
  id = df_filtrado$Id,
  campo = factor(df_filtrado$campo, levels = niveles),
  newhall = factor(df_filtrado$CAD_150, levels = niveles) # cambiar para cada set
)


# Tabla de contingencia
tabla <- table(Campo = datos$campo, Newhall = datos$newhall)
cat("\nTABLA DE CONTINGENCIA (n=8):\n")
print(tabla)

# ============================================
# 1. PRUEBA EXACTA DE FISHER (RECOMENDADA)
# ============================================
cat("\n========================================\n")
cat("1. PRUEBA EXACTA DE FISHER (n=8)\n")
cat("========================================\n")

fisher_test <- fisher.test(tabla, simulate.p.value = TRUE, B = 10000)
print(fisher_test)

# Interpretación
if(fisher_test$p.value < 0.05) {
  cat("\n→ Conclusión: Existe asociación significativa (p < 0.05)\n")
} else {
  cat("\n→ Conclusión: No hay evidencia suficiente de asociación (p > 0.05)\n")
  cat("  (ESPERABLE con n=8, baja potencia estadística)\n")
}

# ============================================
# 2. KAPPA (Métrica clave con muestra pequeña)
# ============================================
cat("\n========================================\n")
cat("2. COEFICIENTE KAPPA (robusto para n pequeña)\n")
cat("========================================\n")

library(irr)
kappa_data <- cbind(as.character(datos$campo), as.character(datos$newhall))
kappa_result <- kappa2(kappa_data)

print(kappa_result)

# Interpretación Kappa
interpretar_kappa <- function(k) {
  if(k < 0) return("peor que azar")
  if(k <= 0.2) return("insignificante")
  if(k <= 0.4) return("leve")
  if(k <= 0.6) return("moderado")
  if(k <= 0.8) return("sustancial")
  return("casi perfecto")
}

cat("\nInterpretación:", interpretar_kappa(kappa_result$value), "\n")

# ============================================
# 3. ANÁLISIS DESCRIPTIVO DETALLADO
# ============================================
cat("\n========================================\n")
cat("3. ANÁLISIS DESCRIPTIVO (más informativo)\n")
cat("========================================\n")

# Exactitud
exactitud <- sum(diag(tabla)) / sum(tabla)
cat("Exactitud global:", round(exactitud*100, 1), "%\n")

# Matriz de confusión con porcentajes
cat("\nMatriz de confusión (% por fila - Campo):\n")
prop_fila <- prop.table(tabla, margin = 1) * 100
print(round(prop_fila, 1))

cat("\nMatriz de confusión (% por columna - Newhall):\n")
prop_col <- prop.table(tabla, margin = 2) * 100
print(round(prop_col, 1))

# ============================================
# 4. VISUALIZACIÓN ADAPTADA
# ============================================
cat("\n========================================\n")
cat("4. VISUALIZACIÓN DE PATRONES\n")
cat("========================================\n")

# Gráfico de concordancia individual
library(ggplot2)

# Crear data frame para visualización
plot_data <- datos
plot_data$concordancia <- ifelse(datos$campo == datos$newhall, 
                                 "Concordancia", "Discordancia")
plot_data$id <- 1:nrow(plot_data)

# Gráfico de puntos individuales
p <- ggplot(plot_data, aes(x = campo, y = newhall, color = concordancia)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_jitter(width = 0.1, height = 0.1, size = 5, alpha = 0.7) +
  scale_color_manual(values = c("Concordancia" = "#2ECC71", 
                                "Discordancia" = "#E74C3C")) +
  labs(title = "Patrones de Concordancia Individual (n=8)",
       subtitle = paste("Kappa =", round(kappa_result$value, 3),
                        "| Exactitud =", round(exactitud*100, 1), "%"),
       x = "Clasificación Campo",
       y = "Clasificación Newhall",
       color = "Tipo") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

