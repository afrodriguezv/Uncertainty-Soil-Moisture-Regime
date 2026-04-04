# =============================================================================
# ANÁLISIS DE DIAS CONSECUTIVOS DE ESTADOS DE HUMEDAD DEL SUELO
# =============================================================================
# Este script calcula estadísticas de periodos consecutivos (rachas)
# para diferentes estados de humedad del suelo por sitio y profundidad.
# =============================================================================
# =============================================================================
# 1.  LIBRERÍAS NECESARIAS
# =============================================================================

# Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(data.table)  # Para rleid (identificación de grupos consecutivos)

# Silenciar mensajes de dplyr (opcional)
options(dplyr.summarise.inform = FALSE)

# -----------------------------------------------------------------------------
# 2. PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------------

# Convertir a data.frame (state_moisture_50p proviene de humedades_espaol.R linea 925)
df <- read.csv("Out/Estados_humedad/Estados_humedad_diario.csv",
               sep=",")

# Convertir a data.frame (state_moisture_50p proviene de humedades_espaol.R linea 925)
# df <- read.csv("Out/Estados_humedad/Estados_humedad_diario_50p.csv",sep=",") # activar para CAD al 50%

#Verificar campo Fecha
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

# Seleccionar 360 dias en comun para todos los sitios
fecha_fin <- max(df$Date, na.rm = TRUE)
fecha_inicio <- fecha_fin - 359

# Filtrar los 360 día
df <- df %>%
  filter(Date >= fecha_inicio & Date <= fecha_fin)


# Ordenar datos (fundamental para identificar días consecutivos)
df <- df %>%
  arrange(Site, Depth, Date)

# Identificar periodos consecutivos por Site y Depth
# rleid crea un ID único para cada racha consecutiva de mismo estado
df <- df %>%
  group_by(Site, Depth) %>%
  mutate(
    consecutive_id = data.table::rleid(state_p_label)
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 3. CÁLCULO DE DURACIÓN DE RACHAS
# -----------------------------------------------------------------------------

cat("Calculando duración de rachas...\n")

rachas <- df %>%
  group_by(Site, Depth, state_p_label, consecutive_id) %>%
  summarise(
    inicio = min(Date),
    fin = max(Date),
    duracion_dias = as.numeric(difftime(fin, inicio, units = "days")) + 1,  # +1 para conteo inclusivo
    .groups = 'drop'
  )

cat("Rachas identificadas:", nrow(rachas), "\n\n")

# -----------------------------------------------------------------------------
# 4. ESTADÍSTICAS DESCRIPTIVAS POR SITIO, PROFUNDIDAD Y ESTADO
# -----------------------------------------------------------------------------

cat("Calculando estadísticas descriptivas...\n")

estadisticas <- rachas %>%
  group_by(Site, Depth, state_p_label) %>%
  summarise(
    # Conteo de periodos
    n_rachas = n(),
    
    # Estadísticas de duración
    min_dias = min(duracion_dias),
    max_dias = max(duracion_dias),
    mean_dias = mean(duracion_dias),
    median_dias = median(duracion_dias),
    sd_dias = sd(duracion_dias),
    p25_dias = quantile(duracion_dias, 0.25, names = FALSE),
    p75_dias = quantile(duracion_dias, 0.75, names = FALSE),
    
    # Total de días acumulados
    total_dias = sum(duracion_dias),
    
    .groups = 'drop'
  ) %>%
  mutate(
    # Redondear para mejor presentación
    across(ends_with("_dias"), ~round(.x, 1))
  )

# -----------------------------------------------------------------------------
# 5. VISUALIZACIÓN DE RESULTADOS
# -----------------------------------------------------------------------------

# 5.1 Tabla básica de estadísticas
cat("\n", rep("=", 60), "\n", sep = "")
cat("ESTADÍSTICAS DE DURACIÓN DE RACHAS (días)\n")
cat(rep("=", 60), "\n", sep = "")
print(estadisticas)

# 5.2 Top 5 rachas más largas por categoría
cat("\n", rep("=", 60), "\n", sep = "")
cat("TOP 5 RACHAS MÁS LARGAS POR CATEGORÍA\n")
cat(rep("=", 60), "\n", sep = "")

rachas_top <- rachas %>%
  arrange(Site, Depth, desc(duracion_dias)) %>%
  group_by(Site, Depth, state_p_label) %>%
  slice_head(n = 5) %>%
  ungroup()

print(rachas_top)

# 5.3 Resumen compacto en formato ancho
cat("\n", rep("=", 60), "\n", sep = "")
cat("RESUMEN COMPACTO: n, media (min-max)\n")
cat(rep("=", 60), "\n", sep = "")

resumen_compacto <- estadisticas %>%
  mutate(
    duracion_resumen = paste0(
      "n=", n_rachas,
      ", media=", round(mean_dias, 1),
      " (", min_dias, "-", max_dias, ")"
    )
  ) %>%
  select(Site, Depth, state_p_label, duracion_resumen)

resumen_ancho <- resumen_compacto %>%
  pivot_wider(
    names_from = state_p_label,
    values_from = duracion_resumen,
    values_fill = "Sin datos"
  )

print(resumen_ancho)

# 5.4 Tabla completa en formato ancho (todas las estadísticas)
cat("\n", rep("=", 60), "\n", sep = "")
cat("ESTADÍSTICAS COMPLETAS EN FORMATO ANCHO\n")
cat(rep("=", 60), "\n", sep = "")

estadisticas_anchas <- estadisticas %>%
  pivot_wider(
    id_cols = c(Site, Depth),
    names_from = state_p_label,
    values_from = c(n_rachas, mean_dias, median_dias, max_dias, total_dias),
    names_glue = "{state_p_label}_{.value}"
  )

print(estadisticas_anchas)

# -----------------------------------------------------------------------------
# 6. EXPORTAR DE RESULTADOS (OPCIONAL)
# -----------------------------------------------------------------------------

# Crear carpeta de resultados si no existe
output_dir <- "resul_rachas"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("\nCarpeta creada:", output_dir, "\n")
}

# Guardar tablas como CSV
write.csv(rachas, file.path(output_dir, "rachas.csv"), 
          row.names = FALSE)
write.csv(estadisticas, file.path(output_dir, "estadisticas_rachas.csv"), 
          row.names = FALSE)
write.csv(resumen_ancho, file.path(output_dir, "resumen_compacto.csv"), 
          row.names = FALSE)
write.csv(rachas_top, file.path(output_dir, "top5_rachas.csv"), 
          row.names = FALSE)

cat("\n", rep("=", 60), "\n", sep = "")
cat("PROCESO COMPLETADO\n")
cat("Archivos guardados en:", output_dir, "\n")
cat(rep("=", 60), "\n", sep = "")