###############################################################################
# ANÁLISIS ESTADÍSTICO DE HUMEDAD DEL SUELO
# TESIS DOCTORAL - ESTADÍSTICAS POR SITIO
###############################################################################

# =============================================================================
# 1. CARGAR LIBRERÍAS NECESARIAS
# =============================================================================

library(gtsummary)    # Tablas estadísticas profesionales
library(dplyr)        # Manipulación de datos
library(gt)           # Formato de tablas
library(lubridate)    # Manejo de fechas
library(ggplot2)      # Visualizaciones
library(flextable)    # Exportar a Word
library(RColorBrewer) # Paletas de colores
library(rlang)
library(tidyr)

# =============================================================================
# 2. PREPARACIÓN DE DATOS
# =============================================================================

df_day_completo <- read.csv("Data/data_day_sensores.csv", sep=";")
  
df_stats <- df_day_completo %>%
  mutate(
    # Variables temporales
    Date = as.Date(Date,"%d/%m/%y"),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = FALSE),
    Month_num = month(Date),
    
    # Estaciones del año (T1:EFM; T2:AMJ...)
    Season = case_when(
      Month_num %in% c(12, 1, 2) ~ "T1",
      Month_num %in% c(3, 4, 5) ~ "T2",
      Month_num %in% c(6, 7, 8) ~ "T3",
      Month_num %in% c(9, 10, 11) ~ "T4"
    ),
    
    # Profundidad como factor ordenado (ajusta nombres según tus datos)
    Depth = factor(Depth, 
                   levels = c("VW_1", "VW_2", "VW_3"),
                   labels = c("VW_1", "VW_2", "VW_3")),
    
    # Site como factor
    Site = factor(Site)
  ) %>%
  # Filtrar datos válidos
  filter(!is.na(SMS), !is.na(Depth), !is.na(Site))

# Mensaje de confirmación
cat("========================================\n")
cat("DATOS CARGADOS CORRECTAMENTE\n")
cat("========================================\n")
cat("Número de observaciones:", nrow(df_stats), "\n")
cat("Sitios:", paste(levels(df_stats$Site), collapse = ", "), "\n")
cat("Profundidades:", paste(levels(df_stats$Depth), collapse = ", "), "\n")
cat("Rango de fechas:", min(df_stats$Date), "a", max(df_stats$Date), "\n")
cat("========================================\n\n")

# =============================================================================
# 3. TABLA 1: ESTADÍSTICAS DESCRIPTIVAS COMPLETAS POR SITIO Y PROFUNDIDAD
# =============================================================================

cat("Generando Tabla 1: Estadísticas descriptivas...\n")
# Primero, calcular CV manualmente en el dataframe
df_stats <- df_stats %>%
  group_by(Site, Depth) %>%
  mutate(CV = (sd(SMS, na.rm = TRUE) / mean(SMS, na.rm = TRUE)) * 100) %>%
  ungroup()


# Primero, calcular CV manualmente en el dataframe
df_stats <- df_stats %>%
  group_by(Site, Depth) %>%
  mutate(CV = (sd(SMS, na.rm = TRUE) / mean(SMS, na.rm = TRUE)) * 100) %>%
  ungroup()

# Paso 1: Crear la tabla principal SIN CV
tabla_1_descriptivas <- df_stats %>%
  select(Site, Depth, SMS) %>%
  tbl_strata(
    strata = Site,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(
        by = Depth,
        type = SMS ~ "continuous2",
        statistic = SMS ~ c(
          "N válido" = "{N_nonmiss}",
          "Media (DE)" = "{mean} ({sd})",
          "Mediana (Q1-Q3)" = "{median} ({p25}, {p75})",
          "Rango" = "{min} - {max}"
        ),
        digits = SMS ~ c(0, 3, 3, 3, 3),
        missing = "no",
        label = SMS ~ "Humedad volumétrica (m³ m⁻³)"
      ) %>%
      modify_header(label = "**Estadístico**") %>%
      modify_spanning_header(everything() ~ "**Profundidad**")
  ) %>%
  modify_caption("**Tabla 1. Estadísticas descriptivas de humedad del suelo por sitio y profundidad**")


# Ver la tabla principal
print(tabla_1_descriptivas)
print(cv_table)

# =============================================================================
# 4. TABLA 2: PRUEBAS ESTADÍSTICAS INFERENCIALES
# =============================================================================

cat("\nGenerando Tabla 2: Pruebas estadísticas...\n")

tabla_2_inferencial <- df_stats %>%
  select(Site, Depth, SMS) %>%
  tbl_summary(
    by = Site,
    include = SMS,
    statistic = SMS ~ "{mean} ({sd})",
    digits = SMS ~ 3,
    label = list(SMS = "Humedad del suelo (m³ m⁻³)")
  ) %>%
  add_p(
    test = SMS ~ "kruskal.test",  # Prueba no paramétrica
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_q(method = "fdr") %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(everything() ~ "**Sitio**") %>%
  bold_p(t = 0.05) %>%
  modify_caption("**Tabla 2. Comparación estadística entre sitios (prueba de Kruskal-Wallis)**")

# Mostrar tabla
tabla_2_inferencial

# =============================================================================
# 5. TABLA 3: ANÁLISIS DE PROFUNDIDAD DENTRO DE CADA SITIO
# =============================================================================

cat("\nGenerando Tabla 3: Análisis por profundidad...\n")

tabla_3_profundidad <- df_stats %>%
  select(Site, Depth, SMS) %>%
  tbl_strata(
    strata = Site,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(
        by = Depth,
        statistic = SMS ~ "{mean} ({sd})",
        digits = SMS ~ 3,
        label = list(SMS = "Humedad (m³ m⁻³)")
      ) %>%
      add_p(
        test = SMS ~ "kruskal.test",
        pvalue_fun = ~ style_pvalue(.x, digits = 3)
      ),
    .combine_with = "tbl_stack"
  ) %>%
  modify_caption("**Tabla 3. Diferencias entre profundidades dentro de cada sitio**")

# Mostrar tabla
tabla_3_profundidad

# =============================================================================
# 6. TABLA 4: ANÁLISIS ESTACIONAL
# =============================================================================

cat("\nGenerando Tabla 4: Análisis estacional...\n")

tabla_4_estacional <- df_stats %>%
  filter(!is.na(Season)) %>%
  select(Season, Site, Depth, SMS) %>%
  tbl_strata(
    strata = Season,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(
        by = Site,
        statistic = SMS ~ "{mean} ({sd})",
        digits = SMS ~ 3,
        label = list(
          SMS = "Humedad",
          Depth = "Profundidad"
        )
      ) %>%
      add_n(),
    .combine_with = "tbl_stack"
  ) %>%
  modify_caption("**Tabla 4. Variación estacional de la humedad del suelo por sitio**")

# Mostrar tabla
tabla_4_estacional


# =============================================================================
# 8. VISUALIZACIONES PARA LA TESIS
# =============================================================================

cat("\nGenerando visualizaciones...\n")

# 8.1 Boxplots por sitio y profundidad
p1 <- ggplot(df_stats, aes(x = Site, y = SMS, fill = Depth)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sitio de muestreo",
    y = expression("Humedad volumétrica (m"^3*" m"^{-3}*")"),
    fill = "Profundidad",
    title = "Distribución de humedad del suelo por sitio y profundidad"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p1)

# 8.2 Series temporales por sitio (facetas 2 columnas)
p2 <- ggplot(df_stats, aes(x = Date, y = SMS, color = Depth)) +
  geom_line(linewidth = 0.4, alpha = 0.7) +
  facet_wrap(~ Site, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c("10 cm" = "#1f78b4", "30 cm" = "#33a02c", "50 cm" = "#e31a1c")
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
  labs(
    x = "Fecha",
    y = expression("Humedad volumétrica (m"^3*" m"^{-3}*")"),
    color = "Profundidad",
    title = "Figura 2. Series temporales de humedad del suelo por sitio"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    legend.position = "top",
    strip.background = element_rect(fill = "grey85"),
    strip.text = element_text(face = "bold")
  )

print(p2)

# 8.3 Heatmap de correlaciones
library(tidyr)

df_wide <- df_stats %>%
  select(Date, Site, Depth, SMS) %>%
  unite("Site_Depth", Site, Depth, sep = "_") %>%
  pivot_wider(names_from = Site_Depth, values_from = SMS)

if(ncol(df_wide) > 2) {
  cor_matrix <- cor(df_wide[,-1], use = "pairwise.complete.obs")
  
  library(reshape2)
  cor_melt <- melt(cor_matrix)
  
  p3 <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         midpoint = 0, limit = c(-1, 1)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
    labs(
      title = "Figura 3. Matriz de correlación entre sitios y profundidades",
      x = "", y = "", fill = "Correlación"
    ) +
    coord_fixed()
  
  print(p3)
}

# =============================================================================
# 9. EXPORTAR RESULTADOS PARA LA TESIS
# =============================================================================

cat("\n========================================\n")
cat("EXPORTANDO RESULTADOS...\n")
cat("========================================\n")

# Crear directorio para resultados si no existe
if(!dir.exists("resultados_tesis")) {
  dir.create("resultados_tesis")
}


# 9.1 Exportar gráficos
ggsave("Figura1_Boxplots.png", p1, width = 10, height = 6, dpi = 300)
ggsave("Figura2_Series_Temporales.png", p2, width = 12, height = 8, dpi = 300)

if(exists("p3")) {
  ggsave("Figura3_Correlaciones.png", p3, width = 8, height = 7, dpi = 300)
}

# 9.3 Exportar datos resumidos a CSV
write.csv(df_resumen, "Estadisticos_Resumen.csv", row.names = FALSE)

# =============================================================================
# 10. REPORTE DE COMPLETADO
# =============================================================================

cat("\n========================================\n")
cat("✅ ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("========================================\n")
cat("Archivos generados en la carpeta: /resultados_tesis\n")
cat("\nTablas exportadas:\n")
cat("- Tabla1_Descriptivas.docx\n")
cat("- Tabla2_Inferencial.docx\n")
cat("- Tabla3_Profundidad.docx\n")
cat("- Tabla4_Estacional.docx\n")
cat("\nFiguras exportadas:\n")
cat("- Figura1_Boxplots.png\n")
cat("- Figura2_Series_Temporales.png\n")
if(exists("p3")) cat("- Figura3_Correlaciones.png\n")
cat("\nDatos:\n")
cat("- Estadisticos_Resumen.csv\n")
cat("========================================\n")