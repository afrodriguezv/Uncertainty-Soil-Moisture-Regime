
# =============================================================================
# ESTIMACIÓN DE LA SECCIÓN CONTROL DE HUMEDAD DEL SUELO  
# y ESTIMACIÓN TAMAÑO DE PARTICULAS (USDA)
# =============================================================================

library(aqp)
library(dplyr)
library(lattice)

# =============================================================================
# 1. Cargar de Archivos
# =============================================================================

datos <- read.csv(
  "Data/data_prop_pfs.csv",
  header = TRUE,
  sep = ";"
)

str(datos)

# =============================================================================
# 2. Transformación de datos
# =============================================================================

datos$Value  <- as.numeric(datos$Value)
datos$Chroma <- as.numeric(datos$Chroma)
datos$bottom <- as.numeric(datos$bottom)
datos$clay   <- as.numeric(datos$clay)
datos$BD     <- as.numeric(datos$BD)

# Renombrar columnas para aqp
datos <- datos %>%
  rename(
    id     = id,  # Identificador
    top    = top, # limite superior horizonte
    bottom = bottom, #limite infierior horizonte
    sand   = sand, # arena (%)
    silt   = silt, # limo (%)
    clay   = clay, #arcilla (%)
    FC     = FC, #capacidad de campo (%)
    PMP    = PMP, # p. de marchitez permanente (%)
    BD     = BD, # densidad aparente (g.cm-3)
    FR     = FR # Fragmentos roca (% volumen)
  )

# =============================================================================
# 3. Crear SoilProfileCollection libreria aqp
# =============================================================================

pfs <- as.data.frame(datos)

depths(pfs) <- id ~ top + bottom

spc_in_sync(pfs)

lisfalse <- checkHzDepthLogic(pfs)
lisfalse <- lisfalse %>% filter(valid == "FALSE")
print(lisfalse)

length(pfs)
nrow(pfs)
names(pfs)
min(pfs)
max(pfs)

profileApply(pfs, function(profile) {
  nrow(horizons(profile))
})

# =============================================================================
# 4. Clase Textural por horizonte
# =============================================================================

pfs$texcl <- ssc_to_texcl(
  sand = pfs$sand,
  clay = pfs$clay
)

hztexclname(pfs) <- "texcl"

# =============================================================================
# 5. Clasificación visual arcillas
# =============================================================================

breaks <- seq(0, 100, by = 20)
labels <- c("0-20", "20-40", "40-60", "60-80", "80-100")

pfs$clay_gr <- cut(
  pfs$clay,
  breaks = breaks,
  include.lowest = TRUE,
  right = TRUE,
  labels = labels
)

pfs$clay_gr <- as.character(pfs$clay_gr)

# Color
pal <- c("#757575", "#a1887f", "#e59866", "#af601a", "#ba4a00")

pfs$clay_gr_color <- cut(
  pfs$clay,
  breaks = breaks,
  include.lowest = TRUE,
  right = TRUE,
  labels = pal
)

pfs$clay_gr_color <- as.character(pfs$clay_gr_color)

# =============================================================================
# 6. Cálculo SCHS
# =============================================================================

source("Script/function/SCMS.R")

SCMS_limits_full <- profileApply(pfs, SCMS, frameify = TRUE)

SCMS_limits_filter <-
  SCMS_limits_full[
    !(SCMS_limits_full$top == 0 & SCMS_limits_full$bottom == 0),
  ]

SCMS_limits <-
  SCMS_limits_full[
    SCMS_limits_full$top > 0 & SCMS_limits_full$bottom > 0,
  ]

# =============================================================================
# 7. Preparación para graficar
# =============================================================================

a <- SCMS_limits

names(a)[2] <- "top_scms"
names(a)[3] <- "bottom_scms"

a$label <- paste(
  round(a$top_scms, 0),
  round(a$bottom_scms, 0),
  sep = "-"
)

pfs_scms <- subset(pfs, id %in% SCMS_limits$id)

site(pfs_scms) <- a

aqp::hzdesgnname(pfs_scms) <- "Horizonte"

total_profiles <- length(pfs_scms)
print(total_profiles)

step_size <- 20

par(mfrow = c(8, 1), mar = c(0, 0, 3, 3))

end_idx <- 0

# =============================================================================
# 8. Gráficas de perfiles
# =============================================================================

for (start_idx in seq(1, total_profiles, by = step_size)) {
  
  end_idx <- min(start_idx + step_size - 1, total_profiles)
  
  plotSPC(
    pfs_scms[start_idx:end_idx],
    id.style = "top",
    print.id = TRUE,
    label.cex = 0.7,
    with = 0.3,
    cex.names = 0.8,
    col.legend.cex = 1,
    depth.axis = FALSE,
    color = "BD",
    show.legend = FALSE,
    col.label = NA,
    shrink = TRUE,
    hz.depths = TRUE,
    hz.depths.offset = 0.03,
    fixLabelCollisions = TRUE,
    name.style = "center-center",
    max.depth = 155,
    n = step_size
  )
  
  if (start_idx == 1) {
    mtext(
      "Soil Moisture Control Section Estimated",
      side = 3,
      at = 10,
      line = 1,
      font = 3,
      adj = 0,
      cex = 1
    )
  }
  
  addBracket(
    a,
    col = "red",
    offset = -0.3,
    tick.length = 0.1,
    label.cex = 0.8,
    lwd = 2,
    label
  )
}

# Guardar gráfico (opcional)
dev.copy(
  png,
  filename = "SCH.png",
  width = 5000,
  height = 5500,
  res = 300,
  bg = "white"
)

dev.off()

# =============================================================================
# 9. Clasificación tamaño de partículas
# =============================================================================

pfs_scms$taxpartsize <- texture_to_taxpartsize(
  texcl = pfs_scms$texcl,
  clay = pfs_scms$clay,
  sand = pfs_scms$sand,
  fragvoltot = pfs_scms$FR
)

hzdesgnname(pfs_scms) <- "Horizonte"
hztexclname(pfs_scms) <- "texcl"
hzmetaname(pfs_scms, "clay") <- "clay"

id <- data.frame(id = pfs_scms$id)

pscs <- estimatePSCS(pfs_scms)

pscs <- merge(id, pscs, by = "id")

names(pscs)[2:3] <- c("top", "bottom")

fm_text <- hz_to_taxpartsize(
  horizons(pfs_scms),
  pscs
)

names(fm_text)[3] <- "taxpartsize_id"

fm_text$taxpartsizemod <- NULL

names(pscs)[2:3] <- c("top_pscs", "bottom_pscs")

# =============================================================================
# 10. Organización final
# =============================================================================

site(pfs_scms) <- fm_text

site(pfs_scms)

oa1 <- order(site(pfs_scms)$taxpartsize_id)

# =============================================================================
# 11. Exportación
# =============================================================================

datos2 <- horizons(pfs_scms)
sites2 <- site(pfs_scms)

write.csv(datos2, "Out/Pfs_hzt_sch.csv", row.names = FALSE)
write.csv(sites2, "Out/Pfs_sch.csv", row.names = FALSE)
