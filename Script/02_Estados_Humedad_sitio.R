# =============================================================================
# ESTADOS DE HUMEDAD DEL SUELO POR SITIO
# =============================================================================
# Este script calcula y grafica los estados de humedad del suelo (seco, humedo,
# saturado) para diferentes sitios y profunidd de los sensores de humedad.
# =============================================================================
# =============================================================================
# LIBRERÍAS NECESARIAS
# =============================================================================

# Instalación
# install.packages(c(
#   "dplyr",        # Para manipulación de datos (bind_rows)
#   "reshape2",     # Para dcast
#   "lattice",      # Para levelplot, trellis.par.set
#   "latticeExtra", # Para funciones adicionales de lattice
#   "soiltexture",  # Posiblemente necesario para estimateSoilMoistureState
#   "aqp"           # Para estimateSoilMoistureState (paquete de suelos)
# ))

# Carga de librerías

library(dplyr)        # Para bind_rows y manipulación de datos
library(reshape2)     # Para dcast (formato ancho)
library(lattice)      # Para levelplot, trellis.par.set
library(latticeExtra) # Para funciones complementarias de lattice
library(aqp)          # Para estimateSoilMoistureState (análisis de perfiles de suelo) ó
library(sharpshootR)  # Para estimateSoilMoistureState (análisis de perfiles de suelo)
library(lubridate)    # Para formato de fechas
library(ggplot2)      # Si quieres migrar gráficos en el futuro
library(grid)         # Para manipulación de gráficos
library(tactile)


# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN INICIAL
# -----------------------------------------------------------------------------

# Definir carpeta de resultados
carpeta_resultados <- "Out/Estados_humedad"

# Crear la carpeta si no existe
if (!dir.exists(carpeta_resultados)) {
  dir.create(carpeta_resultados, recursive = TRUE)
  cat("Carpeta creada:", carpeta_resultados, "\n")
}

# Subcarpeta para imágenes por sitio
carpeta_imagenes <- file.path(carpeta_resultados, "imagenes", "porsitio")
if (!dir.exists(carpeta_imagenes)) {
  dir.create(carpeta_imagenes, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 2. DATOS DE ENTRADA
# -----------------------------------------------------------------------------

# Datos diarios de sensores
df_day <- read.csv(
  "Data/data_day_sensores.csv",
  sep = ";"
)

# Propiedades físicas de los sensores
BD_SENSORS <- read.csv(
  "Data/data_phisyc_sensor.csv",
  sep = ";"
)

# -----------------------------------------------------------------------------
# 3. PROCESAMIENTO INICIAL
# -----------------------------------------------------------------------------

# funcion levelplot

save_levelplot_png <- function(
    plot_obj,
    filename ,
    width_in = 13.5,   # pulgadas
    height_in = 6.5,  # pulgadas
    res = 300         # dpi
) {
  
  png(
    filename = filename,
    width  = width_in  * res,
    height = height_in * res,
    res = res,
    type = "cairo"
  )
  
  print(plot_obj)
  dev.off()
}

# Convertir fecha a formato Date
df_day$Date <- as.Date(df_day$Date, format = "%d/%m/%Y")

# Seleccionar 360 dias en comun para todos los sitios
fecha_fin <- max(df_day$Date, na.rm = TRUE)
fecha_inicio <- fecha_fin - 359

# Filtrar los 360 día
df_360 <- df_day %>%
  filter(Date >= fecha_inicio & Date <= fecha_fin)

# Calcular AWC (Agua Disponible)
BD_SENSORS$AWC <- BD_SENSORS$FC - BD_SENSORS$PMP
#BD_SENSORS$AWC_50 <- BD_SENSORS$FC - (BD_SENSORS$AWC * 0.5) # cambiar para CAD al 50%

# Obtener sitios únicos y eliminar MAVALLE
sitio_unique <- unique(df_day$Site)
sitio_unique <- sitio_unique[sitio_unique != "MAVALLE"]

# Desactivar data.table para evitar conflictos
detach("package:data.table", unload = TRUE)

# Inicializar lista para almacenar resultados
state_moisture_list <- list()

# =============================================================================
# 4. PROCESAMIENTO POR SITIO
# =============================================================================

for (sn in sitio_unique) {
  
  name_sitio <- sn
  
  # ---------------------------------------------------------------------------
  # 4.1 Filtrar datos del sitio actual
  # ---------------------------------------------------------------------------
  sitio <- df_day[df_day$Site == name_sitio, ]
  
  # ---------------------------------------------------------------------------
  # 4.2 Calcular día y año juliano
  # ---------------------------------------------------------------------------
  sitio$year <- format(sitio$Date, "%Y")
  sitio$doy  <- as.numeric(format(sitio$Date, "%j"))
  
  # ---------------------------------------------------------------------------
  # 4.3 Formato horizontal de SMS por profundidad
  # ---------------------------------------------------------------------------
  sms.wide <- dcast(
    sitio,
    Site + Date ~ Depth,
    value.var = 'SMS',
    na.rm = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # 4.4 GRÁFICO G1: VWC diario por profundidad
  # ---------------------------------------------------------------------------
  
  # Paleta de colores
  cols <- colorRampPalette(
    hcl.colors(n = 100, palette = 'spectral', rev = FALSE),
    space = 'Lab',
    interpolate = 'spline'
  )
  
  # Crear gráfico
  G1 <- levelplot(
    SMS ~ doy * factor(year) | factor(Depth),
    main = paste("Daily Mean VWC (m3 m-3): ", sn),
    data = sitio,
    layout = c(1, 3),
    col.regions = cols,
    colorkey = list(space = 'top'),
    as.table = TRUE,
    par.settings = tactile.theme(),
    scales = list(
      alternating = 3,
      cex = 0.75,
      x = list(tick.number = 20)
    ),
    par.strip.text = list(cex = 0.85),
    strip = strip.custom(bg = 'grey'),
    xlab = 'Julian Day',
    ylab = 'Year'
  )
  
  # Guardar gráfico en la carpeta de resultados
  save_levelplot_png(
    plot_obj = G1,
    filename = file.path(
      carpeta_imagenes,
      paste0(sn, "_G1.png")
    )
  )
  
  # ---------------------------------------------------------------------------
  # 4.5 ESTIMACIÓN DE ESTADOS DE HUMEDAD (escala de perfil)
  # ---------------------------------------------------------------------------
  
  # Parámetros promedio del sitio (todas las profundidades)
  sitio$state_mean <- estimateSoilMoistureState(
    sitio$SMS,
    U = 0,
    sat = mean(
      BD_SENSORS[BD_SENSORS$Site == name_sitio, "SAT"][[1]] / 100,
      na.rm = TRUE
    ),
    fc = mean(
      BD_SENSORS[BD_SENSORS$Site == name_sitio, "FC"][[1]] / 100,
      na.rm = TRUE
    ),
    pwp = mean(
      BD_SENSORS[BD_SENSORS$Site == name_sitio, "PMP"][[1]] / 100,
      na.rm = TRUE
    ),
    style = 'default'
  )
  
  sitio$state_newhall <- estimateSoilMoistureState(
    sitio$SMS,
    U = 0,
    sat = mean(
      BD_SENSORS[BD_SENSORS$Site == name_sitio, "SAT"][[1]] / 100,
      na.rm = TRUE
    ),
    fc = mean(
      BD_SENSORS[BD_SENSORS$Site == name_sitio, "FC"][[1]] / 100,
      na.rm = TRUE
    ),
    pwp = mean(
      BD_SENSORS[BD_SENSORS$Site == name_sitio, "PMP"][[1]] / 100,
      na.rm = TRUE
    ),
    style = 'newhall'
  )
  
  # ---------------------------------------------------------------------------
  # 4.6 GRÁFICO G2: Estado de humedad (Newhall)
  # ---------------------------------------------------------------------------
  
  # Configurar colores
  ll <- levels(sitio$state_newhall)
  n.states <- length(ll)
  ms.colors <- hcl.colors(n = n.states, palette = 'spectral', rev = TRUE)
  
  ms.colors.f <- colorRampPalette(
    ms.colors,
    space = 'Lab',
    interpolate = 'spline'
  )
  
  suppressWarnings(
    trellis.par.set(list(
      superpose.polygon = list(col = ms.colors, border = ms.colors)
    ))
  )
  
  sK <- simpleKey(
    text = ll,
    space = 'top',
    columns = n.states,
    rectangles = TRUE,
    points = FALSE,
    cex = 1
  )
  
  # Crear gráfico
  G2 <- levelplot(
    state_newhall ~ doy * factor(year) | factor(Depth),
    main = paste("Daily Soil Moisture State mean: ", name_sitio),
    data = sitio,
    layout = c(1, 3),
    col.regions = ms.colors.f,
    key = sK,
    colorkey = FALSE,
    as.table = TRUE,
    scales = list(
      alternating = 3,
      cex = 0.75,
      x = list(tick.number = 20)
    ),
    par.strip.text = list(cex = 0.85),
    strip = strip.custom(bg = 'grey'),
    xlab = 'Julian Day',
    ylab = 'Year'
  )
  
  # Guardar gráfico en la carpeta de resultados
  save_levelplot_png(
    plot_obj = G2,
    filename = file.path(
      carpeta_imagenes,
      paste0(sn, "_G2.png")
    )
  )
  
  # ---------------------------------------------------------------------------
  # 4.7 GRÁFICO G3: Estado de humedad (default)
  # ---------------------------------------------------------------------------
  
  # Configurar colores
  ll <- levels(sitio$state_mean)
  n.states <- length(ll)
  ms.colors <- hcl.colors(n = n.states, palette = 'spectral', rev = TRUE)
  
  ms.colors.f <- colorRampPalette(
    ms.colors,
    space = 'Lab',
    interpolate = 'spline'
  )
  
  suppressWarnings(
    trellis.par.set(list(
      superpose.polygon = list(col = ms.colors, border = ms.colors)
    ))
  )
  
  sK <- simpleKey(
    text = ll,
    space = 'top',
    columns = n.states,
    rectangles = TRUE,
    points = FALSE,
    cex = 1
  )
  
  # Crear gráfico
  G3 <- levelplot(
    state_mean ~ doy * factor(year) | factor(Depth),
    main = paste("Daily Soil Moisture State mean (II): ", name_sitio),
    data = sitio,
    layout = c(1, 3),
    col.regions = ms.colors.f,
    key = sK,
    colorkey = FALSE,
    as.table = TRUE,
    scales = list(
      alternating = 3,
      cex = 0.75,
      x = list(tick.number = 20)
    ),
    par.strip.text = list(cex = 0.85),
    strip = strip.custom(bg = 'grey'),
    xlab = 'Julian Day',
    ylab = 'Year'
  )
  
  # Guardar gráfico en la carpeta de resultados
  save_levelplot_png(
    plot_obj = G3,
    filename = file.path(
      carpeta_imagenes,
      paste0(sn, "_G3.png")
    )
  )
  
  # ---------------------------------------------------------------------------
  # 4.8 ESTIMACIÓN DE ESTADOS DE HUMEDAD POR SENSOR (escala de profundidad)
  # ---------------------------------------------------------------------------
  
  # Inicializar columna
  sitio$state_p <- NA
  
  # Profundidades a procesar
  depths <- unique(sitio$Depth)
  
  # Loop por profundidad
  for (d in depths) {
    
    # Índice para la profundidad actual
    idx <- sitio$Depth == d
    
    # Saltar si no hay datos
    if (!any(idx)) next
    
    # Parámetros hidráulicos específicos por sitio y profundidad
    sat <- mean(
      BD_SENSORS$SAT[
        BD_SENSORS$Site == name_sitio &
          BD_SENSORS$Depth_s == d
      ] / 100,
      na.rm = TRUE
    )
    
    fc <- mean(
      BD_SENSORS$FC[
        BD_SENSORS$Site == name_sitio &
          BD_SENSORS$Depth_s == d
      ] / 100,
      na.rm = TRUE
    )
    
    pwp <- mean(
      BD_SENSORS$PMP[
        BD_SENSORS$Site == name_sitio &
          BD_SENSORS$Depth_s == d
      ] / 100,
      na.rm = TRUE
    )
    
    BD <- BD_SENSORS$BD[
      BD_SENSORS$Site == name_sitio &
        BD_SENSORS$Depth_s == d
    ]
    
    # Aplicar función a la profundidad actual
    sitio$state_p[idx] <- estimateSoilMoistureState(
      sitio$SMS[idx],
      U     = 0,
      sat   = sat * BD,
      fc    = fc * BD,
      pwp   = pwp * BD,
      style = "newhall"
    )
    
  }
  
  # Convertir a factor con etiquetas
  sitio$state_p_label <- factor(
    sitio$state_p,
    levels = c(1, 2, 3),
    labels = c("dry", "moist", "saturated")
  )
  
  # ---------------------------------------------------------------------------
  # 4.9 GRÁFICO G4: Estado de humedad por sensor
  # ---------------------------------------------------------------------------
  
  # Configurar colores
  ll <- levels(sitio$state_p_label)
  n.states <- length(ll)
  ms.colors <- hcl.colors(n = n.states, palette = 'spectral', rev = TRUE)
  
  ms.colors.f <- colorRampPalette(
    ms.colors,
    space = 'Lab',
    interpolate = 'spline'
  )
  
  suppressWarnings(
    trellis.par.set(list(
      superpose.polygon = list(col = ms.colors, border = ms.colors)
    ))
  )
  
  sK <- simpleKey(
    text = ll,
    space = 'top',
    columns = n.states,
    rectangles = TRUE,
    points = FALSE,
    cex = 1
  )
  
  # Crear gráfico
  G4 <- levelplot(
    state_p_label ~ doy * factor(year) | factor(Depth),
    main = paste("Daily Soil Moisture State mean by sensor: ", name_sitio),
    data = sitio,
    layout = c(1, 3),
    col.regions = ms.colors.f,
    key = sK,
    colorkey = FALSE,
    as.table = TRUE,
    scales = list(
      alternating = 3,
      cex = 0.75,
      x = list(tick.number = 20)
    ),
    par.strip.text = list(cex = 0.85),
    strip = strip.custom(bg = 'grey'),
    xlab = 'Julian Day',
    ylab = 'Year'
  )
  
  # Guardar gráfico en la carpeta de resultados
  save_levelplot_png(
    plot_obj = G4,
    filename = file.path(
      carpeta_imagenes,
      paste0(sn, "_G4.png")
    )
  )
  
  # ---------------------------------------------------------------------------
  # 4.10 Almacenar resultados
  # ---------------------------------------------------------------------------
  state_moisture_list[[name_sitio]] <- sitio
  
}

# =============================================================================
# 5. COMBINAR RESULTADOS Y GUARDAR
# =============================================================================

state_moisture <- bind_rows(state_moisture_list)

# Guardar datos procesados en la carpeta de resultados
archivo_resultados <- file.path(carpeta_resultados, "Estados_humedad_diario.csv")
write.csv(state_moisture, archivo_resultados, row.names = FALSE)

cat("\n========================================\n")
cat("PROCESO COMPLETADO\n")
cat("========================================\n")
cat("Carpeta de resultados:", carpeta_resultados, "\n")
cat("Imágenes guardadas en:", carpeta_imagenes, "\n")
cat("Archivo de datos:", archivo_resultados, "\n")
cat("========================================\n")
