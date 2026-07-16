#' Esporta un DataFrame Meteo nel Formato RAWS (.wxs)
#'
#' @param df Un data.frame contenente i dati meteorologici dell'app. Deve contenere una colonna `datetime` 
#'           (oppure colonne separate year, month, day, hour) e le variabili ws, wd, temp, rh, prec.
#' @param output_path Stringa. Il percorso completo in cui salvare il file (es. "data/output_weather.wxs").
#' @param elevation Numerico. L'elevazione della stazione RAWS (default: 2).
#' @param units Stringa. Le unità di misura, tipicamente "Metric" o "English" (default: "Metric").
#'
#' @return Nessun valore di ritorno. Scrive direttamente il file sul disco.
export_weather_to_wxs <- function(df, output_path, elevation = 2, units = "Metric") {
  
  # 1. Normalizzazione dei nomi delle colonne per sicurezza
  colnames(df) <- tolower(trimws(colnames(df)))
  
  # 2. Se i dati usano la colonna unica 'datetime', estraiamo i singoli componenti
  if ("datetime" %in% colnames(df) && !all(c("year", "month", "day", "hour") %in% colnames(df))) {
    parsed_dates <- suppressWarnings(anytime::anytime(df$datetime, asUTC = FALSE))
    df$year  <- as.integer(format(parsed_dates, "%Y"))
    df$month <- as.integer(format(parsed_dates, "%m"))
    df$day   <- as.integer(format(parsed_dates, "%d"))
    df$hour  <- as.integer(format(parsed_dates, "%H"))
  }
  
  # 3. Mappatura e Fallback delle variabili meteorologiche necessarie per il formato RAWS
  # Se mancano o hanno nomi leggermente diversi (es. TMP invece di TEMP) le allineiamo
  df <- df %>% 
    dplyr::rename_with(~dplyr::case_when(
      . %in% c("temperature", "temp_c", "tmp") ~ "temp",
      . %in% c("relativehumidity", "humidity", "rhum") ~ "rh",
      . %in% c("windspeed", "wind_speed", "wspeed") ~ "ws",
      . %in% c("winddirection", "wind_dir", "wdir") ~ "wd",
      . %in% c("precipitation", "precip", "rain") ~ "prec",
      TRUE ~ .
    ))
  
  # Controllo variabili meteorologiche obbligatorie con valori di default se assenti
  required_cols <- list(temp = 20, rh = 50, prec = 0.0, ws = 0, wd = 0, cloudcov = 0)
  for (col in names(required_cols)) {
    if (!col %in% colnames(df)) {
      df[[col]] <- required_cols[[col]]
    } else {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      df[[col]][is.na(df[[col]])] <- required_cols[[col]]
    }
  }
  
  # 4. Formattazione specifica per il file .wxs
  # - L'ora deve essere espressa a 4 cifre (es. 8 -> "0800", 13 -> "1300")
  # - La precipitazione oraria (HrlyPcp) richiede 2 cifre decimali fisse (es. "0.00")
  df_formatted <- df %>%
    dplyr::transmute(
      Year = as.integer(year),
      Mth  = as.integer(month),
      Day  = as.integer(day),
      Time = sprintf("%02d00", as.integer(hour)),
      Temp = round(temp),
      RH   = round(rh),
      HrlyPcp = sprintf("%.2f", prec),
      WindSpd = round(ws),
      WindDir = round(wd),
      CloudCov = as.integer(cloudcov)
    )
  
  # 5. Scrittura del File con Intestazioni e Metadati RAWS
  # Usiamo un file connection per scrivere riga per riga l'header personalizzato
  con <- file(output_path, "w", encoding = "UTF-8")
  
  tryCatch({
    # Scrittura dei metadati RAWS richiesti in testa al file
    writeLines(paste0("RAWS_UNITS: ", units), con)
    writeLines(paste0("RAWS_ELEVATION: ", elevation), con)
    writeLines(paste0("RAWS: ", nrow(df_formatted)), con)
    
    # Scrittura dell'intestazione delle colonne separata da spazi tabulati
    header_line <- "Year\tMth\tDay\tTime\tTemp\tRH\tHrlyPcp\tWindSpd\tWindDir\tCloudCov"
    writeLines(header_line, con)
    
    # Scrittura delle righe dati usando la tabulazione (\t) come separatore di colonna
    for (i in 1:nrow(df_formatted)) {
      data_line <- paste(df_formatted[i, ], collapse = "\t")
      writeLines(data_line, con)
    }
    
    message("File .wxs generato con successo in: ", output_path)
  }, finally = {
    # Chiusura sicura della connessione del file
    close(con)
  })
}


getDateTimeFromCSV<-function(x){
 
  if(!is.data.frame(x)){
    x <- read.csv(x ) 
  }
 
  x <- na.omit(x)
  header <- names(x) 
 
  datecol <- grep("date", header, ignore.case = TRUE) # Case-insensitive match
  
  if(length(datecol)==1){
    datecol <- datecol[1]  
    # Read just that column to save memory/time
    wf_col <- x[, datecol]
    clean_ts <- anytime::anytime(wf_col, asUTC = FALSE)
    
    # 3. FALLBACK: If anytime fails (e.g., 23/04/2026), hunt by explicit order
    if (anyNA(clean_ts)) {
      # Try Day-Month-Year variants (Common international format)
      clean_ts <- lubridate::dmy_hms(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::dmy_hm(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::dmy(wf_col, quiet = TRUE)
      
      # Try Year-Month-Day variants (ISO format)
      if (anyNA(clean_ts)) clean_ts <- lubridate::ymd_hms(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::ymd_hm(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::ymd(wf_col, quiet = TRUE)
      
      # Try Month-Day-Year variants (US format)
      if (anyNA(clean_ts)) clean_ts <- lubridate::mdy_hms(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::mdy_hm(wf_col, quiet = TRUE)
      if (anyNA(clean_ts)) clean_ts <- lubridate::mdy(wf_col, quiet = TRUE)
    }
      
    if(anyNA(clean_ts)) {
      showNotification(paste0(
        "Sorry, we did find a column named '",
        header[[datecol]] ,"' BUT we could not convert 
          its contents to a time stamp using various heuristics.
  <br> The following timestamp format is advised and recognized:
  <br>2023-07-07 16:00:00
  <br>2023-07-07 16:00
  <br>2023-07-07 16 
<br>We gave a generic 1 hour time lapse for each line."), duration=15 , id="datecolumnMixMatch"    
      )
      
      times <- 1:(nrow(wf)+2)
      
    } else {
      if(hour(clean_ts[[1]])==0){
        clean_ts <- clean_ts+3600*12
      }
      times <- c(clean_ts[[1]]-3600, 
                 clean_ts, 
                 clean_ts[[length(clean_ts)]]+3600  ) 
      
    }
    
  } else { 
    showNotification(paste0(
      "Sorry, we did NOT find a column with name 'date'.
<br>We gave a generic 1 hour time lapse for each line."), duration=15 , id="datecolumnMixMatch"    
    )
    times <- 1:(nrow(wf)+2)
  }
  times
  
}

#' center
#' @description
#' Reads raster and get center of raster
#' 
#'
#' @param r raster in terra format
#'
#' @returns
#' @export
#'
#' @examples
center <- function(r){
  e <- terra::ext(r)
 
  list(
    lng = (e$xmin + e$xmax) / 2,
    lat = (e$ymin + e$ymax) / 2
  )
}
add_suffix <- function(x, suffix) {
  sub("(\\.[^.]+)$", paste0(suffix, "\\1"), x)
}
is_all_caps <- function(x) {
  x == toupper(x)
}
popup_text <- function(name, value=NULL) {
  pp<-paste0("<b>", name, "</b>")
  if(!is.null(value)) pp<-paste0("<b>", name, ":</b> ", value)
  pp
}

checkAPI <- function(url){ 
  res <- httr::GET(url) 
  httr::status_code(res)
}


runForeFire <- function(outfolder){
  library(viridisLite)
  wdc <- getwd()
  outfolder<-"inst/app/data/TC03_CZ_wildfire/output"  
  setwd(outfolder)
  
  # Define parameter values
  params <- list(
    perimeterResolution = c(1,2,3),
    spatialIncrement = c(0.2,0.5,1),
    propagationSpeedAdjustmentFactor = c(0.05, 0.10),
    windReductionFactor = c(0.1, 0.2, 0.3, 0.4)
  )
  
  
  library(glue)
  # Create all combinations
  params <- expand.grid(params, KEEP.OUT.ATTRS = FALSE)
  wdcPreprocessing <- getwd()
  times <- list()
  
  dt2f <- terra::rast("../fuel.tif")
  for(i in seq_len(nrow(params))) { 
    setwd(wdcPreprocessing)
    outdir <- sprintf("runs/run_%03d", i)
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    print(p$perimeterResolution)
    # Output directory for this run
    setwd(outdir)
    p <- params[i,]
    # Create params.ff
    writeLines(
      c(
        glue("setParameter[perimeterResolution={p$perimeterResolution}]"),
        glue("setParameter[spatialIncrement={p$spatialIncrement}]"),
        glue("setParameter[propagationSpeedAdjustmentFactor={p$propagationSpeedAdjustmentFactor}]"),
        glue("setParameter[windReductionFactor={p$windReductionFactor}]")
      ),
      "params.ff"
    )
    
    # lines <- readLines("../../run.ff")
    nm <- sprintf("%04d_pR%04d_sInc%04d_pSAF%04d_wRF%04d",
            i, p$perimeterResolution*100,
            p$spatialIncrement*100,
            p$propagationSpeedAdjustmentFactor*100,
            p$windReductionFactor*100)
    # lines[15] <-  sprintf("plot[parameter=speed;filename=../%04d_pR%04d_sInc%04d_pSAF%04d_wRF%04d.png;opt:area=active;opt:cmap=turbo]",
    #                       i, p$perimeterResolution*100,
    #                       p$spatialIncrement*100,
    #                       p$propagationSpeedAdjustmentFactor*100,
    #                       p$windReductionFactor*100)
    # 
    # writeLines(lines, "../../run.ff")
    # Run ForeFire
    st <- system.time({ 
      system2(
          "/archivio/software/forefire/bin/forefire",
          args = c(
            "-i", "../../run.ff"
            # add your other arguments here
          )
        )
    })
    fn <- terra::rast("ForeFire.0.nc")
    fn[fn< -10]<-NA
    fn <- fn/60
    terra::ext(fn) <- terra::ext(dt2f)
    fn <- flip(fn, direction = "vertical")
    crs(fn) <- crs(dt2f)
    js <- sf::read_sf("final_front01h.json") |> sf::st_transform(terra::crs(fn))
    
    library(ggplot2)
    library(tidyterra)
    

    png(sprintf("../outputs/%s.png",nm), width = 1000, height = 1000, res=200)
    p <- ggplot() +
      geom_spatraster(data = fn) +
      scale_fill_gradientn(
        colours = turbo(256),
        na.value = "transparent",
        name = "Fire arrival \ntime (minutes)"
      ) +
      labs(
        title = "Fire simulation",
        subtitle = sprintf("runtime=%.2f pRes.=%.1f, spatInc=%.1f,
propSpeedAdjF=%.1f, windRedF.=%.1f", st[[3]], p$perimeterResolution,
                           p$spatialIncrement,
                           p$propagationSpeedAdjustmentFactor,
                           p$windReductionFactor),
        x = "Longitude",
        y = "Latitude"
      ) + coord_sf(  ) +   
      theme_bw()
    print(p)
    dev.off()
    times[[nm]]<- st[[3]]
  }
  
  params$processingTime <- unlist(unname(times))
  
  # ext(dt2) <- ext(dt2f) 
  # crs(dt2) <- crs(dt2f)

  system("/archivio/software/forefire/bin/forefire -i run.ff")
  
  # 1. Elenca tutti i file GeoJSON nella cartella
  geojson_files <- list.files( 
                              pattern = "\\.json$", 
                              full.names = TRUE)
  
  # 2. Definisci il percorso del GeoPackage di output
  gpkg_output <- "output_aggregato.gpkg"
  
  layer_unico <- map(geojson_files, function(file) {
    df <- st_read(file, quiet = TRUE)
    # Forza tutte le geometrie a POLYGON/MULTIPOLYGON per sicurezza
    df <- st_cast(df, "MULTIPOLYGON") 
    # Uniforma il CRS a WGS84 (modifica 4326 se usi un altro CRS, es. 3003 o 32632)
    df <- st_transform(df, 4326)
    return(df)
  }) %>% 
    list_rbind() %>%  
    st_as_sf()     # Unisce i passaggi in un unico dataframe
  # plot(dt2$`fuel_ft=1_fz=1`)
  library(sf)
  library(terra)
 
  js <- sf::read_sf(geojson_files[[length(geojson_files)]]) |> sf::st_transform(crs(dt2f))
  plot(  js, col="#ff000044" )
  # js <- sf::read_sf("final_front11h.json") |> sf::st_transform(crs(dt2f))
  # plot(  js, col="#ff000044", add=T )
  # plot(  js, col="#ff000044"  )
  file_output <- "output_poligoni.gpkg"
  if (file.exists(file_output)) file.remove(file_output)
  
  st_write(layer_unico, dsn = file_output, layer = "tutti_i_poligoni")
  setwd(wdc)
}
 
createNetCDF4ForeFire <- function(fuel_raster,  elev_raster, output_file){
  # 
  # fuel_raster<-terra::rast("../fuel.tif")
  # elev_raster<-terra::rast("../dem.tif")
  # output_file <- "ForeFireLandscape.nc"
  
  size_fx <- ncol(fuel_raster); size_fy <- nrow(fuel_raster)
  size_nx <- ncol(elev_raster); size_ny <- nrow(elev_raster)
  
 

  # 1. Convert the raster's extent into a spatial polygon using its current CRS
  poly <- as.polygons(terra::ext(fuel_raster), crs=crs(fuel_raster)) 
  poly_latlon <- project(poly, "EPSG:4326") 
  ext_latlon <- ext(poly_latlon) 
  bounds <- as.vector(ext_latlon)
  
  west  <- bounds["xmin"]
  south <- bounds["ymin"]
  east  <- bounds["xmax"]
  north <- bounds["ymax"]
   
  # -------------------------------------------------------------
  # 2. DEFINE THE 12 TARGET DIMENSIONS (WITHOUT DIMVARS)
  # -------------------------------------------------------------
  dim_ft <- ncdim_def("ft", "", 1:1, create_dimvar=FALSE)
  dim_fz <- ncdim_def("fz", "", 1:1, create_dimvar=FALSE)
  dim_fy <- ncdim_def("fy", "", 1:size_fy, create_dimvar=FALSE)
  dim_fx <- ncdim_def("fx", "", 1:size_fx, create_dimvar=FALSE)
  
  # dim_wdim <- ncdim_def("wind_dimensions", "", 1:2, create_dimvar=FALSE)
  # dim_wdir <- ncdim_def("wind_directions", "", 1:2, create_dimvar=FALSE)
  # dim_wrows <- ncdim_def("wind_rows", "", 1:160, create_dimvar=FALSE)
  # dim_wcols <- ncdim_def("wind_columns", "", 1:160, create_dimvar=FALSE)
  
  dim_nt <- ncdim_def("nt", "", 1:1, create_dimvar=FALSE)
  dim_nz <- ncdim_def("nz", "", 1:1, create_dimvar=FALSE)
  dim_ny <- ncdim_def("ny", "", 1:size_ny, create_dimvar=FALSE)
  dim_nx <- ncdim_def("nx", "", 1:size_nx, create_dimvar=FALSE)
  
  # -------------------------------------------------------------
  # 3. DEFINE THE 4 VARIABLES WITH EXACT PRECISION & CHUNKING
  # -------------------------------------------------------------
  # A. domain variable (Scalar string/character array)
  # dimnchar <- ncdim_def("nchar", "", 1:254, create_dimvar=FALSE )
  var_domain <- ncvar_def("domain", "", list() )
  
  # 1. DEFINE VARIABLES WITH REVERSED DIMENSION LISTS IN R
  var_fuel <- ncvar_def(
    name = "fuel", 
    units = "", 
    dim = list(dim_fx, dim_fy, dim_fz, dim_ft), # <--- REVERSED HERE
    prec = "short",
    shuffle = TRUE, 
    compression = 5
  )
  
  var_altitude <- ncvar_def(
    name = "altitude", 
    units = "", 
    dim = list(dim_nx, dim_ny, dim_nz, dim_nt), # <--- REVERSED HERE
    prec = "short",
    shuffle = TRUE, 
    compression = 5
  )
  
  # C. wind variable: float[wind_columns, wind_rows, wind_directions, wind_dimensions]
  # var_wind <- ncvar_def(
  #   name = "wind",
  #   units = "",
  #   dim = list(dim_wcols, dim_wrows, dim_wdir, dim_wdim),
  #   prec = "float",
  #   missval = NaN,
  #   chunksizes = c(160, 160, 2, 2),
  #   shuffle = TRUE,
  #   compression = 5
  # )
 
  # -------------------------------------------------------------
  # 4. CREATE FILE AND WRITE ATTRIBUTES & ARRAYS
  # -------------------------------------------------------------
  nc <- nc_create(output_file, vars=list(var_domain, 
                                         var_fuel, 
                                          # var_wind,
                                         var_altitude), force_v4=TRUE)
 
  # --- Write Domain Attributes ---
  ncatt_put(nc, "domain", "SWx", 0, prec="float")
  ncatt_put(nc, "domain", "SWy", 0, prec="float")
  ncatt_put(nc, "domain", "Lx",   size_fx , prec="float")
  ncatt_put(nc, "domain", "Ly", size_fy , prec="float")
  
  bbox_str <- sprintf("%.16f,%.16f,%.16f,%.16f", west, south, east, north)
  ncatt_put(nc, "domain", "BBoxWSEN", bbox_str, prec="text")
 
  ncatt_put(nc, "domain", "Lz", 0, prec="float")
  ncatt_put(nc, "domain", "t0", 0, prec="float") 
  ncatt_put(nc, "domain", "Lt", float::Machine_float$float.xmax, prec = "float") 
 
  ncatt_put(nc, "domain", "SWz", 0, prec="float")
  ncatt_put(nc, "domain", "type", "domain", prec="text")
  
  wsenlbrt_str <- sprintf("%.16f,%.16f,%.16f,%.16f,0.0,0.0,%d.0,%d.0", 
                          west, south, east, north, size_fx, size_fy)
  ncatt_put(nc, "domain", "WSENLBRT", wsenlbrt_str, prec="text")
  
  # --- Write Type Metadata Attributes ---
  ncatt_put(nc, "fuel", "type", "fuel", prec="text")
  # ncatt_put(nc, "wind", "type", "wind", prec="text")
  ncatt_put(nc, "altitude", "type", "data", prec="text")
  
  # --- Write Data Payloads ---
  # --- 1. Process Fuel Data ---
  fuel_mat <- as.matrix(fuel_raster, wide=TRUE)
  fuel_mat[is.na(fuel_mat)] <- 99
  
  # Flip the rows if needed for your spatial orientation
  fuel_mat <- fuel_mat[nrow(fuel_mat):1, ] 
  
  # Wrap natively into R array matching [Y, X, Z, T] mapping
  # Indices: 1 = Y (size_fy), 2 = X (size_fx), 3 = Z (1), 4 = T (1)
  fuel_array <- array(fuel_mat, dim=c(size_fy, size_fx, 1, 1))
  
  # We want the data ordered to match our R dim list: list(dim_fx, dim_fy, dim_fz, dim_ft)
  # This means: X (2), Y (1), Z (3), T (4)
  fuel_ready <- aperm(fuel_array, c(2, 1, 3, 4))
  # fuel_ready[] <- 104
  ncvar_put(nc, var_fuel, fuel_ready)
  
  
  # --- 2. Process Altitude Data ---
  elev_mat <- as.matrix(elev_raster, wide=TRUE)
  elev_mat[is.na(elev_mat)] <- mean(elev_mat, na.rm=TRUE)
  elev_mat <- elev_mat[nrow(elev_mat):1, ]
  
  elev_array <- array(elev_mat, dim=c(size_ny, size_nx, 1, 1))
  
  # Match our R dim list: list(dim_nx, dim_ny, dim_nz, dim_nt)
  # This means: X (2), Y (1), Z (3), T (4)
  altitude_ready <- aperm(elev_array, c(2, 1, 3, 4))
  # altitude_ready[]<-100
  ncvar_put(nc, var_altitude, altitude_ready)
  # -------------------------------------------------------------
  # 5. CLOSE FILE
  # -------------------------------------------------------------
  nc_close(nc)
  print("Target NetCDF file matching specifications built successfully!")
}
