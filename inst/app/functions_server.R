
plotPostProcess <- function(simulations, timestamps){
 
  # browser()
  r <-  currentRasterStack[[1]]
  # 2. Extract X and Y coordinates using the cell IDs
  coords <- terra::xyFromCell(r, simulations$ignition_cell)
 
  dd <- as.data.frame(simulations)
  # Bind the coordinates back to the data frame
  df_coords <- cbind(dd, coords)
   
  df_sf <- st_as_sf(df_coords, coords = c("x", "y"), crs = terra::crs(r)) %>%
    st_transform(4326) # Leaflet strictly requires EPSG:4326 (Lat/Lon)
  bbox <- st_bbox(df_sf)
 
  if(!is.null(timestamps)){
    df_sf$start <- sapply(timestamps, function(x){ as.character(x[[1]])}) 
    df_sf$end <- sapply(timestamps, function(x){ as.character(x[[length(x)]])}) 
    df_sf$steps <- sapply(timestamps, function(x){ length(x)}) 
  } else {
    df_sf$start <-1
    df_sf$end <- nrow(df_sf)
    df_sf$steps <- nrow(df_sf)
    
    showNotification("Please run postprocessing of this simulations - click
<span onclick='Shiny.setInputValue(\"processSimulationOutputInstance\", {force:true},  {priority: \"event\"});' class='ptr' >HERE</span>",
                      duration=20, id="postprocess")
  }
 
  leafletProxy("map") |>
    clearGroup(sim_layers$IgnitionPointsSim) |>
    # clearGroup("Simulation Output") %>% # Optional: remove previous run's markers
    addCircleMarkers(group = sim_layers$IgnitionPointsSim,
                     data = df_sf,
      radius = 12,           # The clickable area
      stroke = FALSE, 
      fillOpacity = 0,       # Makes it invisible
      options = pathOptions(pane = "ignition_points_pane"), 
      popup = ~paste0(
        "<table class='sim-popup-table'>",
        "<thead>",
        "<tr><th colspan='2' class='sim-popup-header'>Simulated Ignition n. ", simulation, "</th></tr>",
        "</thead>",
        "<tbody>", 
        "<tr><td>Start: ",  start,
        "</td><td>",
        "End: ", end,"</td></tr>",
        "<tr><td colspan='2'  >
        Current timestamp: <span id='simulationTableDateSpan", simulation, "'></span>
        </td></tr>",
        # --- NEW SLIDER ROW ---
      "<tr><td colspan='2' style='padding: 5px 10px;'>",
        "<input id='myGridSlider", simulation, "' type='range' 
      min='1' max='",steps,"' value='1' style='width:100%; cursor:pointer;' ",
          "onchange='", 
            "Shiny.setInputValue(\"playGrids\", {simulationN:", simulation, ", step: parseInt(this.value)}, {priority: \"event\"});",
          "'>",
      "<span style='cursor:pointer;font-size:14px;'
      onclick='document.getElementById(\"myGridSlider", simulation, "\").stepDown(); 
      document.getElementById(\"myGridSlider", simulation, "\").dispatchEvent(new Event(\"change\"));'>
  ◁
</span> 
<span style='cursor:pointer;font-size:14px;margin-left:10px;'
      onclick='document.getElementById(\"myGridSlider", simulation, "\").stepUp(); 
      document.getElementById(\"myGridSlider", simulation, "\").dispatchEvent(new Event(\"change\"));'>
  ▷
</span>",
        "<div style='font-size:10px; text-align:center;'>Step: <span id='val", simulation, "'>1</span></div>",
      "</td></tr>",
      # -----------------------
        
      "<tr><td colspan='2'  >Rate of Spread Grid - 
         <input type=checkbox title='Add ROS grid to map' onchange='Shiny.setInputValue(\"addROStoMAP\", {simulationN:", simulation, ", force:true},  {priority: \"event\"});' class='ptr' />
         <span title='Download Rate Of Spread raster in TIFF format' onclick='Shiny.setInputValue(\"downloadROS\", {simulationN:", simulation, ", force:true},  {priority: \"event\"});' class='ptr'>⬇️️</span>|
        <span id='simulationTableDateSpan", simulation, "'></span>
        </td></tr> ",  
      "<tr><td colspan='2'  > ", burnt_cells, "</td> </tr>", 
        "</tbody>",
        "</table>"
      )
    ) |>
   
    addLabelOnlyMarkers(group = sim_layers$IgnitionPointsSim,
                        data = df_sf,
      label = HTML('<i class="fa fa-burst" style="color:red; font-size:24px;"></i>'),
      labelOptions = labelOptions(
        noHide = TRUE, 
        direction = 'center', 
        textOnly = TRUE
      )
    )  |>
        leaflet::fitBounds( lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )   
    
}

processSimulationOutputFolder <- function(resDir){
  formatted_time <- gsub(".*_(\\d{4})(\\d{2})(\\d{2})_(\\d{2})(\\d{2})(\\d{2})", 
                         "\\1-\\2-\\3 \\4:\\5:\\6", 
                         basename(resDir))
  
  print(formatted_time)

  shinyjs::runjs("$('#logSim').empty('')")
  shinyjs::runjs(paste0("startSimlog('",formatted_time,"');") )
  ### load log ----
  fl <- list.files(resDir, pattern = "simLog.log", recursive = T, full.names = T)
 
  if(length(fl)!=1){
    print(fl)
    print(resDir)
    showNotification("Output simLog.log file not found!", type="warning", duration=19)
    return(NA)
  }
  
  rfl <- readLines(fl ) 
  session$sendCustomMessage("appendLog",  list(out=rfl))
 
  simout <-  tryCatch({
   parse_fire_log(rfl )
  }, warning=function(e){
    showNotification(paste0("Warning in parsing fire log: ", e$message), type="warning", duration=20)
    NULL
  }, error=function(e){
     showNotification(paste0("Error in parsing fire log: ", e$message), type="error", duration=20)
    NULL
  })
   
  if(!is.null(simout)){
    tryCatch({
      rdaa <- file.path(resDir, "results", "info.rda")
      if(!file.exists(rdaa)){
        timestamps <- NULL
      } else { 
        load(rdaa)
        timestamps <- weatherTimestamps
      }
      plotPostProcess(simout, timestamps)
    }, warning=function(e){
      showNotification(paste0("Warning in plotting fire log: ", e$message), type="warning", duration=20)
    }, error=function(e){
      showNotification(paste0("Error in plotting fire log: ", e$message), type="error", duration=20)
    })
  }  
 
}

killSimProcess <- function(force=F, message=""){
  browser()
  if(nchar(message)>0) message <- paste0("<br><b>", message, "</b>")
  if(exists("simProcess") && !is.null(simProcess)) simProcess$process$kill()
  updateActionButton(inputId = "runsim",label = paste("🔥 Run ", input$simulator)  )
  
  
  session$sendCustomMessage("appendLog", 
                            list( "out"=  "--- process finished either by user or error ---",
                                  "err"=  paste(message, "
--- process finished by user ---
") )
  )
   
  
  if(!is.null(simProcess$outputFolder) && nchar(dirname(simProcess$outputFolder)) > 13) {
    showNotification(paste0(
                      "Removing output  instance ", 
                     dirname(simProcess$outputFolder) 
                     ), type = "info", duration=10 )
    unlink(dirname(simProcess$outputFolder), recursive = T)
  }
   
  
  simProcess <<- NULL
  showNotification(
    paste("Simulation stopped!", message, collapse="<br>"),
    type = ifelse(nchar(message)>0, "warning", "info"),
    duration = 20
  )
}




readGrids <- function(filepath, force=F ){
  
  if( dir.exists(filepath) ){
    filepathn <- list.files(filepath, pattern = "ForestGrid\\d+.csv$", full.names = T)
  } else {
    filepathn <- filepath
  }
  filepathn <- str_sort(filepathn, numeric = TRUE)

  timesSteps <- as.integer(gsub("[^0-9]", "", basename(filepathn)) ) +1
  if(!file.exists(filepathn[[1]])){
    return((paste0(
      "File ", filepathn[[1]] , "does not exist! ERROR ")
     )
    )
  } 
  
  output_tif <- dirname(filepathn[[1]])
 
  rdapath <-  file.path(dirname(dirname(output_tif)), "info.rda")

  simNumber <- as.numeric(gsub("[^0-9]", "", basename(output_tif) ) )
  
  tifpath <- file.path(output_tif, 
            sprintf("%s.tif", 
                    basename(output_tif)   )  )
  vecpath <- file.path(output_tif, 
                       sprintf("%s.gpkg", 
                               basename(output_tif)   )  )
  

  
  if(!force && (file.exists( tifpath) &&  file.exists( vecpath) ) ) {
  
    fire_stack <- rast(tifpath)
    fire_vect <- sf::read_sf(vecpath)  
    
    if(file.exists(rdapath)){
      load(rdapath)
      # weatherTimestamps[[as.character(simNumber)]] <- terra::time(fire_stack)
    } else {
      weatherTimestamps <- list()
      weatherTimestamps[[as.character(simNumber)]] <- terra::time(fire_stack)
      save(weatherTimestamps,  file=rdapath)
    }
    return( paste0("Grids ",basename(output_tif)," already processed!"))
  } 
   
     
    ext <- terra::ext(terra.rasters[[1]])
    res <- terra::res(terra.rasters[[1]])
    crs <- terra::crs(terra.rasters[[1]])
    burnArea <- rep(0, length(filepathn))
   
    vectsList <- list()
    showNotification("Starting conversion to vector areas...", duration = 10)
    raster_list <- lapply(filepathn, function(file) {
      # fread is fast enough to handle the wide columns
      mat <- as.matrix(fread(file, header = FALSE, 
                             na.strings = "0",
                             colClasses = "integer"))
      tt <- as.integer(gsub("[^0-9]", "", basename(file))) + 1
   
      burnArea <- sum(!is.na(mat)) * mean(res)
      # Convert to raster
  
      # 3. Apply it to the raster
      r <- rast(mat, extent=ext, crs=crs  )
      # 1. Convert your terra SpatRaster to a stars object
      # r_stars <- stars::st_as_stars(r) 
      # # 2. Convert to polygons using 8-connectivity (diagonal connections allowed)
      # ff <- st_as_sf(r_stars, as_points = FALSE, merge = TRUE, connect8 = T)
      # plot(ff)
      # browser()
      # 3. If you need it back as a terra SpatVector:
      # p_smooth_terra <- vect(p_smooth)
      
      p <-as.polygons(r, dissolve = TRUE)
      
      ff <- st_as_sf(p)
      
      ff <- ff[, attr(ff, "sf_column")] 
      if (nrow(ff) == 0) {
        # 1. Create an empty geometry column (sfc) and keep the original CRS
        empty_geom <- st_sfc(st_polygon(), crs = st_crs(ff)) 
        ff <- st_sf(geometry = empty_geom)
      }
      ff$timeStep <- tt
      ff$burntArea <- burnArea
      vectsList[[as.character(tt)]] <<- ff
      return(r)
    })
    
  
    # 4. Stack them into a single Multi-band SpatRaster
    fire_stack <- rast(raster_list)
    
    fire_vect <- do.call(rbind, vectsList)
    
    names(fire_stack) <- tools::file_path_sans_ext(basename(filepathn))
    times <- getDateTimeFromCSV(file.path(input$outputInstanceFolder, "Weather.csv"))
  
    terra::time(fire_stack) <- times[timesSteps] 
    fire_vect$Date <- terra::time(fire_stack) 
    fire_vect$simNumber <- simNumber
    fire_vect$simInstance <- basename(input$outputInstanceFolder)
    
    sf::write_sf(fire_vect, vecpath, append = F)
    writeRaster(fire_stack, tifpath, datatype="INT1U", 
                overwrite = TRUE, gdal=c("COMPRESS=LZW"))
 
    if(file.exists(rdapath)){
      load(rdapath)
      weatherTimestamps[[as.character(simNumber)]] <- terra::time(fire_stack)
    } else {
      weatherTimestamps <- list()
      weatherTimestamps[[as.character(simNumber)]] <- terra::time(fire_stack)
    }
    save(weatherTimestamps,  file=rdapath)

  return("Finished processing grids")
}

 

toggleROSfile <- function(simn){
  req(input$outputInstanceFolder)
 
 
    rpath <- file.path( file.path(input$outputInstanceFolder, 
                                    "results", "RateOfSpread",
                                    paste0("ROSFile", simn, ".tif") )  )
    
    if(!file.exists(rpath)){
      showNotification(file.path("Could not read file ", basename(rpath)), 
                       type = "info", duration = 20 )
      
      return(invisible())
    }
    ros <- terra::rast(rpath)
    ros[ros==0] <- NA
    ros
 
} 

checkIgnitionFile <- function(ignfile){
  ip <- read.csv(ignfile)
 
  if(nrow(ip)!=0){ 
    ncell <- grep("ncell", names(ip), ignore.case = T )
    if(length(ncell)==0){
      showNotification(
        paste0("In file ", basename(ignfile) , "   invalid ignition points found! please check file format and remove"),
        type = "error",
        duration = 20
      )
      return(FALSE)
    }
    ## DOES NOT HAVE coordinates -> get THEM FROM Cell index IP
    if(!is.element("X", names(ip)) ){
      ncell <- ip$Ncell
      r2 <- terra::rast(rasters$FUEL)
      cr <- terra::xyFromCell(r2, ncell)
      crv <- terra::vect(cr)
      terra::crs(crv) <- terra::crs(r2) 
      crv2 <- crv |> terra::project("EPSG:4326")
      crvc <-  as.data.frame(crv2@pntr$coordinates())
      names(crvc) <- c("X","Y")
      ignitionPointsCoords(cbind(ip, crvc) )
    } else{
      ignitionPointsCoords(ip )
    } 
  }
  if(anyNA(ip$Ncell)){
    showNotification(
      paste0("In file ", ignitionFiles[[1]] , " some invalid ignition points found, please check file format."),
      type = "warning",
      duration = 15
    )
  } 
  return(TRUE)
}





parse_fire_log <- function(log_text) {
  
  # 1. Read all lines into a character vector
  lines <- log_text
  
  # 2. Find the row indices (line numbers) for the data we want
  sim_indices   <- grep("Simulation \\d+ Start:", lines)
  weatherFiles <-  trimws(sub(".*weather file:\\s*", "", lines[sim_indices+1]))
  ignitionsN <- as.integer(gsub("\\D+", "", lines[sim_indices]))
  
  Map(getDateTimeFromCSV, weatherFiles)
  
  if(anyNA(ignitionsN)){
    stop(errorCondition("We have NAs in simulation start ignition iterations!"))
  }
  
  ign_cells   <- trimws(gsub("ignition cell: ", "", lines[sim_indices+2]))
  
  burnt_indices <- grep("^Simulation\\s+\\d+\\s+Results:", lines)
  burnt_ignitionsN <- as.numeric(str_extract(lines[burnt_indices ], "\\d+"))
  burnt_indicesClean <- burnt_indices[which(!duplicated(burnt_ignitionsN))]
  burnt_ignitionsNClean <- burnt_ignitionsN[which(!duplicated(burnt_ignitionsN))]
  
  burnt_tables <- Map(function(x){  
    x <- which(burnt_ignitionsNClean==x)
    indices <- (burnt_indicesClean[[x]]+3) : (burnt_indices[[x]]+7)
    data_lines <- lines[ indices ]
    data_lines <- gsub("^\t", "", data_lines)
    data_lines_csv <- gsub(" {2,}", ",", data_lines)
    df <- read.csv(text = data_lines_csv, header = FALSE)
    colnames(df) <- c("Cell Status", "Count", "Percent")
    
    kable(df, format = "html", table.attr = "class='table table-striped table-condensed table-sm'")
    
  },
  ignitionsN 
  )
  # df <- data.frame(simulation = sim_indices,
  #                  ignitionsN = ignitionsN,
  #                  ign_cells=ign_cells)
  # 4. Bind them into a data frame
  
  dd <- list(
    simulation = as.integer(ignitionsN),
    ignition_cell = as.integer(ign_cells),
    burnt_cells = as.character(burnt_tables)
  )
  
  if(length(unique(sapply(dd, length)))!=1 || length(dd$simulation)==0){
    showNotification(
      paste0(
        "
<br>Number of simulations: ", length(dd$simulation) ,". ",
        "
<br>Number of ignition cells: ", length(dd$ignition_cell), ". ", 
        "
<br>Number of burnt cells values: ", length(dd$burnt_cells), ". <br>"
      ), type="warning", duration=19 )
  }
  
  dd
}