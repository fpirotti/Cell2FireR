
plotPostProcess <- function(simulations){
 
  r <-  currentRasterStack[[1]]
  # 2. Extract X and Y coordinates using the cell IDs
  coords <- terra::xyFromCell(r, simulations$ignition_cell)
 
  # Bind the coordinates back to the data frame
  df_coords <- cbind(simulations, coords)
   
  df_sf <- st_as_sf(df_coords, coords = c("x", "y"), crs = terra::crs(r)) %>%
    st_transform(4326) # Leaflet strictly requires EPSG:4326 (Lat/Lon)
  bbox <- st_bbox(df_sf)
 
  # 4. Add to the existing Leaflet map
  # Replace "fireMap" with the actual ID of your leafletOutput in the UI
  leafletProxy("map") |>
    clearGroup(sim_layers[[2]]) |>
    addMapPane(name = "ignition_points_pane", zIndex = 650) |>
    # clearGroup("Simulation Output") %>% # Optional: remove previous run's markers
    addCircleMarkers(group = sim_layers[[2]],
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
        "<tr>",
        "<td class='sim-popup-label'><span>DRAW Map</span></td>",
        "<td class='sim-popup-value'><i onclick='Shiny.setInputValue(\"readGrids\", ", simulation, ",  {priority: \"event\"});' class='fas fa-file-export' role='presentation' aria-label='file-export icon'></i></td>",
        "</tr>",
        "<tr>",
        "<td class='sim-popup-label'>Cell ID</td>",
        "<td class='sim-popup-value'>", ignition_cell, "</td>",
        "</tr>",
        "<tr>",
        "<td class='sim-popup-label'>N of Burnt cells</td>",
        "<td class='sim-popup-value'>", burnt_cells, "</td>",
        "</tr>",
        "</tbody>",
        "</table>"
      )
    ) |>
   
    addLabelOnlyMarkers(group = "Simulation Ignition Points",
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
      plotPostProcess(simout)
    }, warning=function(e){
      browser()
      showNotification(paste0("Warning in plotting fire log: ", e$message), type="warning", duration=20)
    }, error=function(e){
      browser()
      showNotification(paste0("Error in plotting fire log: ", e$message), type="error", duration=20)
    })
  }  
 
}

killSimProcess <- function(force=F, message=""){
  if(nchar(message)>0) message <- paste0("<br><b>", message, "</b>")
  simProcess$process$kill()
  updateActionButton(inputId = "runsim",label = paste("🔥 Run ", input$simulator)  )
  
  
  session$sendCustomMessage("appendLog", 
                            list( "out"=  "--- process finished by user ---",
                                  "err"=  paste(message, "
--- process finished by user ---
") )
  )
  if(nchar(dirname(simProcess$outputFolder)) > 13) unlink(dirname(simProcess$outputFolder), recursive = T)
  simProcess <<- NULL
  showNotification(
    paste("Simulation stopped!", message, collapse="<br>"),
    type = ifelse(nchar(message)>0, "warning", "info"),
    duration = 20
  )
}




readGrids <- function(filepath){
  
  if( dir.exists(filepath) ){
    filepathn <- list.files(filepath, pattern = "ForestGrid\\d+.csv$", full.names = T)
  } else {
    filepathn <- filepath
  }
  if(!file.exists(filepathn[[1]])){
    stop(errorCondition(paste0(
      "File ", filepathn[[1]] , "does not exist ")
     )
    )
  } 
  
  output_tif <- dirname(filepathn[[1]])
  tifpath <- file.path(output_tif, 
            sprintf("%s.tif", 
                    basename(output_tif)   )  )
  vecpath <- file.path(output_tif, 
                       sprintf("%s.gpkg", 
                               basename(output_tif)   )  )
  

  
  if(file.exists( tifpath )  ) {
    message("file exists")
    fire_stack <- rast(tifpath)
    fire_vect <- sf::read_sf(vecpath)
    # Put the raster into the reactive container
    current_frame(1) # Reset frame
   
    load(file=file.path(output_tif, "aStats.rda") ) 
    # return(burnArea)
  } else {
     
    ext <- terra::ext(terra.rasters[[1]])
    crs <- terra::crs(terra.rasters[[1]])
    burnArea <- rep(0, length(filepathn))
    showNotification("Reading all output grids, might take a while", 
                     type = "warning", duration = 20, id = "notification")
    raster_list <- lapply(filepathn, function(file) {
      # fread is fast enough to handle the wide columns
      mat <- as.matrix(fread(file, header = FALSE, 
                             na.strings = "0",
                             colClasses = "integer"))
      tt <- as.numeric(gsub("[^0-9]", "", basename(file))) + 1
   
      burnArea[[tt]] <<- sum(!is.na(mat))
      # Convert to raster
  
      # 3. Apply it to the raster
      r <- rast(mat, extent=ext, crs=crs  )
   
      return(r)
    })
    
  
    removeNotification(id = "notification")
    shinyWidgets::closeSweetAlert()
    showNotification(paste0("Done reading ",  length(filepathn) , " GRIDS for simulation n.",
                            as.numeric(gsub("[^0-9]", "", basename(output_tif) ))  ), 
                     type = "warning", duration = 5)
  
    # 4. Stack them into a single Multi-band SpatRaster
    fire_stack <- rast(raster_list)
    names(fire_stack) <- tools::file_path_sans_ext(basename(filepathn))
  
    
    wf <- read.csv(
      file.path(input$outputInstanceFolder, "Weather.csv")
      )
    datecol <- grep("date", names(wf))
    
    if(length(datecol)==1){
      
      raw_ts <- gsub("[^0-9]", "", wf[,datecol])  
      clean_ts <- as.POSIXct(raw_ts, format = "%Y%m%d%H%M%S")
      if(anyNA(clean_ts)) clean_ts <- as.POSIXct(raw_ts, format = "%Y%m%d%H%M")
      if(anyNA(clean_ts)) clean_ts <- as.POSIXct(raw_ts, format = "%Y%m%d%H")
      if(anyNA(clean_ts)) clean_ts <- as.POSIXct(raw_ts, format = "%Y%m%d")
      if(anyNA(clean_ts)) {
        showNotification(paste0(
          "Sorry, we did find a column named '",
                         names(wf)[[datecol]] ,"' but we could not convert 
          its contents to a time stamp using our heuristics.
  <br> The following timestamps formats are recognized:
  <br>2023-07-07 16:00:00
  <br>2023-07-07 16:00
  <br>2023-07-07 16
  <br>2023-07-07") 
          )
      }
      terra::time(fire_stack) <- 1:length(filepathn)
    } else {
      td <-diff(clean_ts[1:2])
      terra::time(fire_stack) <- c(clean_ts[[1]]-td, 
                                   clean_ts, 
                                   clean_ts[[length(clean_ts)]]+td
      )
    }
    
    save(burnArea, file=file.path(output_tif, "aStats.rda") )
    
    writeRaster(fire_stack, tifpath, datatype="INT1U", 
                overwrite = TRUE, gdal=c("COMPRESS=LZW"))
  }
  
  if(!file.exists( vecpath )  ) {
    vector_list <- sapply(fire_stack, function(firlayer){
      p <-as.polygons(firlayer, dissolve = TRUE)
      if (nrow(p) == 0) return(NULL) 
      ff <- st_as_sf(p)
      ff <- ff[, attr(ff, "sf_column")] 
      ff$iteration <- names(firlayer) 
      return(ff)
    })
    fire_vect <- do.call(rbind, Filter(Negate(is.null), vector_list))
    
    sf::write_sf(fire_vect, vecpath, append = F)
  }
  fire_vect <- sf::read_sf(vecpath) |> sf::st_transform(4326)
  
  if(!is.null(isolate(fire_raster()))) fire_raster(NULL)
  else fire_raster( fire_vect) 
  
  return(burnArea)
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