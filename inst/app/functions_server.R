
plotPostProcess <- function(simulations){
 
  r <-  currentRasterStack[[1]]
  # 2. Extract X and Y coordinates using the cell IDs
  coords <- terra::xyFromCell(r, simulations$ignition_cell)
 
  # Bind the coordinates back to the data frame
  df_coords <- cbind(simulations, coords)
   
  df_sf <- st_as_sf(df_coords, coords = c("x", "y"), crs = terra::crs(r)) %>%
    st_transform(4326) # Leaflet strictly requires EPSG:4326 (Lat/Lon)
  bbox <- st_bbox(df_sf)
  print(bbox)
 
  # 4. Add to the existing Leaflet map
  # Replace "fireMap" with the actual ID of your leafletOutput in the UI
  leafletProxy("map") |>
    clearGroup("Simulation Ignition Points") |>
    addMapPane(name = "ignition_points_pane", zIndex = 650) |>
    # clearGroup("Simulation Output") %>% # Optional: remove previous run's markers
    addCircleMarkers(group = "Simulation Ignition Points",
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
        "<td class='sim-popup-label'>Cell</td>",
        "<td class='sim-popup-value'>", ignition_cell, "</td>",
        "</tr>",
        "<tr>",
        "<td class='sim-popup-label'>Burnt cells</td>",
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
  tryCatch({
    simout <-  parse_fire_log(rfl )
    plotPostProcess(simout)
  }, warning=function(e){
    showNotification(e$message, type="warning", duration=20)
  }, error=function(e){
     showNotification(e$message, type="error", duration=20)
  })
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