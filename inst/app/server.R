# server.R

server <- function(input, output, session) {
  ## AUTH ------
  # source("functions_auth.R", local=T)
  ## LOAD STATE ------
  # source("../../R/run_cell2fire.R")
  source("functions_state.R", local = T)
  source("functions_server.R", local = T)
  uniqueId <- paste0(format(Sys.time(), "%H%M%S"), substr(session$token, 1, 6))
  outfolder <- NULL
  
  options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB
  # rasterInfo <- reactiveVal(NULL)
  weatherDataTable <- reactiveVal(
    data.frame(
      Instance = "",
      datetime = "",
      TMP = NA_real_,
      WS = NA_real_,
      WD =  NA_real_,
      RH =  NA_real_,
      FFMC =  NA_real_,
      DMC =  NA_real_,
      DC =  NA_real_,
      ISI =  NA_real_,
      BUI =  NA_real_,
      FWI =  NA_real_,
      FireScenario = ""
    )
  )
  currentRasterStack <- NULL
  lut_fbp_local <- reactiveVal(NULL)
  logs <- list(
    logfile = file.path(
      this.path::this.dir(),
      "sessionLogs",
      paste0(uniqueId, "Logfile.log")
    ) ,
    logfileLn = 0
  )
  
  
  logs$log_con = file(logs$logfile, "a")
  
  ## dummy start/stop process
  simProcess <- NULL
  # all_input_names <- unlist(lapply(PANELS, names), use.names = FALSE)
  # lapply(all_input_names, function(inp) {
  #   input[[inp]]
  # })
  weatherFiles <- NULL
  rasters <- list()
  terra.rasters <- list()
  ignitionPointsCoords <- reactiveVal(NULL)
  
  simulation_output_grids <- reactiveVal(NULL)
  current_frame <- reactiveVal(0)
  
  rasterFiles <- reactiveVal(NULL)
  csvFiles <- reactiveVal(NULL)
  weatherFiles <- reactiveVal(NULL)
  ignitionFiles <- reactiveVal(NULL)
  lut_generic <- lut_fbp
  
  
  ## SIM OUTPUT Polling ----
  observe({
    invalidateLater(1000)
    
    if (is.null(simProcess))
      return()
    
    if (!file.exists(file.path(simProcess$outputFolder, "simLog.log"))) {
      cat(
        "============= COMMAND LINE ==============\n",
        paste(c(
          simProcess$command, simProcess$args
        ), collapse = " ") ,
        "\n=========================================\n\n",
        file = file.path(simProcess$outputFolder, "simLog.log"),
        append = T
      )
    }
    # 1. Smarter safe read functions
    # These check if the connection is actually valid BEFORE reading
    read_out_safe <- function(proc) {
      if (proc$has_output_connection()) {
        res <- tryCatch({
          proc$read_output_lines(n = -1)
        }, error = function(e) {
          showNotification(paste0("Error in reading simulation output: ", e$message))
          NULL
        }, warning = function(e) {
          print("read out safe")
          showNotification(paste0("Warning in reading simulation output: ", e$message))
          NULL
        })
        if (is.null(res))
          return(character(0))
        else
          return(res)
      }
      return(character(0))
    }
    
    read_err_safe <- function(simProcess) {
      if (simProcess$has_error_connection() &&
          simProcess$is_incomplete_error()) {
        res <- tryCatch(
          simProcess$read_error_lines(),
          error = function(e)
            NULL
        )
        if (is.null(res))
          return(character(0))
        else
          return(res)
      }
      return(character(0))
    }
    
    
    # Read standard out and standard error safely
    outl <- read_out_safe(simProcess$process)
    errl <- read_err_safe(simProcess$process)
    
    # Send payload if there's anything to send
    payload <- list(out = outl, err = errl)
    if (length(outl) > 0)
      cat(
        paste(outl, collapse = "\n") ,
        "\n",
        file = file.path(simProcess$outputFolder, "simLog.log"),
        append = T
      )
    if (length(errl) > 0)
      cat(
        paste(errl, collapse = "\n") ,
        "\n",
        file = file.path(simProcess$outputFolder, "simErr.log"),
        append = T
      )
    if (length(payload$err) > 0 ||
        length(payload$out) > 0)
      session$sendCustomMessage("appendLog", payload)
    
    realError <- which(grepl("^Error:", errl))
    
    if (length(realError) > 0) {
      killSimProcess(T, errl[realError])
      return(invisible())
    }
    
    # ---- process finished? ----
    if (!simProcess$process$is_alive()) {
      # Final drain loop with Safety Limit
      max_drains <- 50
      drain_count <- 0
      
      repeat {
        drain_count <- drain_count + 1
        if (drain_count > max_drains)
          break
        
        drain_out <- read_out_safe(simProcess$process)
        drain_err <- read_err_safe(simProcess$process)
        
        if (length(drain_out) == 0 && length(drain_err) == 0) {
          break
        }
        drain_payload <- list(out = drain_out, err = drain_err)
        if (length(drain_payload) > 0) {
          session$sendCustomMessage("appendLog", drain_payload)
        }
      }
      
      # Send termination message
      session$sendCustomMessage("appendLog", list("out" =  " --- process finished ---"))
      
      # Clean up state
      
      loadInstancesSimulationOutputs(TRUE)
      simProcess <<- NULL
      updateActionButton(inputId = "runsim",
                         label = paste("🔥 Run ", input$simulator))
    }
  })
  
  
  ## reactive log file  ----
  log_reader <- reactivePoll(
    intervalMillis = 1000,
    # 1 second
    session = session,
    
    checkFunc = function() {
      file.info(logs$logfile)$mtime
    },
    
    # expensive read: only when it DID change
    valueFunc = function() {
      readLines(logs$logfile, warn = FALSE)
    }
  )
  
  
  ## RUN SIM STRING ------
  source("functions_makeCmd.R", local = T)
  
  
  ## render HTML to log ----
  output$log <- renderUI({
    lines <- log_reader()
    tt <- tags$div(lapply(lines, function(line) {
      cls <- dplyr::case_when(
        grepl("ERROR", line, ignore.case = T) ~ "log-error",
        grepl("WARN", line, ignore.case = T) ~ "log-warn",
        grepl("INFO", line, ignore.case = T) ~ "log-info",
        TRUE                  ~ "log-default"
      )
      tags$div(class = cls, HTML(line))
    }))
    
    tags$div(id = "logbox", tt)
  })
  
  
  showNotification <- function(...,
                               type = "message",
                               duration = 0,
                               id = NULL) {
    text <- paste(..., collapse = " ")
    if (!is.element(type, c("default", "message", "warning", "error"))) {
      typesh <- "default"
    } else {
      typesh <- type
    }
    
    if (duration > 0) {
      shiny::showNotification(
        HTML(text),
        duration = duration,
        type = typesh,
        id = id
      )
    }
    
    if (duration > 10) {
      shinyWidgets::sendSweetAlert(text = HTML(text),
                                   title = type,
                                   html = T)
    }
    if (!is.null(logs$logfile)) {
      writeLines(paste0(
        format(Sys.time(), "<b>[%Y-%m-%d %H:%M:%S] "),
        ifelse(toupper(type)=="warning", 
               paste(toupper(type), " ⚠ "),
               toupper(type)
               ), "</b>: ",
        paste(text, 
        collapse = " ") ), 
        con = logs$log_con)
      flush(logs$log_con)
    }
    
  }
  
  # change simulator -----
  observeEvent(input$simulator, {
    if (input$simulator == "FlamMap") {
      shinyWidgets::inputSweetAlert(
        inputId = "flammapPath",
        text = "Please insert the exact path to FlamMap
software in your MS Windows machine (the same where you are running your R app).
Work in progress....",
        input = "textarea"
      )
    }
    
    updateActionButton(inputId = "runsim",
                       label = paste("🔥 Run ", input$simulator))
    
  })
  
  # IGNITION FILES OBSERVE -----
  observeEvent(ignitionFiles(), {
    ign <- ignitionFiles()
    names(ign) <- basename(ign)
    
    
    if (length(ign) > 0) {
      checkIgnitionFile(ign[[length(ign)]])
      shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile", choices = ign)
      sel <- ign[[1]]
      if (isTruthy(input$IGNIPOINT))
        sel <- input$IGNIPOINT
      print(sel)
      shiny::updateSelectInput(inputId = "IGNIPOINT",
                               choices = ign,
                               selected = sel)
    } else {
      shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile", choices = c())
      shiny::updateSelectInput(inputId = "IGNIPOINT", choices = c())
    }
    
  })
  ## IGNITION UPLOAD -----
  observeEvent(input$upload_table_ignition_input, {
    write.csv(
      read.csv(input$upload_table_ignition_input$datapath),
      file.path(input$inputfolder, sprintf(
        "Ignitions_%s.csv",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      )),
      quote = FALSE,
      row.names = F
    )
    
    ignitionFiles(
      list.files(
        path = input$inputfolder,
        pattern = ".*ignitions*\\.(csv)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
  }, ignoreInit = T)
  
  ## WEATHER FILES OBSERVE -----
  observeEvent(weatherFiles(), {
    wf <- weatherFiles()
 
    if (length(wf) != 0) {
      df <-  na.omit(read.csv(wf[[1]]))
      weatherDataTable(df)
      names(wf) <- basename(wf)
      shinyWidgets::updatePickerInput(inputId = "chooseWeatherFile", choices = wf)
      sel <- wf[[1]]
      if (isTruthy(input$WEAFILE))
        sel <- input$WEAFILE
      
      updateSelectInput(inputId = "WEAFILE",
                        choices = wf,
                        selected = sel)
      
      
    } else {
      showNotification(paste0("NO WEATHER FILE! A TEMPLATE WITH ONE LINE IS LOADED ..."),
                       type = "warning")
      f <- file.path(input$inputfolder, "Weather.csv")
      weatherFiles(f)
      df <- read.csv("templates/Weather.csv", nrows = 1)
      write.csv(df, f, quote = FALSE, row.names = FALSE)
      weatherDataTable(df)
    }
  })
  ## WEATHER UPLOAD -----
  observeEvent(input$upload_table_weather_input, {
    write.csv(
      read.csv(input$upload_table_weather_input$datapath),
      file.path(input$inputfolder, sprintf(
        "Weather_%s.csv", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      )),
      quote = FALSE,
      row.names = F
    )
    
    weatherFiles(
      list.files(
        path = input$inputfolder,
        pattern = ".*weather.*\\.(csv)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
    
  }, ignoreInit = T)
  
  
  output$download_table_ignition_shapefile <- downloadHandler(
    filename = function() { 
      req(input$chooseIgnitionFile)
      gsub("\\.csv$", ".zip", basename(input$chooseIgnitionFile))
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
    
        df <- read.csv(input$chooseIgnitionFile) 
        shp_files <- file.path(dirname(file), "Ignitions.shp")
 
        write_sf(sf::st_as_sf(df, coords=c("X","Y"), crs="epsg:4326"), shp_files) 
        wd <- getwd()
        setwd(dirname(file))
        zip::zip(zipfile = file, 
            files = list.files(path = dirname(file),
                               pattern =  "Ignitions" 
                              )
            )
        setwd(wd)
    }
  )
  
  output$download_table_weather_flammap <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(gsub(".csv$", ".wxs", basename(input$chooseWeatherFile)))
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      export_weather_to_wxs(
        read.csv(input$chooseWeatherFile),
        file
      )
    }
  )
  
  output$download_table_weather <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(basename(input$chooseWeatherFile))
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(
        read.csv(input$chooseWeatherFile),
        file,
        quote = FALSE,
        row.names = FALSE
      )
    }
  )
  # change RASTER stack -----
  observeEvent(rasterFiles(), {
    req(rasterFiles())
    
    
    ### READ RASTERS ----
    withProgress(message = 'Adding layers', value = 0, {
      ## UPDATE LEAFLET ------
      leaflet::leafletProxy("map") |>
        leaflet::clearImages()  |>
        leaflet::clearControls() |> addFWI()
      
      crs <- NA
      layerNames <- clcLayerName
      rasters <<- list()
      rfiles <- rasterFiles()
      names(rfiles) <- basename(rfiles)
      for (wg in names(SIM_INPUTS)) {
        if (tolower(wg) == "fuel_model")
          next
        shiny::updateSelectInput(
          inputId = toupper(wg),
          choices =  rfiles,
          selected = ""
        )
      }
      strt <- 0
      
      terra.rasters <<- list()
      for (fi in rfiles) {
        strt <- strt + 1
        layerName <- paste0(basename(input$inputfolder),
                            " - ",
                            toupper(tools::file_path_sans_ext(basename(fi))))
        
        layerNames <- c(layerNames, layerName)
        incProgress(strt / (length(rfiles) + 2),
                    detail = sprintf("Adding layer %s", layerName))
        
        r2 <- terra::rast(fi)
        
        if(terra::is.lonlat(r2)){
          showNotification(HTML(
            sprintf(
              "Raster %s is in geographic coordinates (latitude and longitude). <br>
This will not work with fire spread simulations - 
I will force this to EPSG:3857 (PseudoMercator projection) and 
<u>square pixel rounded to nearest meter</u>. 
<b>Raster will be overwritten. </b>
<br>If this is not what you want please remove the dataset and reload a projected dataset.
",
              basename(fi) 
            )
          ), duration = ifelse(length(rasters) > 0, 10, 20), type = "info")
          
          
          
           if(length(rasters) > 0){
             rt <- terra::project(r2, terra::rast(rasters[[1]]))
           } else {
             r_3857 <- terra::project(r2, "epsg:3857")
             exact_res <- mean(res(r_3857)) 
             
             ## 
             rt <- project(r2, "EPSG:3857", res = round(exact_res), method = "near")
          
           }
           terra::writeRaster(rt, filename = fi, overwrite=T)
           r2 <- terra::rast(fi)
        } 
        
        
        
        ## check alignment between rasters -----
        if (length(rasters) > 0) {
          if (!compareGeom(terra::rast(rasters[[1]]), r2, stopOnError = FALSE)) {
            showNotification(HTML(
              sprintf(
                "Raster <b>%s</b> NOT aligned with raster stack <b>%s!</b>
<br>%s <br>%s <br>%s <br>%s <br>
Either CRS,
origin or resolution are different. <br>
The system will try to align the landscape stack. overwriting the misaligned raster. 
<u>Reload the dataset for better control</u>.",
                basename(sources(r2)),
                basename(rasters[[1]]),
                paste(res(r2), collapse = "x") , 
                paste(res(terra::rast(rasters[[1]])), collapse = "x"),
                paste(ext(r2) , collapse = "x"), 
                paste(ext(terra::rast(rasters[[1]])), collapse = "x") 
              )
            ),
            duration = 20,
            type = "warning")
            
            rt <- terra::project(r2, terra::rast(rasters[[1]]))
            terra::writeRaster(rt, filename = fi, overwrite=T)
            r2 <- terra::rast(fi)
          }
        }
        
        terra.rasters[[layerName]] <<- r2
        
        
        if (terra::crs(r2) == "") {
          showNotification(
            HTML(
              "<b>Raster Info:</b> NO 🌐 CRS provided to raster, so a generic pseudo-mercator projection assigned:
<code>EPSG:3857</code>.  Work in progress for assigning a CRS 🌐 manually - keep tuned!"
            ),
            duration = 0,
            type = "warning"
          )
          # guess CRS of input
          cc <- center(r2)
          if (abs(cc$lng) > 360 && abs(cc$lat) > 90) {
            terra::crs(r2) <- "EPSG:3857"
          }
        }
        ## LANDSCAPE FILES ------
        #### FUEL map ------
        if (grepl("forest|fuel", layerName, ignore.case = T)) {
          tf <- r2
         
          filepath.old <- terra::sources(tf)[[1]]
          if (as.integer(substr(terra::datatype(tf), 4, 4)) < 4 ||
              grepl("FLT", terra::datatype(tf)) ) { 
            # Note: add_suffix must be defined elsewhere in your package
           
 
              showNotification("Fuel is in " , terra::datatype(tf), " not in 32 or 64 bits integer! Converting and copying. Remember to upload  datasets with fuel tif as 32 and 64 bits integers to avoid this warning.",
                               duration = 22, type="warning")
            
              filepath <- paste0(tools::file_path_sans_ext(filepath.old), "_INT4U.", tools::file_ext(filepath.old))
            
              terra::writeRaster(tf, filename = filepath, datatype = "INT4U", 
                                   overwrite=T  )    
              file.remove(filepath.old)
              file.rename(filepath, filepath.old)
             
          }
          r2 <- terra::as.factor(r2)
          levs <- data.frame(
            value = lut_fbp$grid_value,
            class = lut_fbp$descriptive_name,
            fuel = lut_fbp$fuel_type,
            color =   rgb(lut_fbp$r, lut_fbp$g, lut_fbp$b, maxColorValue = 255)
          )
          lut_generic <- lut_fbp
          if (isScottBurgan(r2)) {
            levs <- data.frame(
              value = lut_sb$grid_value,
              class = as.character(lut_sb$descriptive_name),
              fuel = lut_sb$fuel_type,
              color =   rgb(lut_sb$r, lut_sb$g, lut_sb$b, maxColorValue = 255)
            )
            lut_generic <- lut_sb
          }
          
          nl <- levs[1, ]
          nl$value <- 0
          nl$class <- "N.A."
          nl$fuel <- "N.A."
          nl$color <- "#00000000"
          
          if (nrow(levs[levs$value == 0, ]) == 0) {
            levs <- rbind(nl , levs)
          } else {
            levs[levs$value == 0, ] <- nl
          }
          levels(r2) <- levs
          # browser()
          copts <- leafem::colorOptions(
            palette = levs$color,
            domain = levs$value,
            na.color = "#00000000"
          )
          
          na.color = "transparent"
          rasters[["FUEL"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "FUEL",
                                   choices =  rfiles ,
                                   selected =   rasters[["FUEL"]])
          
          
        } else {
          v <- spatSample(r2,
                          size = 1e4,
                          method = "regular",
                          na.rm = TRUE)
          
          rrange <- quantile(v[, 1], probs = c(0.1, 0.9))
          palette = viridisLite::viridis(20)
          domain  = rrange
          copts <- colorOptions(
            palette = palette,
            domain = unname(domain),
            na.color = "#00000000"
          )
        }
        
        
        if (grepl("elev|dtm|dem", layerName, ignore.case = T)) {
          rasters[["ELEVATION"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "ELEVATION",
                                   choices =  rfiles ,
                                   selected =  rasters[["ELEVATION"]])
        }   else if (grepl("slope", layerName, ignore.case = T)) {
          rasters[["SLOPE"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "SLOPE",
                                   choices =  rfiles ,
                                   selected =  rasters[["SLOPE"]])
        }   else if (grepl("saz|azimuth", layerName, ignore.case = T)) {
          rasters[["SAZ"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "SAZ",
                                   choices =  rfiles ,
                                   selected =  rasters[["SAZ"]])
        }   else if (grepl("cur|curvature", layerName, ignore.case = T)) {
          rasters[["CUR"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CUR",
                                   choices =  rfiles ,
                                   selected =  rasters[["CUR"]])
        }    else if (grepl("cbd|bulk", layerName, ignore.case = T)) {
          rasters[["CBD"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CBD",
                                   choices =  rfiles ,
                                   selected =  rasters[["CBD"]])
        }  else if (grepl("cbh|base", layerName, ignore.case = T)) {
          rasters[["CBH"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CBH",
                                   choices =  rfiles ,
                                   selected =  rasters[["CBH"]])
        }  else if (grepl("ccf|cover|fcc", layerName, ignore.case = T)) {
          rasters[["CCF"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CCF",
                                   choices =  rfiles ,
                                   selected =  rasters[["CCF"]])
        }  else if (grepl("ch|height|hm", layerName, ignore.case = T)) {
          rasters[["CHM"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CHM",
                                   choices =  rfiles ,
                                   selected =  rasters[["CHM"]])
        }  else if (grepl("breaks", layerName, ignore.case = T)) {
          rasters[["FIREBREAKS"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "FIREBREAKS",
                                   choices =  rfiles ,
                                   selected = rasters[["FIREBREAKS"]])
        } else {
          rasters[[layerName]] <<- terra::sources(r2)[[1]]
        }
       
        opacityControl [[layerName]] <- list(
          min = 0,
          max = 1,
          step = 0.1,
          default = 0.85,
          width = '100%',
          class = 'opacity-slider'
        )
        
        view_settings[[layerName]] <- list(coords = as.numeric(st_transform(st_bbox(r2), 4326)))
        
        leaflet::leafletProxy("map") |>
          leafem::addGeoRaster(
            stars::st_as_stars(r2),
            colorOptions = copts, 
            opacity = 0.65, 
            group = layerName,
            autozoom = F,
            options = leafletOptions(pane = "fuels_pane"), 
            imagequeryOptions = leafem::imagequeryOptions(
              digits = 0,
              prefix =
                "",
              position =
                "bottomright",
              noData = "NA"
            ),
            layerId = gsub(" ", "", layerName)
          ) |>   leaflet::hideGroup(layerName)
        # if (!grepl("forest|fuel", layerName, ignore.case = T)) {
          # leaflet::leafletProxy("map")   |> leaflet::hideGroup(layerName)
        # }
        
      }
      
      
      if (length(rasters) < 8) {
        # Keep button disabled and warn the user via notification
        disable("download_landscape_flammap")
        enable("download_landscape_flammap")
        shinyjs::runjs(paste("$('#download_landscape_flammap').parent().attr('data-tippy-content', 'Warning: Found only ", length(rasters), " rasters. You need at least 8 to download.');"))
        
        
      } else {
        # Folder is valid, check if the correct elements are available
        ls <- which( names(rasters) %in% 
                       landscapeFlamMap )
        if(length(ls)!= 8){
          
          showNotification(paste(setdiff(landscapeFlamMap, names(rasters)), 
                                 collapse = " - "), " missing: landscape file cannot be created.",
                           duration=22, type="warning" )
          
        } else {
          enable("download_landscape_flammap")
          enable("download_landscape_forefire")
          shinyjs::runjs(paste("$('#download_landscape_flammap').parent().attr('data-tippy-content', null  );"))        
        }
 
      }
      
    })
    
    currentRasterStack <<- tryCatch({
      terra::rast(terra.rasters)
    }, error = function(e) {
      showNotification(paste("ERROR:", e$message),
                       type = "error",
                       duration = 16)
      NULL
    })
    if (is.null(currentRasterStack))
      return(NA)
    
    r2 <- terra::project(terra::ext(r2), from = terra::crs(r2), to = "epsg:4326")
    
    leafletProxy("map") |>
      
      addLayersControl(
        baseGroups = unlist(unname(base_layers)),
        overlayGroups = c(unname(unlist(sim_layers)), layerNames, names(fwi_layers)),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      )   |>
      customizeLayersControl(
        view_settings =view_settings ,
        # home_btns = TRUE,
        opacityControl = opacityControl,
        includelegends = FALSE,
        addCollapseButton = TRUE,
        layersControlCSS = list("opacity" = 0.9),
        increaseOpacityOnHover = FALSE
      ) |>
      leaflet::fitBounds(
        lng1 =  xmin(r2),
        lat1 =  ymin(r2),
        lng2 =  xmax(r2),
        lat2 =  ymax(r2)
      )
    
    session$sendCustomMessage("layersControlReady", list())
    # shinyjs::runjs("autoGroupLeafletLayers(\".leaflet-control-layers\");")
    
    if (is.null(currentRasterStack)) {
      showNotification(
        "No Forest raster file found, please read  documentation on how to prepare a dataset",
        type = "error",
        duration = 12
      )
    }
    
  })
  
  # INPUT ARGS OBSERVE ------
  ### CROWN  ----
  observeEvent(input$CROWN, {
    if (!input$CROWN) {
      shinyjs::disable("CBH")
      shinyjs::disable("CBD")
      shinyjs::disable("CCF")
      shinyjs::disable("CHM")
    } else {
      shinyjs::enable("CBH")
      shinyjs::enable("CBD")
      shinyjs::enable("CCF")
      shinyjs::enable("CHM")
    }
  })
  ### IGNITION_MODE   ----
  observeEvent(input$IGNITION_MODE, {
    md <- as.integer(substr(input$IGNITION_MODE, 1, 1))
    if (md == 0) {
      shinyjs::disable("IGNITIONFILE")
      shinyjs::disable("IGNIPOINT")
      shinyjs::disable("IGNIRADIUS")
    }
    else if (md == 1) {
      shinyjs::enable("IGNITIONFILE")
      shinyjs::disable("IGNIPOINT")
      shinyjs::disable("IGNIRADIUS")
    }
    else if (md == 2) {
      shinyjs::enable("IGNIRADIUS")
      shinyjs::disable("IGNITIONFILE")
      shinyjs::enable("IGNIPOINT")
    }
  })
  
  # SIMULATION INSTANCE  ------
  
  ## POSTPROCESS SIM -----
  observeEvent(input$processSimulationOutputInstance, {
    req(input$inputfolder)
    req(input$outputInstanceFolder)
    ### grids ----
    resultspath <- file.path(input$outputInstanceFolder,
                             "results" )
       gridpath <- file.path(resultspath, "Grids")
       # rdapath <- file.path(resultspath, "weatherTimestamps.rda")
    
    if (!dir.exists(gridpath)) {
      showNotification(
        "No Grid output found in this simulation instance, did you select the correct output arguments in the argument grid of the simulator?" ,
        type = "warning",
        duration = 20
      )
      return(invisible())
    }
    
     
    grids <- list.files(gridpath, pattern = "Grids\\d+", 
                        include.dirs = TRUE, 
                       full.names = T, recursive = F)
    if (length(grids) == 0) {
      showNotification(
        "No Grid files found in this simulation instance, there is a Grid folder but not grids inside" ,
        type = "warning",
        duration = 10
      )
      
    }  else {
      
      withProgress(message = 'Processing GRIDS Raster...',
                   min = 0,
                   max = length(grids)*2, {
                     # 1. Load the raster
                  
                     for (fp in grids) {
                       iter <- as.integer( gsub("\\D+", "",
                                    basename(fp) ) )
                       if(is.na(iter)) next
                       incProgress(1, detail = paste("Start processing grid n. ", iter ) )
                    
                       ms <- readGrids(fp, force = input$processSimulationOutputInstance_force)
                       incProgress(1, detail = ms)
                       
                     }
         })
    }
    
    if(!input$processSimulationOutputInstance_force && 
       file.exists(
         file.path(input$outputInstanceFolder, "results", 
                   paste0(
                     basename(input$outputInstanceFolder),
                     "_Grids_GeoTiffs.zip" 
                    ) 
                  )
       ) && 
       file.exists(
         file.path(input$outputInstanceFolder, "results", 
                   paste0(
                     basename(input$outputInstanceFolder),
                     "_Grids_Geopackages.zip" 
                   ) 
         )
       )
    ){ 
      
      sztif <- file.size(  file.path(input$outputInstanceFolder, "results", 
                                  paste0(
                                    basename(input$outputInstanceFolder),
                                    "_Grids_GeoTiffs.zip" 
                                  )  ) )
      
      szgpkg <- file.size(  file.path(input$outputInstanceFolder, "results", 
                                  paste0(
                                    basename(input$outputInstanceFolder),
                                    "_Grids_Geopackages.zip" 
                                  )  ) )
      
      showNotification(
        "Files already processed, check the force flag to re-process" ,
        type = "warning",
        duration = 19
      )
      } else {
        withProgress(message = 'Compressing GRIDS Raster...',
                     min = 0,
                     max = 4, {
                       
                       gpkgout <- file.path(input$outputInstanceFolder, 
                                            "results", "Grids", "geopackages")
                       tifout <- file.path(input$outputInstanceFolder, 
                                           "results", "Grids", "geotiffs")
                       if(dir.exists(tifout) && nchar(tifout)>10){
                         unlink(tifout, recursive = T, force = T)
                       }
                       if(dir.exists(gpkgout) && nchar(gpkgout)>10){
                         unlink(gpkgout, recursive = T, force = T)
                       }
                       grids  <- list.files(
                         file.path(input$outputInstanceFolder, "results", "Grids"),
                         pattern = "Grids\\d+.tif$", full.names = T, recursive = T)
                       
                       gpkgs  <- list.files(
                         file.path(input$outputInstanceFolder, 
                                   "results", "Grids"),
                         pattern = "Grids\\d+.gpkg$", full.names = T, recursive = T)
                       
                       

                       dir.create(gpkgout, showWarnings = F )
         
                       dir.create(tifout, showWarnings = F )
                       
                       incProgress(1, detail = paste("Copying ", length(grids), " files" ))
                  
                       tifcopysuccess <- file.copy(grids, file.path(tifout, basename(grids) ), overwrite = T)
                       gpkgcopysuccess <- file.copy(gpkgs, file.path(gpkgout, basename(gpkgs) ), overwrite = T)
                       if(any(c(!tifcopysuccess, !gpkgcopysuccess)) ){
                         showNotification(
                           "Some files not copied" ,
                           type = "warning",
                           duration = 10
                         )
                       }
                       
                       
                       incProgress(1, detail = paste("Zipping  ", length(grids), " GeoTiffs files" ))
                       
                       old_wd <- getwd()
                       target_dir <- tifout
                       setwd(target_dir)
                       
                       zip(zipfile =file.path("../../", 
                                              paste0(
                                                basename(input$outputInstanceFolder),
                                                "_Grids_GeoTiffs.zip" 
                                              ) ),
                           list.files(getwd(), pattern = "\\.tif$") )
                       
                       sztif <- file.size(file.path("../../",
                                                 paste0(
                                                   basename(input$outputInstanceFolder),
                                                   "_Grids_GeoTiffs.zip"
                                                 )  ))
                       
                       setwd("../geopackages")
                       
                       
                       incProgress(1, detail = paste("Zipping  ", length(grids), " Geopackages files" ))
                       zip(file.path("../../", 
                                     paste0(
                                       basename(input$outputInstanceFolder),
                                       "_Grids_Geopackages.zip"
                                     )
                       ),  list.files(getwd(), pattern = "\\.gpkg$") ) 
                       
                       szgpkg <- file.size(file.path("../../",
                                                 paste0(
                                                   basename(input$outputInstanceFolder),
                                                   "_Grids_Geopackages.zip"
                                                 )  ))
                       
                   
                       
                       setwd(old_wd)
                       if(dir.exists(tifout) && nchar(tifout)>10){
                         unlink(tifout, recursive = T, force = T)
                       }
                       if(dir.exists(gpkgout) && nchar(gpkgout)>10){
                         unlink(gpkgout, recursive = T, force = T)
                       }
                     })
      }
    
    shinyjs::runjs(paste0("$('#download\\\\.grids\\\\.tiff span:first').html('(",
                          round(sztif/1000000,2)," MB)&nbsp;');"))
    shinyjs::runjs(paste0("$('#download\\\\.grids\\\\.gpkg span:first').html('(",
                          round(szgpkg/1000000,2)," MB)&nbsp;');"))
    
    ### ROS ----
 
    rospath <- file.path(input$outputInstanceFolder, "results", "RateOfSpread")
    if (!dir.exists(rospath)) {
      showNotification(
        "No RateOfSpread output found in this simulation instance, did you select the correct output arguments in the argument grid of the simulator?" ,
        type = "warning",
        duration = 10
      )
      
    } else {
      
      if(!input$processSimulationOutputInstance_force && 
         file.exists(
           file.path(input$outputInstanceFolder, "results", 
                     paste0(
                       basename(input$outputInstanceFolder),
                       "_RateOfSpread.zip" 
                     ) 
           )
         )
      ) {
        
        invisible()
        
      } else {
        withProgress(message = 'Processing ROS  Rasters...',
                     min = 0,
                     max = length(grids), {  
                       
                       ext <- terra::ext(terra.rasters[[1]])
                       res <- terra::res(terra.rasters[[1]])
                       crs <- terra::crs(terra.rasters[[1]])
                       grids <- list.files(
                         rospath,
                         pattern = ".*\\.asc$",
                         full.names = T,
                         recursive = F
                       )
                       
                       outtifs <- gsub("\\.asc$", "\\.tif", grids)  
                       
                       for (fp in grids) {
                         message(basename(fp))
                         incProgress(1, detail = paste("Processing ", basename(fp) ))
                         
                         r <- rast(fp)
                         r[r == 0] <- NA
                         terra::crs(r) <- crs
                         
                         writeRaster(
                           r,
                           paste0(tools::file_path_sans_ext(fp), ".tif"),
                           overwrite = T,
                           gdal = c("COMPRESS=DEFLATE")
                         )
                         
                       }
                     })
        
        
        old_wd <- getwd()
        target_dir <- dirname(outtifs)[[1]]
        setwd(target_dir)
        
        zip(file.path("../",
                      paste0(
                        basename(input$outputInstanceFolder),
                        "_RateOfSpread.zip"
                      )
        ), 
        list.files(getwd(), pattern = "\\.tif$") )
        setwd(old_wd)
      }

      sz <- file.size(file.path(input$outputInstanceFolder, "results", 
                                paste0(
                                  basename(input$outputInstanceFolder),
                                  "_RateOfSpread.zip" 
                                )  ) )
      shinyjs::runjs(paste0("$('#download\\\\.ros span:first').html('(",
                            round(sz/1000000,2)," MB)&nbsp;');"))
      

    }
    

     
    
  })
  
  
  output$download.grids.gpkg <- downloadHandler(
    filename = function() {
      paste0(basename(input$outputInstanceFolder),
             "_BurntAreaPerTimestampGpkg.zip")
    },
    content = function(file) {
      zip_path <-  file.path(
        input$outputInstanceFolder,
        "results",
        paste0(
          basename(input$outputInstanceFolder),
          "_Grids_Geopackages.zip"
        )
      )
      
      if (file.exists(zip_path)) {
        file.copy(zip_path, file, overwrite = T)
      } else {
        showNotification("BurntAreaPerTimestamp ZIP file not found!",
                         type = "warning",
                         duration = 10)
      }
    },
    contentType = "application/zip"
  )
  
  
  output$download.grids.tiff <- downloadHandler(
    filename = function() {
      paste0(basename(input$outputInstanceFolder),
             "_BurntAreaPerTimestampTIFFs.zip")
    },
    content = function(file) {
      zip_path <-  file.path(
        input$outputInstanceFolder,
        "results",
        paste0(
          basename(input$outputInstanceFolder),
          "_Grids_GeoTiffs.zip"
        )
      )
      
      if (file.exists(zip_path)) {
        file.copy(zip_path, file, overwrite = T)
      } else {
        showNotification("BurntAreaPerTimestamp ZIP file not found!",
                         type = "warning",
                         duration = 10)
      }
    },
    contentType = "application/zip"
  )
  
  
  output$download.ros <- downloadHandler(
    filename = function() {
      paste0(basename(input$outputInstanceFolder),
             "_RateOfSpread.zip")
    },
    content = function(file) {
      zip_path <-  paste0(file.path(
        input$outputInstanceFolder,
        "results",
        paste0(
          basename(input$outputInstanceFolder),
          "_RateOfSpread.zip"
        )
      ))
      
      if (file.exists(zip_path)) {
        file.copy(zip_path, file,  overwrite = T)
      } else {
        showNotification("RateOfSpread ZIP file not found!",
                         type = "warning",
                         duration = 10)
      }
    },
    contentType = "application/zip"
  )
  ### change folder ----
  observeEvent(input$outputInstanceFolder, {
    req(input$outputInstanceFolder)
    processSimulationOutputFolder(input$outputInstanceFolder)
  })
  ### del folder ----
  observeEvent(input$deleteSimulationOutputInstance, {
    req(input$deleteSimulationOutputInstance)
    
    if (nchar(input$outputInstanceFolder) > 5 &&
        sum(lengths(gregexpr("/", input$outputInstanceFolder))) > 2) {
      shinyWidgets::ask_confirmation(
        inputId = "deleteSimulationOutputInstance_ok",
        html = T,
        sprintf(
          "Confirm you want to delete the selected
Simulation output?? It cannot be undone!<br><u><b>%s</b></u>",
          basename(input$outputInstanceFolder)
        )
      )
    }
  })
  
  observeEvent(input$deleteSimulationOutputInstance_ok, {
    req(input$deleteSimulationOutputInstance_ok)
    req(input$outputInstanceFolder)
    unlink(input$outputInstanceFolder, recursive = T)
    loadInstancesSimulationOutputs()
  })
  
  # change DATASET folder ----
  observeEvent(input$inputfolder, {
    
    if (is.null(input$inputfolder) || input$inputfolder == "") {
      shinyjs::disable("download_table_ignition_shapefile")
    } else {
      shinyjs::enable("download_table_ignition_shapefile")
    }
    
    req(input$inputfolder)
    
    
    showNotification(
      text = paste0(
        " DATASET:  ",
        basename(input$inputfolder) ,
        "========================================"
      )
    )
    
    outfolder <<- file.path(input$inputfolder, "output")
    dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)
    
    loadInstancesSimulationOutputs(FALSE)
    
    
    ip <- NULL
    currentRasterStack <<- NULL
    
    rasterFiles(
      list.files(
        path = normalizePath(input$inputfolder),
        pattern = "\\.(asc|tif|tiff)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
    csvFiles(
      list.files(
        path =  normalizePath(input$inputfolder),
        pattern = "\\.(csv)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
    
    ignitionFiles(
      list.files(
        path = input$inputfolder,
        pattern = ".*ignition.*\\.(csv)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
    
    weatherFiles(
      list.files(
        path = input$inputfolder,
        pattern = ".*weather.*\\.(csv)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
    
    
    shinyjs::runjs("$('.info.legend.rastervals.leaflet-control').remove();")
    
    loadState()
    
  })
  
  ##render leaflet -----
  output$map <- renderLeaflet({
    mymap
  })
  
  # CLICK mode ----
  observeEvent(input$map_mode, {
    # print(input$map_mode)
    if (input$map_mode == "") {
      runjs("document.getElementById('map').style.cursor = null;")
    }
  })
  
  # tooltips  ----
  observeEvent(list(input$tooltips, input$tooltipsSize), {
    print(input$tooltips)
    if (input$tooltips) {
      shinyjs::runjs(
        sprintf(
          "   makeTooltips(%d); ttinstances.forEach(i => i.enable());",
          input$tooltipsSize
        )
      )
    } else {
      shinyjs::runjs("   ttinstances.forEach(i => i.disable());")
    }
  })
  
  # WMSQueryReturned Popup ----
  observeEvent(input$WMSQueryReturnedPop, {
    req(input$WMSQueryReturnedPop)
    leafletProxy("map") |>
      addPopups(
        lng = input$WMSQueryReturnedPop$coords[[1]],
        lat = input$WMSQueryReturnedPop$coords[[2]],
        popup = input$WMSQueryReturnedPop$data,
        layerId = "my_popup"  # optional, to remove/update later
      )
  })
  
  # EFFIS AND open meteo for weather file ----
  # 
  #   onclick = 
  
  observeEvent( input$create_table_weather, {
    dt <- isolate(weatherDataTable())
    req(dt)
    timeseries <- getDateTimeFromCSV(dt)
    if(!inherits(timeseries, "POSIXct")){
      showNotification("Cannot read value ", timeseries[[1]] , " as Date Time! Please check the format of your timeseries column which should be named 'date' or 'datetime'. ",
                       duration=15, type="warning")
      return()
    }
    shinyjs::runjs(sprintf("getFromOpenMeteo(null, null, false, start_date='%s', end_date='%s'); queryWMS(null, null, false, start_date='%s');", 
                           as.character(as.Date(min(timeseries))),
                           as.character(as.Date(max(timeseries))),
                           as.character(as.Date(min(timeseries)))
                           )
                   );
 
    #"getFromOpenMeteo(); queryWMS(); ",
  })
  observeEvent(list(input$openmeteoInput, input$WMSQueryReturned), {
    
    if (!(
      shiny::isTruthy(input$openmeteoInput) ||
      shiny::isTruthy(input$WMSQueryReturned)
    )) {
      return(invisible(NULL))
    } 
    df <- isolate(weatherDataTable())
    dfl <- isolate(as.list(df[1, ]))
    
    
    if (shiny::isTruthy(input$openmeteoInput)) {
      if (!is.list(input$openmeteoInput)) {
        msg <- paste("❌ API error:", input$openmeteoInput)
        showNotification(msg, type = "error")
        return(invisible(NULL))
      }
      
      # 1. Determine if the API payload is Historical (hourly arrays) or Current (single values)
      if (!is.null(input$openmeteoInput$hourly)) {
        
        # --- HISTORICAL / ARCHIVE TRACK ---
        # Open-Meteo hourly timestamps look like "2026-06-01T00:00". 
        # We convert the entire vector to data.frame/list friendly format.
        formatted_datetimes <- gsub("T", " ", input$openmeteoInput$hourly$time)
        
        dflt <- list(
          Instance     = rep("SB", length(formatted_datetimes)),
          datetime     = formatted_datetimes,
          TMP          = unlist(input$openmeteoInput$hourly$temperature_2m),
          WS           = unlist(input$openmeteoInput$hourly$wind_speed_10m),
          WD           = unlist(input$openmeteoInput$hourly$wind_direction_10m),
          RH           = unlist(input$openmeteoInput$hourly$relative_humidity_2m),
          FireScenario = rep("open meteo historical + EFFIS", length(formatted_datetimes))
        )
        
        outp <- "OM_Historical"
        
      } else {
        
        # --- CURRENT FORECAST TRACK ---
        # Standard single-value parsing fallback for live data
        dflt <- list(
          Instance     = "SB",
          datetime     = gsub("T", " ", input$openmeteoInput$current$time),
          TMP          = input$openmeteoInput$current$temperature_2m,
          WS           = input$openmeteoInput$current$wind_speed_10m,
          WD           = input$openmeteoInput$current$wind_direction_10m,
          RH           = input$openmeteoInput$current$relative_humidity_2m,
          FireScenario = "open meteo current + EFFIS"
        )
        
        outp <- "OM_Current"
      }
      
      # outp <- "OM"
      # dflt <-   list(
      #   Instance = "SB",
      #   datetime = gsub("T", " ", input$openmeteoInput$current$time),
      #   TMP = input$openmeteoInput$current$temperature_2m,
      #   WS = input$openmeteoInput$current$wind_speed_10m,
      #   WD = input$openmeteoInput$current$wind_direction_10m,
      #   RH = input$openmeteoInput$current$relative_humidity_2m,
      #   FireScenario = "open meteo + EFFIS"
      # )
      
      dfl[na.omit(names(dflt))] <- dflt[na.omit(names(dflt))]
    }
    
    if (shiny::isTruthy(input$WMSQueryReturned) &&
        shiny::isTruthy(input$WMSQueryReturned$datast)) {
      outp <- "EFFIS"
 
      
      doc <- xml2::read_html(input$WMSQueryReturned$datast)
      rows <- xml2::xml_find_all(doc, ".//tr")
      kv_list <- lapply(rows, function(row) {
        cols <- xml_find_all(row, ".//td")
        if (length(cols) >= 2) {
          key <- xml_text(cols[[1]])
          value <- as.numeric(xml_text(cols[[2]]))
          setNames(list(value), key)  # named list
        } else {
          NULL
        }
      })
      dflt <-   do.call(c, kv_list)
      inside <- str_extract(names(dflt), "\\(([^)]+)\\)")
      
      inside_text <- str_remove_all(inside, "[()]")
      names(dflt) <- inside_text
      dfl[na.omit(names(dflt))] <- dflt[na.omit(names(dflt))]
    }
 
 
    weatherDataTable(as.data.frame(dfl, stringsAsFactors = FALSE))
  }, ignoreInit = T)
  
  ## TABLE WEATHER table -----
  output$weatherTable <- renderDT({
    dt <- weatherDataTable()
    datatable(dt,
              editable = TRUE,
              options = list(
                initComplete = JS(
                  "function(settings) {",
                  "  var th = $('#weatherTableOutputDIV thead th').filter(function() {",
                  "    $(this).attr('data-tippy-content', tooltipsStrings[$(this).text().trim() ] );  ",
                  "  });",
                  ifelse(
                    input$tooltips,
                    sprintf(" makeTooltips(%d); ", input$tooltipsSize),
                    ""
                  ),
                  "}"
                  
                )
              )) |> formatRound(columns = intersect(
                names(weatherDataTable()),
                c('DC', 'FWI', 'DMC', 'ISI', 'BUI', 'FFMC')
              ), digits = 2)
  })
  
  observeEvent(input$chooseWeatherFile, {
    req(input$chooseWeatherFile)
    df <- read.csv(input$chooseWeatherFile)
    weatherDataTable(df)
    updateSelectInput(
      inputId = "WEAFILE",
      # choices = weatherFiles,
      selected = input$chooseWeatherFile
    )
  })
  
  observeEvent(input$delete_table_weather, {
    req(input$chooseWeatherFile)
    shinyWidgets::ask_confirmation(
      inputId = "delete_table_weather_confirm",
      html = T,
      sprintf(
        "Confirm you want to delete the selected weather table: <br><u><b>%s</b></u>",
        basename(input$chooseWeatherFile)
      )
    )

  })
  
  observeEvent(input$delete_table_weather_confirm, {
    req(input$chooseWeatherFile)
    if (input$delete_table_weather_confirm) {
      if (length(isolate(weatherFiles())) > 1) {
        file.remove(input$chooseWeatherFile) 

        lf <-   list.files(
            path = input$inputfolder,
            pattern = ".*weather.*\\.(csv)$",
            full.names = TRUE,
            ignore.case = TRUE
          )
        
        weatherDataTable(NULL)
        
        updateSelectInput(
          inputId = "WEAFILE",
          choices = lf,
          selected = ""
        )
        weatherFiles( lf )
        
      } else {
        showNotification(
          paste0("Cannot remove the last weather file, is is used as a template - just modify it or upload a new one."),
          type = "warning",
          duration = 20
        )
      }  
    }
  })
  
  
  observeEvent(input$delete_table_weather_row, {
    df <- isolate(weatherDataTable())
    df <- df[-1 * input$weatherTable_rows_selected, ]
    weatherDataTable(df)
  })
  
  
  observeEvent(input$weatherTable_cell_edit, {
    print(input$weatherTable_cell_edit)
    info <- input$weatherTable_cell_edit
    df <- isolate(weatherDataTable())
    df[info$row, info$col] <- info$value
    weatherDataTable(df)
  })
  
  observeEvent(input$save_table_weather, {
    if (!isTruthy(input$chooseWeatherFile)) {
      save_table_weather_final(F)
    } else {
      showModal(md_overwrite_ignition_weather) 
    }
  })
 
  
  observeEvent(input$overwrite_file_confirm_weather_yes, {
    req(input$chooseWeatherFile)
    save_table_weather_final(T)
    removeModal()
  })
  
  observeEvent(input$overwrite_file_confirm_weather_newFile, {
    save_table_weather_final(F)
    removeModal()
  })
  
  save_table_weather_final <- function(overwrite = F) {
    df <- isolate(weatherDataTable())  
    if (overwrite && isTruthy(input$chooseWeatherFile)) {
      write.csv(df,
                input$chooseWeatherFile,
                quote = FALSE,
                row.names = F)
    } else {
      write.csv(
        df,
        file.path(
          input$inputfolder,
          sprintf(
            "Weather_%s.csv",
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          )
        ),
        quote = FALSE,
        row.names = F
      )
      weatherFiles(
        list.files(
          path = input$inputfolder,
          pattern = ".*Weather.*\\.(csv)$",
          full.names = TRUE,
          ignore.case = TRUE
        )
      )
    }
    
  }
  ## TABLE FBP table ----
  output$FBP.table <- renderDT({
    req(lut_fbp_local())
    datatable(lut_fbp_local() , editable = TRUE)
  })
  
  ## TABLE IGNITION and edits  ----
  ##
  observeEvent(input$chooseIgnitionFile, { 
    req(input$chooseIgnitionFile)
    df <- tryCatch({
      na.omit(read.csv(input$chooseIgnitionFile))
    }, warning = function(e) {
      showNotification(e$message, type = "warning", duration = 19)
      NULL
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 19)
      NULL
    })
    req(df)
    ignitionPointsCoords(df)
  })
  
  output$ignitionInfo <- renderDT({ 
    # req(ignitionPointsCoords())
    ip <- ignitionPointsCoords()
 
    if (is.null(ip) || nrow(ip)<1) {
      ip <- data.frame(X = numeric(),
                       Y = numeric(),
                       Tools = character())
    } else {
      ip$Tools <- ""
    }
    DT::datatable(
      ip,
      escape = FALSE,
      # extensions = "Buttons",
      editable = FALSE ,
      options = list(
        # dom = "Bfrtip",
        columnDefs = list(list(
          targets = ncol(ip),
          render = JS(
            "function(data, type, row, meta) {
                      return '<button onclick=\"mymap.flyTo(['+row[4]+', '+(parseFloat(row[3])+0.02)+'], 14);\">🔎</button>';
           }"
          )
        ))
      )
    ) |> formatRound(columns = c('X', 'Y'), digits = 5)
    # datatable( ip , editable = TRUE)
  })
  
  observeEvent(input$delete_table_ignition_row, {
    df <- isolate(ignitionPointsCoords())
    df <- df[-1 * input$ignitionInfo_rows_selected, ]
    ignitionPointsCoords(df)
  })
  
  
  observeEvent(input$ignitionInfo_cell_edit, {
    info <- input$ignitionInfo_cell_edit
    df <- isolate(ignitionPointsCoords())
    df[info$row, info$col] <- info$value
    ignitionPointsCoords(df)
  })
  
  save_table_ignition_final <- function(overwrite = F) {
    df <- isolate(ignitionPointsCoords())
 
    if (overwrite && isTruthy(input$chooseIgnitionFile)) {
      write.csv(df,
                input$chooseIgnitionFile,
                quote = FALSE,
                row.names = F)
    } else {
      write.csv(
        df,
        file.path(
          input$inputfolder,
          sprintf(
            "ignitionPoints_%s.csv",
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          )
        ),
        quote = FALSE,
        row.names = F
      )
      ignitionFiles(
        list.files(
          path = input$inputfolder,
          pattern = ".*ignition.*\\.(csv)$",
          full.names = TRUE,
          ignore.case = TRUE
        )
      )
    }
    
  }
  
  observeEvent(input$delete_table_ignition, {
    req(input$chooseIgnitionFile)
    shinyWidgets::ask_confirmation(
      inputId = "delete_table_ignition_confirm",
      html = T,
      sprintf(
        "Confirm you want to delete the selected ignition table: <br><u><b>%s</b></u>",
        basename(input$chooseIgnitionFile)
      )
    ) 
  })
    
  observeEvent(input$delete_table_ignition_confirm, {
    req(input$chooseIgnitionFile)
    if (input$delete_table_ignition_confirm) {
      if (length(isolate(ignitionFiles())) > 1) {
        file.remove(input$chooseIgnitionFile)
        ignitionFiles(
          list.files(
            path = input$inputfolder,
            pattern = ".*ignition.*\\.(csv)$",
            full.names = TRUE,
            ignore.case = TRUE
          )
        )
      } else {
        showNotification(
          paste0("Cannot remove the last ignition file - just modify it  "),
          type = "warning",
          duration = 10
        )
      }
    }
  })
  
  observeEvent(input$overwrite_file_confirm_yes, {
    req(input$chooseIgnitionFile)
    save_table_ignition_final(T)
    removeModal()
  })
  
  observeEvent(input$overwrite_file_confirm_newFile, {
    save_table_ignition_final(F)
    removeModal()
  })
  
  ## updates tab info ----
  observeEvent(input$infoBoxButton, {
    updateTabsetPanel(session, "tabs", selected = "infoBox")
  })
  
  ## updates side bar ignition ----
  observeEvent(input$ignitionsTable, {
    updateTabsetPanel(session, "tabs", selected = "dashboardMap")
    updateBoxSidebar("ignitionSideBar")
  })
  
  observeEvent(input$save_table_ignition, {
    if (!isTruthy(input$chooseIgnitionFile)) {
      save_table_weather_final(F)
    } else {
      showModal(md_overwrite_ignition) 
    }
  })
  
  ## add ignitionpoints -----
  observe({
    req(ignitionPointsCoords())
    ip <- ignitionPointsCoords()
    ipt <- na.omit(ip)
    leafletProxy("map") |> leaflet::clearGroup(sim_layers$IgnitionPointsMan)
    for (i in 1:nrow(ipt)) {
      # print(ipt[i,  ])
      leafletProxy("map") |>
        addAwesomeMarkers(
          as.numeric(ipt[i, 3]),
          as.numeric(ipt[i, 4]),
          icon =  fireIcon,
          options = pathOptions(pane = "ignition_points_pane"), 
          group = sim_layers$IgnitionPointsMan,
          popup = paste0(
            popup_text("Year:", ipt[i, 1]),
            "<br>",
            popup_text("NCell:", ipt[i, 2]),
            "<br>",
            popup_text("Lat:", round(as.numeric(ipt[i, 3]), 6)),
            "<br>",
            popup_text("Lon:", round(ipt[i, 4], 6))
          )
        )
    }
  })
  # Handle map mode and clicks ----
  observeEvent(input$map_click, {
    req(input$map_mode)
    lng <- input$map_click$lng
    lat <- input$map_click$lat
    
    if (input$map_mode == "WMSQuery") {
      js <- sprintf("queryWMS(%f, %f, '%s')",
                    lat,
                    lng,
                    paste(fwi_layers, collapse = ","))
      # print(js)
      shinyjs::runjs(js)
    }
    
    if (is.null(currentRasterStack)) {
      showNotification(
        paste0(
          "You did not load a dataset with rasters. Only coordinates returned: ",
          sprintf("%.6f", lat),
          ", ",
          sprintf("%.6f", lng)
        ),
        type = "message",
        duration = 0
      )
      
      # runjs("Shiny.setInputValue('map_mode', '', {priority: 'event'});")
      
      return(NULL)
    }
    
    crr <- currentRasterStack[[1]]
    
    # if(terra::nlyr(currentRasterStack)>1 ){
    #   crr <- currentRasterStack$FUELMAP
    # }
    ### ignition add click -----
    if (input$map_mode == "ignitionPoint") {
      pt4326 <- terra::vect(matrix(c(lng, lat), ncol = 2), crs = "EPSG:4326")
      
      pt <- pt4326 |> terra::project(crr)
      ptmt <- as.matrix(unname(as.data.frame(pt@pntr$coordinates())))
      ptmt4326 <- as.matrix(unname(as.data.frame(pt4326@pntr$coordinates())))
      
      cr <- terra::cellFromXY(crr , ptmt)
      if (is.na(cr) || is.na(crr[cr][[1]])) {
        sendSweetAlert(
          session = session,
          title = "Sorry!",
          html = T,
          text = HTML(
            "Ignition point is not over a valid value but over
                            an NA value, will not add ignition point.<br>
          <b>Please make sure it overlaps data in input rasters.</b>"
          ),
          type = "warning"
        )
        return(NA)
      }
      allig <- isolate(ignitionPointsCoords())
      df <- data.frame(
        Year = as.integer(1),
        Ncell = as.integer(cr),
        X = ptmt4326[1, 1],
        Y = ptmt4326[1, 2]
      )
      
      nn <- rbind(allig, df)
      
      ignitionPointsCoords(nn)
      
    }
    
    
    ## make sure map mode returns to null
    # runjs("Shiny.setInputValue('map_mode', '', {priority: 'event'});")
    
  })
  
  # pretty raster info ----
  output$raster_info <- renderUI({
    # info <- rasterInfo()
    div(
      style = "
        background-color:#f8f9fa;
        border-left:6px solid #1f77b4;
        padding:12px;
        border-radius:6px;
        font-family:Arial;
      ",
      
      h4("FOREST Raster stack information", style = "margin-top:0;"),
      
      tags$b("Dimensions: "),
      info$nrow,
      " × ",
      info$ncol,
      tags$br(),
      tags$b("Resolution: "),
      info$res,
      tags$br(),
      tags$b("Extent: "),
      HTML(info$extent),
      tags$b("CRS: "),
      tags$code(info$crs),
      tags$br(),
      
      if (!is.null(info$classes)) {
        tagList(tags$hr(), tags$b("Classes:"), tags$ul(lapply(info$classes, tags$li)))
      }
    )
  })
  
  
  # DATASET MANAGEMENT ------
  ### delete dataset -----
  observeEvent(input$deletefolder_dataset, {
    if (!isTruthy(input$inputfolder)) {
      showNotification(
        "Please select a dataset from the drop down menu",
        type = "warning",
        duration = 20
      )
      return(NULL)
    }
    print("asdgfasdf")
    
    shinyWidgets::confirmSweetAlert(
      session = session,
      inputId =  "confirmDelete_dataset",
      title = "Confirm",
      text =  paste("Confirm you want to delete dataset", input$inputfolder)
    )
  })
  
  observeEvent(input$confirmDelete_dataset, {
    req(input$confirmDelete_dataset)
    showNotification(paste("Removing folder ", input$inputfolder), duration =
                       10)
    if (nchar(input$inputfolder) > 4)
      unlink(input$inputfolder, recursive = T)
    loadInstances()
    
  })
  
  ### download_landscape_flammap -----
  output$download_landscape_flammap  <- downloadHandler(
             filename = function() {
               sprintf("LANDSCAPE_FARSITE_%s.zip", basename(input$inputfolder))
             },
             content = function(file) {
               req(input$inputfolder)
               ff <- list.files(input$inputfolder, full.names = T, pattern="\\.tif$")
               if (length(ff) < 8) {
                 showNotification(
                   paste0(
                     "Download aborted: Found only ",
                     length(ff),
                     " raster files. At least 8 are needed for parsing a landscape file."
                   ), 
                   type = "error",duration = 20
                 )
               }
              
               ls <- which( names(rasters) %in% 
                              landscapeFlamMap )
               if(length(ls)!= 8){
                 
                 showNotification(paste(setdiff(landscapeFlamMap, names(rasters)), 
                                        collapse = " - "), " missing: landscape file cannot be created.",
                                  duration=22, type="warning" )
                 return()
               }
            
               if(!file.exists(file.path(input$inputfolder, "output", "landscape.tif" ))){
                 showNotification( "Creating and compressing stack of landscape file",
                                   duration=10, id="flammap" )
                 stack <- terra::rast( unlist(rasters[intersect(landscapeFlamMap, names(rasters))]) )
                 names(stack)<-intersect(landscapeFlamMap, names(rasters))
                 stack$CBD <- stack$CBD*100 
                 stack$CHM <- stack$CHM*10 
                 stack$CBH <- stack$CBH*10 
                 stack$FUEL[ is.na(stack$FUEL)|stack$FUEL==0 ] <- 99
                 uv <- unique(values(stack$FUEL))
                 uvdiff <- setdiff(uv, scott_burgan_models)
                 if(length(uv) > 40 ){
                   showNotification( "Wrong classes found in FUEL model file: supposed to be 40 classes from Scott&Burgan, found",
                                     length(uv), "classes. Uncompatible classes are: ",
                                     paste(uvdiff, collapse=","),"Please check", duration=20, 
                                     id="flammap", type="warning" )
                   
                 }
                 showNotification( "Saving landscape file",
                                   duration=10, id="flammap" )
                 writeRaster(stack, datatype="INT4U",
                             filename = 
                               file.path(input$inputfolder, "output", "landscape.tif" )
                 )
                  
               } 
               
               showNotification( "Compressing stack of landscape file",
                                 duration=10, id="flammap" )
               zip(file, 
                   file.path(input$inputfolder, "output", "landscape.tif" ),
                   flags = "-r9Xj")
             }
           )
           
  
  ### download_landscape ForeFire -----
  output$download_landscape_forefire  <- downloadHandler(
    filename = function() {
      sprintf("LANDSCAPE_FOREFIRE_%s.zip", basename(input$inputfolder))
    },
    content = function(file) {
      req(input$inputfolder)
      ff <- list.files(input$inputfolder, full.names = T, pattern="\\.tif$")
      if (length(ff) < 2) {
        showNotification(
          paste0(
            "Download aborted: Found only ",
            length(ff),
            " raster files. At least 2 are needed for parsing a landscape ForeFire file."
          ),
          type = "error",duration = 20
        )
      }
      
      ls <- which( names(rasters) %in% 
                     landscapeForeFire )
 
      if(length(ls) < 2){
        showNotification( "At least FUEL and ELEVATION rasters must be present, seems they are not.",
                          duration=10, id="flammap" )
        return(invisible())
      } 
      outLandscapeFile <- file.path(input$inputfolder, "output", "ForeFireLandscape.nc" )
      if(!file.exists(outLandscapeFile)){
        showNotification( "Creating and compressing stack of landscape file",
                          duration=10, id="flammap" )
        stack <- terra::rast( unlist(rasters[c("FUEL","ELEVATION")]) )
        names(stack)<- c("fuel","altitude")   
 
        createNetCDF4ForeFire(stack$fuel, stack$altitude,
                              outLandscapeFile )
        
        showNotification( "Saving landscape file for ForeFire in NetCDF",
                          duration=10, id="flammap" )
      
       
        #  library(ncdf4)
        # nc1 <- ncdf4::nc_open("/archivio/shared/R/Cell2FireR/inst/app/data/TC03_CZ_wildfire/output/data.nc")
        # print(nc1) # Look for the "variables" section
        # nc_close(nc1)
        # nc2 <- ncdf4::nc_open("/archivio/shared/R/Cell2FireR/inst/app/data/TC03_CZ_wildfire/output/ForeFireLandscape.nc")
        # print(nc2) # Look for the "variables" section
        #   nc_close(nc2)

 
        file.copy("templates/fuelForeFire.csv",
                  file.path(input$inputfolder, "output", "fuels.csv" ) )

        file.copy("templates/ForeFireParams.ff",
                  file.path(input$inputfolder, "output", "params.ff" ) )
      }
 
      
      wt <- weatherDataTable()
      
      iso_string <- strftime(anytime::anytime(wt$datetime[[1]]), 
                             format = "%Y-%m-%dT%H:%M:%SZ" )
      
      fp <- file(file.path(input$inputfolder, "output", "run.ff" ), "wt" )
      writeLines("include[params.ff]", fp)
      writeLines(sprintf("loadData[%s;%s]",
                         basename(outLandscapeFile),
                         iso_string), fp)
       
      ign <- ignitionPointsCoords()
      writeLines(sprintf("startFire[lonlat=(%.6f,%.6f,0.);t=0]",
                          ign$X, ign$Y), fp)
      
      for(i in 1:nrow(wt)){
        spd <- wt[i,"WS"]
        dir <- wt[i,"WD"]
        
        writeLines(
          sprintf("trigger[wind;vel=(%.5f,%.5f,0.)]@t=%d",
                  wt[i,"WS"]*cos(wt[i,"WD"]*pi/180),
                  wt[i,"WS"]*sin(wt[i,"WD"]*pi/180),
                  (i-1)*3600),
          fp
        )
        
        writeLines("step[dt=3600]", fp)
        writeLines( sprintf("print[final_front%002dh.json]@t=%d", i, (i)*3600), fp)
     
      }
      
      writeLines("print[final_front.json]", fp)
      flush(fp) 
      close(fp)
      # file.copy(file.path(input$inputfolder, "output", "landscapeForeFire.nc" ), file)
      showNotification( "Compressing stack of landscape file",
                        duration=10, id="flammap" )

      # runForeFire()
      zip(file,
          c(input$chooseWeatherFile,
            outLandscapeFile,
            file.path(input$inputfolder, "output", "fuels.csv" ) ,
            file.path(input$inputfolder, "output", "run.ff" ),
            file.path(input$inputfolder, "output", "params.ff" )
            ),
          flags = "-r9Xj")
    }
  )
  
  ### download dataset -----
  output$downloadfolder_dataset <- downloadHandler(
    filename = function() {
      sprintf("%s.zip", basename(input$inputfolder))
    },
    content = function(file) {
      req(input$inputfolder)
      zip(file, list.files(input$inputfolder, full.names = T))
    }
  )
  
  ### upload dataset -----
  observeEvent(input$zipfileload_dataset, {
    req(input$zipfileload_dataset)
    
    
    path <- file.path("data",
                      tools::file_path_sans_ext(input$zipfileload_dataset$name))
    if (dir.exists(path)) {
      showNotification(
        paste(
          "Directory ",
          input$zipfileload_dataset$name,
          " exists, please delete it first or change the name of the zip file!"
        ),
        type = "error",
        duration = 15
      )
      return(NULL)
    }
    fs <- file.size(input$zipfileload_dataset$datapath)
    showNotification(
      paste(
        "Unzipping ",
        ifelse(fs / 1e6 > 1, paste0(round(fs / 1e6, 1), " MB "), paste0(round(fs /
                                                                                1e3, 1), " kB")),
        "  in data folder to ",
        input$zipfileload_dataset$name
      ),
      duration = 10, id = "unzip"
    )
    
    unzip(input$zipfileload_dataset$datapath, exdir = path)
    
    showNotification(paste("Finished unzipping  to ", input$zipfileload_dataset$name), 
                      duration = 10, id = "unzip")
    if (length(list.files(path = path, pattern = ".*fuel.*\\.(asc|tif)$", ignore.case = T)) == 0) {
      dd <- list.dirs(path = path, recursive = F)
      if (length(dd) == 0) {
        showNotification(
          "<b>No fuel raster found!</b> - make sure that   your zip does not have subfolders and  a TIF or ASC file with the word  fuel  e.g. myfuel.tif or fuelINT.asc is available in the instance.",
          type = "error",  duration = 20
        )
        
        if (nchar(path) > 4)  unlink(path, recursive = TRUE)
        return()
      } 
      
      dd <- list.dirs(path = path, recursive = F)
      subdd <- list.files(path = dd, pattern = "fuel.*\\.(asc|tif)$", ignore.case = T)
      if (length(subdd) == 0) {
        showNotification(
          "<b>No fuel rasters found!</b> - make sure that   your zip does not have subfolders and  a TIF or ASC file with the word  fuel  e.g. myfuel.tif or fuelINT.asc is available in the instance.",
          type = "error",
          duration = 20
        )
        if (nchar(path) > 4)  unlink(path, recursive = TRUE)
        return()
      }  
    
      files_to_move <- list.files(path = dd, full.names = T)
      dest_paths <- file.path(path, basename(files_to_move))
      
      # 4. Move the files
      # file.rename returns TRUE if successful
      success <- file.rename(from = files_to_move, to = dest_paths)
      # Check results
      if (all(success)) {
        showNotification(
          "<b>Files in subfolder</b> copied to main folder successfully",
          type = "success",
          duration = 20
        )
        
        loadInstances()
      } else {
        showNotification(
          "<b>Some files could not be moved, please check your zip contents, no subfolders should be present",
          type = "error",
          duration = 20
        )
        if (nchar(path) > 4)
          unlink(path, recursive = TRUE)
      }
      unlink(dd)
    
 
      # browser()
      
    } else {
      showNotification(paste("LOADING TO ", input$zipfileload$name), duration=10, id = "unzip")
      
      loadInstances()
    }
  })
  
  ## monitor DATA FOLDER -----
  # folders <- reactivePoll(
  #   intervalMillis = 10000,   # check every 10 seconds
  #   session,
  #
  #   # Check function (fast, lightweight)
  #   checkFunc = function() {
  #     if (!dir.exists(base_path)) return(NULL)
  #     file.info(base_path)$mtime   # modification time
  #   },
  #
  #
  #   # Value function (runs only if check changes)
  #   valueFunc = function() {
  #     if (!dir.exists(base_path)) return(character(0))
  #     dirs <- list.dirs("data",full.names = T,recursive = F)
  #     names(dirs) <- gsub("_", " ", basename(dirs))
  #     dirs
  #   }
  # )
  
  
  # RUN CELL2FIRE ------
  observeEvent(input$runsim, {
    # cmd <- "FlamMap.exe /in:input.fmp /out:output /log:log.txt"
    if (!isTruthy(input$inputfolder)) {
      shinyWidgets::sendSweetAlert(title = NULL,
                                   text = "Please choose a dataset to run simulations!",
                                   status = "warning")
      return(NULL)
      
    }
    
    if (!is.null(simProcess)) {
      shinyWidgets::ask_confirmation("confirmKillProc", text = "Process is running, do you want to stop it?", status = "warning")
    } else {
      
      cat("------------", this.path::this.dir(), " .... \n", file="mylog3.log")
      proc(F)
      updateActionButton(
        inputId = "runsim",
        label = paste("<span class=spin>🔥</span> Running ", input$simulator)
      )
    }
  })
  
  observeEvent(input$confirmKillProc, {
    if (input$confirmKillProc) {
      killSimProcess(T)
    }
  })
  ## data folder observe -----
  # observeEvent(folders(), {
  
  loadInstances <- function()  {
    current <- isolate(input$inputfolder)
    dirs <- list.dirs("data", full.names = T, recursive = F)
    names(dirs) <- gsub("_", " ", basename(dirs))
    updateSelectInput(
      session,
      "inputfolder",
      choices = c("", dirs),
      selected = if (current %in% dirs)
        current
      else
        NULL
    )
  }
  
  loadInstancesSimulationOutputs <- function(uponSimulationFinisched = FALSE)  {
    req(input$inputfolder)
    current <- isolate(input$inputfolder)
    dirs <- list.dirs(outfolder, full.names = T, recursive = F)
    names(dirs) <- gsub("_", " ", basename(dirs))
    updateSelectInput(
      session,
      "outputInstanceFolder",
      choices = c("", dirs) ,
      selected = ifelse(uponSimulationFinisched, dirs[[length(dirs)]], "")
    )
    
  }
  
  observeEvent({
    all_input_names <- unlist(lapply(PANELS, names), use.names = FALSE)
    lapply(all_input_names, function(inp) {
      input[[inp]]
    })
    input$tabs
  }, {
 
   req(input$tabs == "inputInstancesInputArgs") 
      proc(TRUE) 
  }, ignoreInit = T)
  
  
  
  # ROS RASTER-----
  observeEvent(input$addROStoMAP, {
    fp <- file.path(
      input$outputInstanceFolder,
      "results",
      "RateOfSpread",
      sprintf("ROSFile%d.tif", input$addROStoMAP$simulationN)
    )
    if (!file.exists(fp)) {
      showNotification(
        paste0(
          "ROS file <b>",
          basename(fp) ,
          "</b> does not exist in results! Did you post-process the simulation instance?
  (tools in the second panel that apprears when you click the top right gear icon)"
        ),
        type = "warning",
        duration = 20
      )
    } else {
      r <-stars::read_stars( fp )
      # if(!st_is_longlat(r)) {
     r <- st_transform(r, 3857)
     v <- spatSample(terra::rast(fp),
                     size = 1e4,
                     method = "regular",
                     na.rm = TRUE)
     
     rrange <- quantile(v[, 1], probs = c(0.1, 0.9))
     palette = viridisLite::turbo(20)
     domain  = rrange
     copts <- colorOptions(
       palette = palette,
       domain = unname(domain),
       na.color = "#00000000"
     )
     
      leaflet::leafletProxy("map") |>
        # clearGroup(sim_layers$ROS)  |>
        leafem::addGeoRaster(
          r ,
          opacity = 0.85,
          group = sim_layers$ROS,
          autozoom = TRUE,
          colorOptions = copts,
          options = leafletOptions(pane = "fire_ROS_pane"),
          tileOptions = leaflet::tileOptions(zIndex = 999),
          imagequeryOptions = leafem::imagequeryOptions(
            digits = 0,
            prefix =
              "",
            position =
              "bottomright",
            noData = "NA"
          ) ,
          layerId = gsub("[^[:alnum:]_-]", "", sim_layers$ROS)
        )
      
    }
  }) 
  
  # FIRE SPREAD VECTOR -----
  
  observeEvent(input$playGrids, {
    fp <- file.path(
      input$outputInstanceFolder,
      "results",
      "Grids",
      sprintf("Grids%d", input$playGrids$simulationN)
    )
    if (!dir.exists(fp)) {
      showNotification(
        paste0(
          "Directory <b>",
          sprintf("Grids%d", input$playGrids$simulationN) ,
          "</b> does not exist in results!"
        ),
        type = "warning",
        duration = 20
      )
    } else {
      
      print("nonoo")
      req(input$outputInstanceFolder)
      print("hhhhh")
      currentVect <- isolate(simulation_output_grids())
      if(!is.null(currentVect) && !isTruthy(input$playGrids$step)) {
        simulation_output_grids(NULL)
      }  else {
        vecpath <-  file.path(fp, 
                       paste0("Grids", input$playGrids$simulationN, ".gpkg") )  
        
        if(!file.exists(vecpath)){
          showNotification(file.path("Could not read file ", basename(vecpath), "<br>Did you run post-processing? (tools in the second panel that appears when you click the top right gear icon)?"), 
                           type = "info", duration = 20 ) 
          return(invisible())
        }
        
        
        fm <- as.integer(input$playGrids$step) 
        print(fm)
   
        if(is.null(currentVect) || 
           attr(currentVect, "source_file") != vecpath ){
          
          fire_vect <- sf::read_sf(vecpath) 
          attr(fire_vect, "source_file") <- vecpath
          simulation_output_grids( fire_vect |> arrange(timeStep) |> sf::st_transform(4326)) 
          current_frame(fm)  
        } else {
          fire_vect <- currentVect
          shinyjs::runjs( sprintf("$('#simulationTableDateSpan%d').html('%s');", 
                                  fire_vect[fm,]$simNumber ,
                                  as.character(fire_vect[fm,]$Date) 
          ) )
          current_frame(fm) 
        } 
        
        print(fire_vect[fm,])
      }
      
    }
  }) 
  
  observeEvent(current_frame(), {
    
    r <- simulation_output_grids()
    req(r)
    
    frame_idx <- current_frame()
    req(frame_idx <= nrow(r))
    
    
    # Filter the sf object to just this frame
    active_poly <- r[frame_idx, ]
    
    leafletProxy("map") %>%
      clearGroup(sim_layers$SimBurntArea) %>%
      addPolygons(data = active_poly, fillColor = "red",color = "red",
                    opacity = 0.2,
                  fillOpacity = 0.05,  weight = 2,stroke = TRUE,
                  options = pathOptions(pane = "fire_spread_pane"), 
                  group = sim_layers$SimBurntArea)
     
  })
  
  ## END SESSION ----
  # observe({
  onSessionEnded(function() {
    if (!is.null(logs$log_con)) close(logs$log_con)
    saveState()
    # cleanup qui
  })
  # })

  loadInstances()
}
