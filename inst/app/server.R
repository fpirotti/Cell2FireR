# server.R

server <- function(input, output, session) {
  ## AUTH ------
  # source("functions_auth.R", local=T)
  ## LOAD STATE ------
  source("functions_state.R", local=T)
  uniqueId <- paste0(format(Sys.time(), "%H%M%S"),substr(session$token, 1, 6))
  outfolder<-NULL
  
  options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB
  # rasterInfo <- reactiveVal(NULL)
  weatherDataTable <- reactiveVal( 
    data.frame(
      Instance ="",
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
      FireScenario =""
    )  
  )
  currentRasterStack <- NULL
  lut_fbp_local <- reactiveVal(NULL)
  logs <- list( logfile = file.path(this.path::this.dir(), "sessionLogs",  
                       paste0(uniqueId, "Logfile.log") ) ,
                logfileLn = 0,
                logfileSim = file.path(this.path::this.dir(), "sessionLogs",  
                                       paste0(uniqueId, "LogfileSim.log") ),
                loffileSimLn = 0)
  
   
  logs$log_con = file(logs$logfile, "a")
  
  ## dummy start/stop process
  simProcess <- NULL
 
  
  file.create(logs$logfileSim, showWarnings = FALSE)
  logs$t
  ignitionFiles <- NULL
  weatherFiles <- NULL
  rasters <- list()
  terra.rasters <- list()
  ignitionPointsCoords <- reactiveVal(NULL)
  
  rasterFiles <- reactiveVal(NULL)
  csvFiles <- reactiveVal(NULL)
  weatherFiles <- reactiveVal(NULL)
  ignitionFiles <- reactiveVal(NULL)
  lut_generic <- lut_fbp
  
  ## reactive Polling
  observe({
    invalidateLater(1000) 
    
    if (is.null(simProcess)) return()  
    
    # 1. Smarter safe read functions
    # These check if the connection is actually valid BEFORE reading
    read_out_safe <- function(proc) {
      if (proc$has_output_connection() && proc$is_incomplete_output()) {
        res <- tryCatch(proc$read_output_lines(), error = function(e) NULL)
        if (is.null(res)) return(character(0)) else return(res)
      }
      return(character(0))
    }
    
    read_err_safe <- function(proc) {
      if (proc$has_error_connection() && proc$is_incomplete_error()) {
        res <- tryCatch(proc$read_error_lines(), error = function(e) NULL)
        if (is.null(res)) return(character(0)) else return(res)
      }
      return(character(0))
    }
    
    # 2. Helper function to format the payload
    build_payload <- function(out_lines, err_lines) {
      payload <- list()
      if (length(out_lines) > 0) {
        payload$out <- lapply(out_lines, function(x) list(type = "out", text = x))
      }
      if (length(err_lines) > 0) {
        payload$err <- lapply(err_lines, function(x) list(type = "err", text = x))
      }
      return(payload)
    }
    
    # Read standard out and standard error safely
    out <- read_out_safe(simProcess)
    err <- read_err_safe(simProcess)
    
    # Send payload if there's anything to send
    payload <- build_payload(out, err)
    if (length(payload) > 0) {
      session$sendCustomMessage("appendLog", payload)
    }
    
    # ---- process finished? ----
    if (!simProcess$is_alive()) {
      
      # Final drain loop with Safety Limit
      max_drains <- 50 
      drain_count <- 0
      
      repeat {
        drain_count <- drain_count + 1
        if (drain_count > max_drains) break
        
        drain_out <- read_out_safe(simProcess)
        drain_err <- read_err_safe(simProcess)
        
        if (length(drain_out) == 0 && length(drain_err) == 0) {
          break
        }
        
        drain_payload <- build_payload(drain_out, drain_err)
        if (length(drain_payload) > 0) {
          session$sendCustomMessage("appendLog", drain_payload)
        }
      }
      
      # Send termination message
      session$sendCustomMessage("appendLog", list( 
        "out" = list(list(type = "out", text = "--- process finished ---"))
      ))
      
      # Clean up state
      simProcess <<- NULL
      updateActionButton(inputId = "runsim",label = paste("🔥 Run ", input$simulator)  )
    }
  })

  
  ## reactive log file  ----
  log_reader <- reactivePoll(
    intervalMillis = 1000,  # 1 second
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
  source("functions_makeCmd.R", local=T)
  
 
  ## render HTML to log ----
  output$log <- renderUI({  
    lines <- log_reader()
    tt <- tags$div(
      lapply(lines, function(line) {
        cls <- dplyr::case_when(
          grepl("ERROR", line, ignore.case = T) ~ "log-error",
          grepl("WARN",  line, ignore.case = T) ~ "log-warn",
          grepl("INFO",  line, ignore.case = T) ~ "log-info",
          TRUE                  ~ "log-default"
        )
        tags$div(class = cls, HTML(line))
      })
    )
    
    tags$div(
      id="logbox",
      tt
    )
  })
  
 
  showNotification<-function(..., type="message", duration=0){
    text <- paste(..., collapse = " ")
    if( !is.element(type, c("default", "message", "warning", "error")) ){
      typesh <- "default"
    } else {
      typesh <- type
    }

    if(duration>0) {
      shiny::showNotification(
                     HTML(text),
                     duration = duration,
                     type = typesh
                      )
    }
     
    if(duration>10) {
      shinyWidgets::sendSweetAlert(text = HTML(text), title=type,html = T)
    } 
    if(!is.null(logs$logfile)   ){
      writeLines(
        c( paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), toupper(type), "; 
  ",
        paste(text, collapse = " ") ), "

" ),
        con = logs$log_con
      )
      flush(logs$log_con)
    }

  }
 
  # change simulator -----
  observeEvent(input$simulator, {
    
      if(input$simulator=="FlamMap"){
        shinyWidgets::inputSweetAlert(inputId = "flammapPath",
                                      text="Please insert the exact path to FlamMap 
software in your MS Windows machine (the same where you are running your R app). 
Work in progress....",  input="textarea")
      }    
    
    updateActionButton(inputId = "runsim",label = paste("🔥 Run ", input$simulator)  )
    
  })
  
  # IGNITION files stack -----
  observeEvent(ignitionFiles(),{
    ign <- ignitionFiles()
    names(ign) <- basename(ign)

    
    shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile",
                                    choices = ign,
                                    clearOptions = T  )
     
    if(length(ign)>0){ 
      ip <- read.csv(ign[[1]] )
      if(nrow(ip)!=0){ 
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
          duration = 2
        )
      } 
      
    }
    
    
    sel <- ign[[1]] 
    if(isTruthy(input$IGNIPOINT)) sel <- input$IGNIPOINT
    shinyWidgets::updatePickerInput(inputId = "IGNIPOINT",
                                    choices = ign, 
                                    selected = sel  )
     
  })
  
  observeEvent(weatherFiles(),{ 
    wf <- weatherFiles()
    if(length(wf)!=0) { 
      df <-  read.csv(wf[[1]] ) 
      weatherDataTable(df) 
      names(wf) <- basename(wf)
      shinyWidgets::updatePickerInput(inputId = "chooseWeatherFile",
                                      choices = wf,
                                      clearOptions = T  )
      sel <- wf[[1]] 
      if(isTruthy(input$WEAFILE)) sel <- input$WEAFILE
      shinyWidgets::updatePickerInput(inputId = "WEAFILE",
                                      choices = wf, 
                                      selected = sel  )
      
      
    } else {
      showNotification(
        paste0("NO WEATHER FILE! A TEMPLATE WITH ONE LINE IS LOADED ..."),
        type = "warning") 
      f<-file.path(input$inputfolder, "Weather.csv")
      weatherFiles(f)
      df <- read.csv("templates/Weather.csv", nrows = 1)
      write.csv(df,   f, row.names = FALSE )
      weatherDataTable(df)
    }

    
  })
  # change RASTER stack -----
  observeEvent(rasterFiles(),{
    req(rasterFiles()) 
    
    
    ### READ RASTERS ----
    withProgress(message = 'Adding layers',  value = 0, {
      ## UPDATE LEAFLET ------
      leaflet::leafletProxy("map") |>
        leaflet::clearImages()  |>
        leaflet::clearControls() |> addFWI()
      
      crs <- NA
      layerNames <- clcLayerName
      rasters <<- list()
      rfiles<- rasterFiles()
      names(rfiles) <- basename(rfiles)
      for(wg in names(SIM_INPUTS)){
        # print(toupper(wg))
        shiny::updateSelectInput(inputId = toupper(wg),
                                        choices =  rfiles, selected=""  )
      }
      strt <- 0 
      for(fi in rfiles){
        strt <- strt + 1  
        layerName <- paste0(basename(input$inputfolder), " - ", toupper(tools::file_path_sans_ext(basename(fi))))
        
        layerNames <- c(layerNames, layerName) 
        incProgress( strt/(length(rfiles)+2), detail = sprintf("Adding layer %s", layerName) )
        
        r2 <- terra::rast(fi)
        terra.rasters[[layerName]] <<- r2
        ## check alignment between rasters -----
        if(length(rasters)>0){
          if(!compareGeom(terra::rast(rasters[[1]]), r2, stopOnError = FALSE) ){
            showNotification(
              HTML(
              sprintf("<b>Rasters NOT aligned!</b> Either CRS, 
origin or resolution are 
different between raster %s 
and raster %s", 
                    basename(sources(rasters[[1]])) , 
                    basename(sources(r1))
                      )
                   ),
              duration = 12,
              type = "error"
            )
            break
          } 
        }
        
        
        
        if(terra::crs(r2)==""){
          showNotification(
            HTML("<b>Raster Info:</b> NO 🌐 CRS provided to raster, so a generic pseudo-mercator projection assigned: 
<code>EPSG:3857</code>.  Work in progress for assigning a CRS 🌐 manually - keep tuned!"),
            duration = 0,
            type = "warning"
          )
          # guess CRS of input
          cc<-center(r2)
          if( abs(cc$lng) > 360 && abs(cc$lat) > 90  ){
            terra::crs(r2) <- "EPSG:3857"
          }
        }
        ## LANDSCAPE FILES ------
        #### FUEL map ------
        if(grepl("forest|fuel", layerName, ignore.case = T) ){
          r2 <- terra::as.factor(r2) 
 
          levs <- data.frame(
            value = lut_fbp$grid_value,
            class = lut_fbp$descriptive_name,
            fuel = lut_fbp$fuel_type,
            color =   rgb(lut_fbp$r, lut_fbp$g, lut_fbp$b, maxColorValue = 255)
          )
          
          
          lut_generic <- lut_fbp
          if(isScottBurgan(r2)){ 
            levs <- data.frame(
              value = lut_sb$grid_value,
              class = as.character(lut_sb$descriptive_name),
              fuel = lut_sb$fuel_type,
              color =   rgb(lut_sb$r, lut_sb$g, lut_sb$b, maxColorValue = 255)
            )
            lut_generic <- lut_sb
          }
          
          nl <- levs[1, ]
          nl$value <- 0; nl$class<- "N.A."; nl$fuel<- "N.A."; nl$color<- "#00000000";
          if(nrow(levs[levs$value==0,])==0) {
            levs <- rbind(nl , levs)
          } else {
            levs[levs$value==0,] <- nl 
          }
          levels(r2) <- levs
          # browser()
          copts <- leafem::colorOptions(palette = levs$color, domain=levs$value,
                                        na.color = "#00000000") 
          
          na.color = "transparent"
          rasters[["FUEL"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "FUEL", 
                                          choices =  rfiles ,
                                          selected =   rasters[["FUEL"]]   )
       
          
        } else {
          v <- spatSample(r2, size = 1e4, method = "regular", na.rm = TRUE)
          
          rrange <- quantile(v[,1], probs = c(0.1,0.9))
          palette = viridisLite::viridis(20)
          domain  = rrange 
          copts <- colorOptions(palette=palette,
                                domain=unname(domain),
                                na.color = "#00000000")
        }
        
        
        if(grepl("elev|dtm|dem", layerName, ignore.case = T) ){ 
          rasters[["ELEVATION"]] <<- terra::sources(r2)[[1]] 
          shiny::updateSelectInput(inputId = "ELEVATION", 
                                          choices =  rfiles ,
                                          selected =  rasters[["ELEVATION"]]  )
        }  else if(grepl("cbd|bulk", layerName, ignore.case = T) ){ 
          rasters[["CBD"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CBD", 
                                          choices =  rfiles ,
                                          selected =  rasters[["CBD"]]  )
        }  else if(grepl("cbh|base", layerName, ignore.case = T) ){ 
          rasters[["CBH"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CBH", 
                                          choices =  rfiles ,
                                          selected =  rasters[["CBH"]]  )
        }  else if(grepl("ccf|cover", layerName, ignore.case = T) ){ 
          rasters[["CCF"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CCF", 
                                          choices =  rfiles ,
                                          selected =  rasters[["CCF"]]   )
        }  else if(grepl("ch|height", layerName, ignore.case = T) ){ 
          rasters[["CHM"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "CHM", 
                                          choices =  rfiles ,
                                          selected =  rasters[["CHM"]]  )
        }  else if(grepl("breaks", layerName, ignore.case = T) ){ 
          rasters[["FIREBREAKS"]] <<- terra::sources(r2)[[1]]
          shiny::updateSelectInput(inputId = "FIREBREAKS", 
                                          choices =  rfiles ,
                                          selected = rasters[["FIREBREAKS"]] )
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
        
        view_settings[[layerName]] <- list(coords = as.numeric(st_transform(st_bbox(r2), 4326))   )
        
        leaflet::leafletProxy("map") |>
          leafem::addGeoRaster( stars::st_as_stars(r2),
                                colorOptions = copts,
                                # pixelValuesToColorFn = pv2col,
                                opacity = 0.85, 
                                group = layerName,
                                autozoom = F,
                                options = leafletOptions(pane = "markerPane"),
                                tileOptions = leaflet::tileOptions(zIndex = 999),
                                imagequeryOptions = leafem::imagequeryOptions(digits=0,
                                                                              prefix="",
                                                                              position="bottomright",
                                                                              noData = "NA"),
                                layerId = gsub(" ", "", layerName))
          if(!grepl("forest|fuel", layerName, ignore.case = T) ){
            leaflet::leafletProxy("map")   |> leaflet::hideGroup( layerName )
          }
           
      }
    })
    
    currentRasterStack <<- tryCatch({
      terra::rast(terra.rasters)
    },
    error=function(e){
      showNotification(
        paste("ERROR:", e$message),
        type = "error",
        duration = 16
      )
      NULL
    })
    if(is.null(currentRasterStack)) return(NA)
     
    r2 <- terra::project(terra::ext(r2), from=terra::crs(r2), to="epsg:4326")
    
    leafletProxy("map") |>
      
      addLayersControl(
        baseGroups = unlist(unname(base_layers)),
        overlayGroups = c(layerNames, names(fwi_layers)),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      )   |>
      customizeLayersControl(
        view_settings =view_settings ,
        # home_btns = TRUE,
        opacityControl = opacityControl,
        includelegends = TRUE,
        addCollapseButton = TRUE,
        layersControlCSS = list("opacity" = 0.6),
        increaseOpacityOnHover = TRUE
      ) |>
      leaflet::fitBounds( lng1 =  xmin(r2),
                          lat1 =  ymin(r2),
                          lng2 =  xmax(r2),
                          lat2 =  ymax(r2)
      )  
    
    session$sendCustomMessage("layersControlReady", list())
    # shinyjs::runjs("autoGroupLeafletLayers(\".leaflet-control-layers\");")
    
    if(is.null(currentRasterStack)){
      showNotification(
        "No Forest raster file found, please read  documentation on how to prepare a dataset",
        type = "error",
        duration = 12
      )
    }
    
  })
  
  # CROWN  ----
  observeEvent(input$CROWN, {
    if(!input$CROWN){
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
  
  
  # change INSTANCE folder ----
  observeEvent(input$outputInstanceFolder, {
    req(input$outputInstanceFolder) 
    
    
    
  })
  
  # change DATASET folder ----
  observeEvent(input$inputfolder, {
    req(input$inputfolder) 
    
    
    showNotification(text=paste0("
========================================
====== DATASET:  ", basename(input$inputfolder) ," 
========================================")  )
    outfolder <<- file.path(input$inputfolder, "output")
    dir.create(outfolder, recursive = TRUE, showWarnings = FALSE) 

    
    shiny::updateSelectInput(inputId = "outputInstanceFolder", choices = 
                               c("", basename(list.dirs(outfolder,recursive = F))) )
    
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
    if(input$map_mode==""){
      runjs("document.getElementById('map').style.cursor = null;")
    }
  })
  
  # tooltips  ----
  observeEvent(list(input$tooltips, 
                    input$tooltipsSize), {
    print(input$tooltips)
    if(input$tooltips){ 
      shinyjs::runjs( sprintf("   makeTooltips(%d); ttinstances.forEach(i => i.enable());", input$tooltipsSize )  )
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
  observeEvent( list(input$openmeteoInput, 
                     input$WMSQueryReturned ), 
                {
                  
        if(! (shiny::isTruthy(input$openmeteoInput)||
              shiny::isTruthy(input$WMSQueryReturned) ) ){
          return(invisible(NULL))
        } 
                  
        df <- isolate(weatherDataTable())
       dfl <- isolate( as.list(df[1,]) )
      
       
       if( shiny::isTruthy(input$openmeteoInput  ) ){ 
         if(!is.list(input$openmeteoInput)){ 
           msg <- paste("❌ API error:", input$openmeteoInput  ) 
           showNotification(msg, type = "error")
           return(invisible(NULL))
         }
         outp <- "OM"
         dflt <-   list(
           Instance = "SB",
           datetime = gsub("T", " ", input$openmeteoInput$current$time),
           TMP = input$openmeteoInput$current$temperature_2m,
           WS = input$openmeteoInput$current$wind_speed_10m,
           WD = input$openmeteoInput$current$wind_direction_10m,
           RH = input$openmeteoInput$current$relative_humidity_2m,
           FireScenario = "open meteo + EFFIS"
         )
         
         dfl[ na.omit(names(dflt)) ] <- dflt[ na.omit(names(dflt)) ]
       }
       
       if( shiny::isTruthy(input$WMSQueryReturned  ) ){  
         
         outp <- "EFFIS"
         doc <- xml2::read_html(input$WMSQueryReturned$datast)  
         rows <- xml2::xml_find_all(doc, ".//tr") 
         kv_list <- lapply(rows, function(row) {
           cols <- xml_find_all(row, ".//td")
           if(length(cols) >= 2) {
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
         names(dflt)<- inside_text
         dfl[ na.omit(names(dflt)) ] <- dflt[ na.omit(names(dflt)) ]
       }
        
       weatherDataTable( as.data.frame(dfl) )
  })
  
  ## TABLE WEATHER table -----
  output$weather.table <- renderDT({ 
    datatable( 
              weatherDataTable() , 
              editable = TRUE, 
              options = list(
                initComplete = JS(
                  "function(settings) {",
                  "  var th = $('#weatherTableOutputDIV thead th').filter(function() {",
                  "    $(this).attr('data-tippy-content', tooltipsStrings[$(this).text().trim() ] );  ",
                  "  });",
                 ifelse(input$tooltips, sprintf(" makeTooltips(%d); ", input$tooltipsSize), ""),
                  "}"
                 
               )
              )
              )|> formatRound(
      columns = c('DC', 'FWI', 'DMC', 'ISI', 'BUI', 'FFMC'),
      digits = 2
    )
  })
  observeEvent(input$chooseWeatherFile, {   
    df <- read.csv(input$chooseWeatherFile) 
    weatherDataTable(df)
    shinyWidgets::updatePickerInput(inputId = "WEAFILE",
                                    # choices = weatherFiles,
                                    clearOptions = T, 
                                    selected = input$chooseWeatherFile  )
  })
  ## TABLE FBP table ----
  output$FBP.table <- renderDT({
    req(lut_fbp_local())
    datatable( lut_fbp_local() , editable = TRUE)
  })

  ## TABLE IGNITION and edits  ----
  output$ignitionInfo <- renderDT({
    # req(ignitionPointsCoords())
    ip <- ignitionPointsCoords()
    if(is.null(ip)){
      ip <- data.frame(X=numeric(), Y=numeric(), Tools=character())
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
        columnDefs = list(
          list(
            targets = ncol(ip),
            render = JS(
              "function(data, type, row, meta) {
                      return '<button onclick=\"mymap.flyTo(['+row[4]+', '+(parseFloat(row[3])+0.02)+'], 14);\">🔎</button>';
           }"
            )
          )
        )
      )
    ) |> formatRound(
      columns = c('X', 'Y'),
      digits = 5
    )
    # datatable( ip , editable = TRUE)
  })
  
  observeEvent(input$delete_table_ignition_row, {  
    df <- isolate(ignitionPointsCoords())
    df <- df[ -1*input$ignitionInfo_rows_selected, ]   
    ignitionPointsCoords(df)
  })

  
  observeEvent(input$ignitionInfo_cell_edit, {
    info <- input$ignitionInfo_cell_edit
    df <- isolate(ignitionPointsCoords())
    df[info$row, info$col] <- info$value
    ignitionPointsCoords(df)
  })
  
  save_table_ignition_final <- function(overwrite=F){
    df <- isolate(ignitionPointsCoords())
    if(overwrite && isTruthy(input$chooseIgnitionFile)) {
      write.csv(df, input$chooseIgnitionFile, row.names = F)
    } else {
      write.csv(df, file.path(input$inputfolder, 
                              sprintf("ignitionPoints_%s.csv", 
                                      format(Sys.time(), 
                                             "%Y-%m-%d_%H-%M-%S" ) )), row.names = F)
    }
    

    
    ignitionFiles <<- list.files(
      path = input$inputfolder,
      pattern = ".*ignition.*\\.(csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    ) 
    names(ignitionFiles) <<- basename(ignitionFiles)
    shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile",
                                    choices = ignitionFiles,
                                    clearOptions = T  )
    shinyWidgets::updatePickerInput(inputId = "IGNIPOINT",
                                    choices = ignitionFiles,
                                    clearOptions = T, 
                                    selected = ignitionFiles[[1]]  )
  }
  
  
  observeEvent(input$delete_table_ignition, {
    req(input$chooseIgnitionFile)
    shinyWidgets::ask_confirmation(inputId = "delete_table_ignition_confirm",html = T,
                                    sprintf("Confirm you want to delete the selected ignition table: <br><u><b>%s</b></u>",
                                           basename(input$chooseIgnitionFile)  )
                                   )

  })
  
  observeEvent(input$delete_table_ignition_confirm, {
    req(input$chooseIgnitionFile)
    if(input$delete_table_ignition_confirm){
     file.remove(input$chooseIgnitionFile) 
      
      ignitionFiles <<- list.files(
        path = input$inputfolder,
        pattern = ".*ignition.*\\.(csv)$",
        full.names = TRUE,
        ignore.case = TRUE
      ) 
      names(ignitionFiles) <<- basename(ignitionFiles)
      shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile",
                                      choices = ignitionFiles,
                                      clearOptions = T  )
      shinyWidgets::updatePickerInput(inputId = "IGNIPOINT",
                                      choices = ignitionFiles,
                                      clearOptions = T, 
                                      selected = ignitionFiles[[1]]  )
     ignitionPointsCoords(NULL)
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
  
  ## updates side bar ignition ----
  observeEvent(input$ignitionsTable, {
    print("Ignitions Table")
    updateTabsetPanel(session, "tabs", selected = "dashboardMap")
    updateBoxSidebar("ignitionSideBar")
  })
  
  observeEvent(input$save_table_ignition, {
    if(!isTruthy(input$chooseIgnitionFile) ) {
      save_table_ignition_final(F)
    } else {  
      showModal(
        md_overwrite_ignition
      )
       
    }
  })

  ## add ignitionpoints -----
  observe( {
    req(ignitionPointsCoords())
    ip<-ignitionPointsCoords()
    ipt <- na.omit(ip)
    leafletProxy("map") |> leaflet::clearMarkers()
    for(i in 1:nrow(ipt)){
      # print(ipt[i,  ])
      leafletProxy("map") |>
        addAwesomeMarkers(  as.numeric(ipt[i, 3] ),
                   as.numeric(ipt[i, 4]),
                   icon=  fireIcon,
                   popup = paste0(
                     popup_text("Year:",  ipt[i, 1]  ),
                     "<br>",
                     popup_text("NCell:",  ipt[i, 2]  ),
                     "<br>",
                     popup_text("Lat:", round(as.numeric(ipt[i, 3]),6) ),
                     "<br>",
                     popup_text("Lon:",  round(ipt[i, 4], 6) ) )
      )
    }
  })
  # Handle map mode and clicks ----
  observeEvent(input$map_click, {
    req(input$map_mode)
    lng <- input$map_click$lng
    lat <- input$map_click$lat

    if ( input$map_mode == "WMSQuery") {
      js <- sprintf("queryWMS(%f, %f, '%s')", lat, lng, paste(fwi_layers, collapse=",")   )
      # print(js)
      shinyjs::runjs(js)
    }

    if(is.null(currentRasterStack)){
      showNotification(
        paste0(
          "You did not load a dataset with rasters. Only coordinates returned: ",
          sprintf("%.6f", lat), ", ",
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
    if ( input$map_mode == "ignitionPoint") {

      pt4326 <- terra::vect( matrix(c(lng, lat),ncol = 2), crs="EPSG:4326" )

      pt <- pt4326 |> terra::project( crr )
      ptmt <- as.matrix( unname(as.data.frame(pt@pntr$coordinates()) ) )
      ptmt4326 <- as.matrix( unname(as.data.frame(pt4326@pntr$coordinates()) ) )
 
      cr <- terra::cellFromXY( crr , ptmt )
      if(is.na(cr) || is.na(crr[cr][[1]])){
        sendSweetAlert(
          session = session,
          title = "Sorry!", html=T,
          text = HTML("Ignition point is not over a valid value but over 
                            an NA value, will not add ignition point.<br> 
          <b>Please make sure it overlaps data in input rasters.</b>"),
          type = "warning"
        ) 
        return(NA)
      }   
      allig <- isolate(ignitionPointsCoords())
      df <- data.frame(Year=as.integer(1), Ncell=as.integer(cr), X=ptmt4326[1,1],  Y=ptmt4326[1,2])
       
      nn <- rbind(allig, df )

      ignitionPointsCoords( nn )

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

      tags$b("Dimensions: "), info$nrow, " × ", info$ncol, tags$br(),
      tags$b("Resolution: "), info$res, tags$br(),
      tags$b("Extent: "), HTML(info$extent),
      tags$b("CRS: "), tags$code(info$crs), tags$br(),

      if (!is.null(info$classes)) {
        tagList(
          tags$hr(),
          tags$b("Classes:"),
          tags$ul(
            lapply(info$classes, tags$li)
          )
        )
      }
    )
  })


  ## delete dataset -----
  observeEvent(input$deletefolder, {
    if(input$inputfolder==""){
      showNotification("Please select a dataset from the drop down menu",
                       type="warning")
      return(NULL)
    } 
        shinyWidgets::confirmSweetAlert(session=session,
                                        "confirmDelete",
                                        "Confirm",
     paste("Confirm you want to delete dataset", input$inputfolder ))
  })

  observeEvent(input$confirmDelete, {
    print(input$inputfolder)
      req(input$inputfolder)
      showNotification("Removing folder " )
      unlink( file.path("data", input$inputfolder) )
      loadInstances()
 
  })
  ## download dataset -----
  observeEvent(input$downloadfolder, {
    req(input$inputfolder)
    fp <- file.path("data", input$inputfolder )
    req(fp!="data")
    zip(sprintf("%s.zip", fp), list.files(fp, full.names = T) )
  })
  ## upload dataset -----
  observeEvent(input$zipfileload, {
    req(input$zipfileload)
   
   
    path <- file.path("data", tools::file_path_sans_ext(input$zipfileload$name) )
    if(dir.exists(path)){
      showNotification(paste("Directory ", input$zipfileload$name, " exists, please delete it first or change the name of the zip file!"),
                       type = "error",duration = 15
      )
      return(NULL)
    }
    fs <- file.size(input$zipfileload$datapath)
    showNotification(
      paste("Unzipping ", fs/1e6 ," MB in data folder to ", input$zipfileload$name)
      )

    unzip(input$zipfileload$datapath, exdir=path )

    showNotification(
      paste("Finished unzipping  to ", input$zipfileload$name)
    )
    
    if( length( list.files(path=path, pattern = "fuel.*\\.(asc|tif)$") )==0 ){
      # browser()
      showNotification(
        "<b>No fuel raster found!</b> - make sure that   your zip does not have subfolders and  a TIF or ASC file with the word  fuel  e.g. myfuel.tif or fuelINT.asc is available in the instance.",
        type = "error", duration = 15
      )
      
      if(nchar(path)>4) unlink(path, recursive = TRUE)
      
    } else { 
      showNotification(
        paste("LOADING TO ", input$zipfileload$name)
      )
      
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
    if(!isTruthy(input$inputfolder)){
      shinyWidgets::sendSweetAlert(
        title=NULL, 
        text="Please choose a dataset to run simulations!", 
                                   status = "warning")
      return(NULL)
       
    }
    
    if(!is.null(simProcess)){ 
      shinyWidgets::ask_confirmation("confirmKillProc", 
        text="Process is running, do you want to stop it?", 
        status = "warning")
    } else { 
      proc(F)  
      updateActionButton(inputId = "runsim",label = paste("<span class=spin>🔥</span> Running ", input$simulator)  )
    }           
  })
  
  observeEvent(input$confirmKillProc,
               {
                 if(input$confirmKillProc) {
                   simProcess$kill()
                   updateActionButton(inputId = "runsim",label = paste("🔥 Run ", input$simulator)  )
                 
                   
                    session$sendCustomMessage("appendLog", list( "out"=
                                                                  list( list(type="out", text="--- process finished by user ---") ),
                                                                "err"=
                                                                  list( list(type="out", text="--- process finished by user ---") )
                   ))
                   
                   simProcess <<- NULL
                   showNotification(
                     "Simulation stopped!",
                     type = "info",
                     duration = 10
                   )
                 }
               })
  ## data folder observe -----
  # observeEvent(folders(), {
    
  loadInstances <- function()  {
    current <- isolate(input$inputfolder)
    dirs <- list.dirs("data",full.names = T,recursive = F)
    names(dirs) <- gsub("_", " ", basename(dirs))
    updateSelectInput(
      session,
      "inputfolder",
      choices = c("", dirs),
      selected = if (current %in% dirs) current else NULL
    )

  } 

  
  observeEvent({ 
    all_input_names <- unlist(lapply(PANELS, names), use.names = FALSE) 
    lapply(all_input_names, function(inp) {  
      input[[inp]]
      })
  }, {
     print(proc(TRUE))
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
