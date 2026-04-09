# server.R

server <- function(input, output, session) {
  ## AUTH ------
  # source("functions_auth.R", local=T)
  ## LOAD STATE ------
  source("functions_state.R", local=T)
  options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB
  # rasterInfo <- reactiveVal(NULL)
  ignitionFiles <- NULL
  weatherFiles <- NULL
  rasters <- list()
  terra.rasters <- list()
  ignitionPointsCoords <- reactiveVal(NULL)
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
  logfile <- reactiveVal(NULL)
  rasterFiles <- reactiveVal(NULL)
  weatherFiles <- reactiveVal(NULL)
  ignitionFiles <- reactiveVal(NULL)
  lut_generic <- lut_fbp
  ## reactive log file  ----
  log_reader <- reactivePoll(
    intervalMillis = 1000,  # 1 second
    session = session,

    # cheap check: did the file change?
    checkFunc = function() {
      if(is.null(logfile())) return(NULL)
      if ( !file.exists(logfile())) return(NULL)
      file.info(logfile())$mtime
    },

    # expensive read: only when it DID change
    valueFunc = function() {
      if(is.null(logfile())) return("No logfile yet")
      if ( !file.exists(logfile())) return("Log file does not exist yet")
      readLines(logfile(), warn = FALSE)
    }
  )
  ## reactive log html ----
  log_html <- reactive({
    lines <- log_reader()

    tags$div(
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
  })

  ## render HTML to log ----
  output$log <- renderUI({
    tags$div(
      id="logbox",
      log_html()
    )
  })

  showNotification<-function(..., type="message", duration=4){
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
    
    
    if(duration>10 && type=="error") {
      shinyWidgets::alert(HTML(text), status="danger")
    }
    logf <- isolate(logfile())
    if(!is.null(logf) && dir.exists(dirname(logf) ) ){
      cat(
        format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), toupper(type),
        " | ",
        paste(text, collapse = " "),
        "\n",
        file = logf,
        append = TRUE
      )
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
    
    updateActionButton(inputId = "runsim",label = paste("Run ", input$simulator)  )
    
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
      strt <- 0 
      for(fi in rfiles){
        strt <- strt + 1  
        layerName <- paste0(basename(input$inputfolder), " - ", toupper(tools::file_path_sans_ext(basename(fi))))
        
        layerNames <- c(layerNames, layerName) 
        incProgress( strt/(length(rfiles)+2), detail = sprintf("Adding layer %s", layerName) )
        
        r2 <- terra::rast(fi)
        terra.rasters[[layerName]] <- r2
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
          levels(r2) <- levs
          copts <- leafem::colorOptions(palette = levs$color, domain=levs$value) 
          
          na.color = "transparent"
          rasters[["FUEL"]] <- terra::sources(r2)[[1]]
          shinyWidgets::updatePickerInput(inputId = "FUEL", 
                                          choices =  rfiles ,
                                          selected = basename(rasters[["FUEL"]]) )
          
        } else {
          v <- spatSample(r2, size = 1e4, method = "regular", na.rm = TRUE)
          
          rrange <- quantile(v[,1], probs = c(0.1,0.9))
          palette = viridisLite::viridis(20)
          domain  = rrange 
          copts <- colorOptions(palette=palette,
                                domain=unname(domain),
                                na.color = "#eaeaea00")
        }
        
        
        if(grepl("elev|dtm|dem", layerName, ignore.case = T) ){ 
          rasters[["ELEVATION"]] <- terra::sources(r2)[[1]] 
          shinyWidgets::updatePickerInput(inputId = "ELEVATION", 
                                          choices =  rfiles ,
                                          selected = basename(rasters[["ELEVATION"]]) )
        }  else if(grepl("cbd|bulk", layerName, ignore.case = T) ){ 
          rasters[["CBD"]] <- terra::sources(r2)[[1]]
          shinyWidgets::updatePickerInput(inputId = "CBD", 
                                          choices =  rfiles ,
                                          selected = basename(rasters[["CBD"]]) )
        }  else if(grepl("cbh|base", layerName, ignore.case = T) ){ 
          rasters[["CBH"]] <- terra::sources(r2)[[1]]
          shinyWidgets::updatePickerInput(inputId = "CBH", 
                                          choices =  rfiles ,
                                          selected = basename(rasters[["CBH"]]) )
        }  else if(grepl("ccf|cover", layerName, ignore.case = T) ){ 
          rasters[["CCF"]] <- terra::sources(r2)[[1]]
          shinyWidgets::updatePickerInput(inputId = "CCF", 
                                          choices =  rfiles ,
                                          selected = basename(rasters[["CCF"]]) )
        }  else if(grepl("ch|height", layerName, ignore.case = T) ){ 
          rasters[["CHM"]] <- terra::sources(r2)[[1]]
          shinyWidgets::updatePickerInput(inputId = "CHM", 
                                          choices =  rfiles ,
                                          selected = basename(rasters[["CHM"]]) )
        }  else if(grepl("breaks", layerName, ignore.case = T) ){ 
          rasters[["FIREBREAKS"]] <- terra::sources(r2)[[1]]
          shinyWidgets::updatePickerInput(inputId = "FIREBREAKS", 
                                          choices =  rfiles ,
                                          selected = basename(rasters[["FIREBREAKS"]]) )
        } else {
          rasters[[layerName]] <- terra::sources(r2)[[1]] 
        }
        
         
        opacityControl [[layerName]] <- list(
          min = 0,
          max = 1,
          step = 0.1,
          default = 1,
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
                                layerId = gsub(" ", "", layerName)) |>
          leaflet::hideGroup( layerName )
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
    
    r2 <- terra::project(ext(r2), from=terra::crs(r2), to="epsg:4326")
    
    
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
        "No Forest raster file found, please read Cell2Fire documentation on how to prepare a dataset",
        type = "error",
        duration = 6
      )
    }
    
  })
  # change dataset folder ----
  observeEvent(input$inputfolder, {
    req(input$inputfolder)
    loadState() 
    outfolder <- file.path(input$inputfolder, "output")
    dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(outfolder, "Logfile.log"), showWarnings = FALSE)
    logfile(file.path(outfolder, "Logfile.log"))
    if(dir.exists(outfolder)){
      showNotification(
        paste0("Directory ", outfolder, " exists, files will be overwritten if you continue!"),
        type = "warning",
        duration = 8
      )
    } else {
      showNotification(
        paste0("Output Directory: <b>", outfolder, "</b> does not exist and will be created"),
        type = "INFO",
        duration = 0
      )
    }
 
 
    ip <- NULL
    currentRasterStack <<- NULL

    # rfiles <- list.files(
    #   path = input$inputfolder,
    #   pattern = "\\.(asc|tif|tiff)$",
    #   full.names = TRUE,
    #   ignore.case = TRUE
    # )
    rasterFiles(
      list.files(
        path = input$inputfolder,
        pattern = "\\.(asc|tif|tiff)$",
        full.names = TRUE,
        ignore.case = TRUE
     )
    )
    csvfiles <- list.files(
      path = input$inputfolder,
      pattern = "\\.(csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    ignitionFiles <<- list.files(
      path = input$inputfolder,
      pattern = ".*ignition.*\\.(csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    weatherFiles <<- list.files(
      path = input$inputfolder,
      pattern = ".*weather.*\\.(csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    ) 
    
    if(length(weatherFiles)!=0) { 
      df <-  read.csv(weatherFiles[[1]] ) 
      weatherDataTable(df) 
    } else {
      showNotification(
        paste0("NO WEATHER FILE! I LOADED A TEMPLATE..."),
        type = "warning",
        duration = 8 )
      weatherFiles <- file.path(input$inputfolder, "Weather.csv")
      df <- read.csv("templates/Weather.csv", nrows = 1)
      write.csv(df,   weatherFiles, row.names = FALSE )
      weatherDataTable(df)
    }
    names(weatherFiles) <- basename(weatherFiles)
    shinyWidgets::updatePickerInput(inputId = "chooseWeatherFile",
                                    choices = weatherFiles,
                                    clearOptions = T  )
    
    shinyWidgets::updatePickerInput(inputId = "WEAFILE",
                                    choices = weatherFiles,
                                    clearOptions = T, 
                                    selected = weatherFiles[[1]]  )
    
 
    names(ignitionFiles) <<- basename(ignitionFiles)
    shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile",
                                    choices = ignitionFiles,
                                    clearOptions = T  )

    ## IGNITION FILE ----
    # ignitionFile <-  grep("ignition", basename(tolower(csvfiles)), ignore.case = T )
    if(length(ignitionFiles)>0){
      
      shinyWidgets::updatePickerInput(inputId = "IGNIPOINT",
                                      choices = ignitionFiles,
                                      clearOptions = T, 
                                      selected = ignitionFiles[[1]]  )
      
      ip <- read.csv(ignitionFiles[[1]] )
      if(!is.null(ip)){
        
        if(!is.element("X", names(ip)) ){
          ncell <- ip$Ncell
          r2 <- terra::rast(raster$FUEL)
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



    shinyjs::runjs("$('.info.legend.rastervals.leaflet-control').remove();")



    ## FBP FILE ----
    fbpFile <-  grep("fbp_lookup_table", basename(tolower(csvfiles)), ignore.case = T )
    if(length(fbpFile)>0){
      lut_fbp_local(read.csv(csvfiles[[fbpFile[[1]]]] ))
    } else {
      showNotification(
        paste0("Did not find file fbp_lookup_table.csv! Will fall back to default template file in 'templates' folder."),
        type = "warning",
        duration = 1
      )
      lut_fbp_local(lut_generic)
    }



  })

  ##render leaflet -----
  output$map <- renderLeaflet({
    mymap
  })

  # CLICK mode ----
  observeEvent(input$map_mode, {
    print(input$map_mode)
    if(input$map_mode==""){
      runjs("document.getElementById('map').style.cursor = null;")
    }
  })
  
  # tooltips  ----
  observeEvent(list(input$tooltips, 
                    input$tooltipsSize), {
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
      extensions = "Buttons",
      editable = FALSE ,
      options = list(
        dom = "Bfrtip",
        columnDefs = list(
          list(
            targets = ncol(ip),
            render = JS(
              "function(data, type, row, meta) {
                      return '<button onclick=\"mymap.flyTo(['+row[4]+', '+row[3]+'], 10);\">🔎</button>';
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
  })
  
  observeEvent(input$overwrite_file_confirm_newFile, {
    save_table_ignition_final(F) 
  })
  ## updates side bar ignition ----
  observeEvent(input$ignitionsTable, {
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
      print(ipt[i,  ])
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
          text = "Ignition point is not over a valid value but over 
                            an NA value, will not add ignition point.<br> 
          <b>Please make sure it overlaps data in input rasters.</b>",
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
    print("here")
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
 
    print(input$zipfileload$name)
   
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
  })

  ## monitor DATA FOLDER -----
  folders <- reactivePoll(
    intervalMillis = 10000,   # check every 10 seconds
    session,

    # Check function (fast, lightweight)
    checkFunc = function() {
      if (!dir.exists(base_path)) return(NULL)
      file.info(base_path)$mtime   # modification time
    },


    # Value function (runs only if check changes)
    valueFunc = function() {
      if (!dir.exists(base_path)) return(character(0))
      dirs <- list.dirs("data",full.names = T,recursive = F)
      names(dirs) <- gsub("_", " ", basename(dirs))
      dirs
    }
  )
  
  
  # RUN CELL2FIRE ------
  observeEvent(input$runsim, {
     cmd <- "FlamMap.exe /in:input.fmp /out:output /log:log.txt"
    Cell2FireR::cell2fire_run(input)
    # cell2fire_run(c("--input-instance-folder", input$inputfolder,
    #                 "--output-folder", "../Sub40x40", 
    #                 "--ignitions", "1",
    #                 "--sim-years", "1") )             
  })
  
  ## data folder observe -----
  observeEvent(folders(), {

    current <- input$inputfolder
    
    updateSelectInput(
      session,
      "inputfolder",
      choices = c("", folders()),
      selected = if (current %in% folders()) current else NULL
    )

  }, ignoreNULL = FALSE)


  ## END SESSION ----
  observe({
    session$onSessionEnded(function(inp=input) { 
      saveState()
      # cleanup qui
    })
  })

  
}
