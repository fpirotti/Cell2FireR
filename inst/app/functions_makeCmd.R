# Helper function mapping the UI fuel choices to Cell2Fire's internal string keys
# (This replaces the NAME["fuel_model_key"] from the Python script)
get_fuel_key <- function(fuel_string) {
  fuel_map <- c(
    "0. Scott & Burgan" = "ScottBurgan",
    "1. Kitral" = "Kitral",
    "2. Canada FBP" = "FBP",
    "3. Portugal" = "Portugal"
  )
  return(unname(fuel_map[fuel_string]))
}
 
proc <- function() {   
  browser()
    req(input$FUEL, input$FUEL_MODEL, input$inputfolder)
    showNotification("Building Simulation Instance...", id = "sim_status", duration = NULL)
    
    tryCatch({
      # -------------------------------------------------------------
      # 1. SETUP DIRECTORIES
      # -------------------------------------------------------------
      # Create an instance directory similar to QGIS: firesim_yymmdd_HHMMSS
      timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
      instance_dir <- file.path(outfolder, paste0("firesim_", timestamp))
      results_dir <- file.path(instance_dir, "results")
        
      dir.create(instance_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Helper to copy and rename files to standard names expected by C2F
      copy_to_instance <- function(filepath, standard_name) {
        if (!is.null(filepath) && filepath != "") {
          ext <- tools::file_ext(filepath)
          file.copy(filepath, file.path(instance_dir, paste0(standard_name, ".", ext)))
        }
      }
      
      # -------------------------------------------------------------
      # 2. COPY LANDSCAPE INPUTS
      # -------------------------------------------------------------
      copy_to_instance(input$FUEL, "fuels")
      copy_to_instance(input$ELEVATION, "elevation")
      
      if (input$CROWN) {
        copy_to_instance(input$CBH, "cbh")
        copy_to_instance(input$CBD, "cbd")
        copy_to_instance(input$CCF, "ccf")
        copy_to_instance(input$CHM, "hm")
      }
      
      if (input$IGNITION_MODE == "1. Probability map distributed random ignition") {
        copy_to_instance(input$IGNITIONFILE, "probabilityMap")
      }
      
      # Handling Firebreaks (Translating 'raster_layer_to_firebreak_csv')
      if (!is.null(input$FIREBREAKS) && input$FIREBREAKS != "") {
        # Using terra to read raster, find cells == 1, and write coordinates/ids to CSV
        fb_rast <- terra::rast(input$FIREBREAKS)
        fb_cells <- terra::cells(fb_rast, 1) # Get cell indices where value is 1
        # Cell2Fire requires 1-based indices or specific row/col CSVs depending on the version
        write.csv(data.frame(Cell = fb_cells), file.path(instance_dir, "firebreaks.csv"), row.names = FALSE)
      }
      
      # -------------------------------------------------------------
      # 3. WEATHER & IGNITIONS
      # -------------------------------------------------------------
      if (input$WEATHER_MODE == "0. Single weather file") {
        req(input$WEAFILE)
        file.copy(input$WEAFILE, file.path(instance_dir, "Weather.csv"))
      } else {
        # Random draw from directory (Looks in the directory of the selected dataset)
        wea_dir <- dirname(input$WEAFILE)
        wea_out <- file.path(instance_dir, "Weathers")
        dir.create(wea_out, showWarnings = FALSE)
        
        wea_files <- list.files(wea_dir, pattern = "^Weather[0-9]*\\.csv$", full.names = TRUE)
        if (length(wea_files) > 0) {
          file.copy(wea_files, wea_out)
        } else {
          stop("Multiple weathers requires a directory with Weather[0-9]*.csv files!")
        }
      }
      
      if (input$IGNITION_MODE == "2. Single point on a Layer") {
        req(input$IGNIPOINT)
        # Assuming IGNIPOINT is a vector file. We extract its coordinate, 
        # map it to the fuel raster to get the cell ID.
        fuel_rast <- terra::rast(input$FUEL)
        ign_pt <- sf::st_read(input$IGNIPOINT, quiet = TRUE)
        # Match CRS
        ign_pt <- sf::st_transform(ign_pt, terra::crs(fuel_rast))
        
        # Get cell index mapping (Cell2Fire uses 1-based indexing top-left to bottom-right)
        cell_id <- terra::cellFromXY(fuel_rast, sf::st_coordinates(ign_pt)[1, 1:2])
        
        # Write Ignitions.csv
        writeLines(c("Year,Ncell", paste0("1,", cell_id)), file.path(instance_dir, "Ignitions.csv"))
      }
      
      # -------------------------------------------------------------
      # 4. BUILD COMMAND LINE ARGUMENTS
      # -------------------------------------------------------------
      # Core Arguments
      args <- list(
        "--sim" = get_fuel_key(input$FUEL_MODEL),
        "--nsims" = input$NSIM,
        "--seed" = input$RNG_SEED,
        "--nthreads" = input$SIM_THREADS,
        "--fmc" = input$FMC,
        "--scenario" = input$LDFMCS
      )
      
      # Boolean / Mode Flags
      if (input$CROWN) args[["--cros"]] <- ""
      
      if (input$IGNITION_MODE == "2. Single point on a Layer") {
        args[["--ignitions"]] <- ""
        args[["--IgnitionRad"]] <- input$IGNIRADIUS
      }
      
      if (input$WEATHER_MODE == "0. Single weather file") {
        args[["--weather"]] <- "rows"
      } else {
        args[["--weather"]] <- "random"
      }
      
      if (!is.null(input$FIREBREAKS) && input$FIREBREAKS != "") {
        args[["--FirebreakCells"]] <- shQuote(file.path(instance_dir, "firebreaks.csv"))
      }
      
      # Append Selected Outputs
      if (!is.null(input$OUTPUTS)) {
        for (opt in input$OUTPUTS) {
          args[[paste0("--", opt)]] <- ""
        }
      }
      
      # Directories
      args[["--input-instance-folder"]] <- shQuote(instance_dir)
      args[["--output-folder"]] <- shQuote(results_dir)
      
      # Flatten list to a character vector for system2
      cli_args <- character()
      for (name in names(args)) {
        if (args[[name]] == "") {
          cli_args <- c(cli_args, name) # Append as a flag (no value)
        } else {
          cli_args <- c(cli_args, name, as.character(args[[name]]))
        }
      }
      
      # -------------------------------------------------------------
      # 5. EXECUTE CELL2FIRE
      # -------------------------------------------------------------
      # Change this path if your Cell2Fire binary lives elsewhere
      c2f_bin <- if(.Platform$OS.type == "windows") "Cell2Fire.exe" else "./Cell2Fire"
      
      # Optional: Print command to R console for debugging
      cat("Executing:", c2f_bin, paste(cli_args, collapse = " "), "\n")
      
      removeNotification("sim_status")
      showNotification("Simulation running...", id = "sim_status", duration = NULL, type = "message")
      
      # Execute via system2. Output is pushed to LogFile.txt
      exit_code <- system2(
        command = c2f_bin,
        args = cli_args,
        stdout = file.path(results_dir, "LogFile.txt"),
        stderr = "2>&1",
        wait = TRUE
      )
      
      if (exit_code == 0) {
        removeNotification("sim_status")
        showNotification("Simulation Finished Successfully! Results are in the output folder.", type = "message")
        
        # NOTE: At this point, you would read the shapefiles/rasters from `results_dir` 
        # and render them to a leaflet map.
        
      } else {
        removeNotification("sim_status")
        showNotification(paste("Simulation Failed. Exit code:", exit_code), type = "error")
      }
      
    }, error = function(e) {
      removeNotification("sim_status")
      showNotification(paste("Error preparing simulation:", e$message), type = "error")
    })
  }  