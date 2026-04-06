#' Main Entry Point for Cell2FireR
#' @export
#'
#' @param input The input list/reactive values from Shiny
#'
#' @return The path to the results directory
#' @examples ## get help.
cell2fire_run <- function(input) {
  
  # 1. Setup Directories --------------------------------------------------
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  instance_dir <- file.path(input$inputfolder, paste0("firesim_", timestamp)) 
  ## output folder is always in instance folder with name = timestamp. 
  ## outputs are kept only for one week then removed
  results_dir <-   file.path(instance_dir, "results")  
  fs::dir_create(instance_dir)
  fs::dir_create(results_dir)
  
  # 2. Mappings (Translating UI to CLI args) ------------------------------
  fuel_model_map <- c(
    "0. Scott & Burgan" = "SB", 
    "1. Kitral"         = "Kitral",
    "2. Canada FBP"     = "FBP",
    "3. Portugal"       = "Portugal"
  )
  
  output_arg_map <- c(
    "Final Fire Scar" = "finalscar",
    "Propagation Fire Scars" = "propagationscars",
    "Propagation Directed Graph" = "propagationdigraph",
    "Ignition Points" = "ignitionpoints",
    "Hit Rate Of Spread" = "ros",
    "Surface Flame Length" = "flamelen"
    # Expand this list to match all STATS/OUTPUTS names to their flags
  )
  # 3. Handle Raster Copies -----------------------------------------------
  copy_raster <- function(src, dest_name) {
    if (!is.null(src) && src != "") {
      ext <- tools::file_ext(src)
      dest_path <- file.path(instance_dir, paste0(dest_name, ".", ext))
      fs::file_copy(src, dest_path, overwrite = TRUE)
    }
  }
  
  browser()
  copy_raster(input$FUEL, "fuels")
  copy_raster(input$ELEVATION, "elevation")
  
  if (input$CROWN) {
    copy_raster(input$CBH, "cbh")
    copy_raster(input$CBD, "cbd")
    copy_raster(input$CCF, "ccf")
    copy_raster(input$HM, "hm")
  }
  
  # 4. Firebreaks Raster to CSV Conversion --------------------------------
  if (!is.null(input$FIREBREAKS) && input$FIREBREAKS != "") {
    fb_rast <- terra::rast(input$FIREBREAKS)
    fb_cells <- which(terra::values(fb_rast) == 1) 
    
    fb_csv_path <- file.path(instance_dir, "firebreaks.csv")
    utils::write.table(fb_cells, fb_csv_path, row.names = FALSE, col.names = FALSE)
  }
  
  # 5. Build Base Command Arguments ---------------------------------------
  args <- c(
    "--input-instance-folder", shQuote(instance_dir),
    "--output-folder", shQuote(results_dir),
    "--sim", fuel_model_map[input$FUEL_MODEL],
    "--nsims", input$NSIM,
    "--seed", input$RNG_SEED,
    "--nthreads", input$SIM_THREADS,
    "--fmc", input$FMC,
    "--scenario", input$LDFMCS
  )
  
  if (input$CROWN) args <- c(args, "--cros")
  
  # Append Outputs Flags 
  selected_flags <- output_arg_map[input$OUTPUTS]
  for (flag in selected_flags) {
    if (!is.na(flag)) args <- c(args, paste0("--", flag))
  }
  
  if (!is.null(input$FIREBREAKS) && input$FIREBREAKS != "") {
    args <- c(args, "--FirebreakCells", shQuote(file.path(instance_dir, "firebreaks.csv")))
  }
  
  # 6. Ignition Logic -----------------------------------------------------
  if (input$IGNITION_MODE == "1. Probability map distributed random ignition") {
    copy_raster(input$IGNIPROBMAP, "probabilityMap")
    args <- c(args, "--ignitions")
    
  } else if (input$IGNITION_MODE == "2. Single point on a Layer") {
    args <- c(args, "--ignitions", "--IgnitionRad", input$IGNIRADIUS)
    
    if (!is.null(input$IGNIPOINT) && input$IGNIPOINT != "") {
      fuel_rast <- terra::rast(input$FUEL)
      ign_pts <- sf::st_read(input$IGNIPOINT, quiet = TRUE)
      
      if (sf::st_crs(ign_pts) != terra::crs(fuel_rast)) {
        ign_pts <- sf::st_transform(ign_pts, terra::crs(fuel_rast))
      }
      
      coords <- sf::st_coordinates(ign_pts)
      cell_ids <- terra::cellFromXY(fuel_rast, coords)
      
      ign_df <- data.frame(Year = rep(1, length(cell_ids)), Ncell = cell_ids)
      utils::write.csv(ign_df, file.path(instance_dir, "Ignitions.csv"), row.names = FALSE, quote = FALSE)
    }
  } else {
    args <- c(args, "--ignitions", "False") 
  }
  
  # 7. Weather Logic ------------------------------------------------------
  if (input$WEATHER_MODE == "0. Single weather file") {
    fs::file_copy(input$WEAFILE, file.path(instance_dir, "Weather.csv"), overwrite = TRUE)
    args <- c(args, "--weather", "rows")
  } else {
    args <- c(args, "--weather", "random") 
  }
  
  # 8. Execute ------------------------------------------------------------
  ext <- if (.Platform$OS.type == "windows") ".exe" else ""
  
  # Make sure "simulator/C2F" is available relative to where your package function is called, 
  # or use system.file() if the binary is bundled inside your R package.
  c2f_bin <- file.path("simulator", "C2F", paste0("Cell2Fire", ext))
  
  cat("Running command:\n", c2f_bin, paste(args, collapse = " "), "\n")
  browser()
  res <- sys::exec_wait(c2f_bin, args, std_out = TRUE, std_err = TRUE)
  
  if (res == 0) {
    return(results_dir)
  } else {
    stop("Cell2Fire execution failed. Check the console logs.")
  }
}