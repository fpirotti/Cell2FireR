library(terra)
library(fs)



# leaflet() %>%
#   addTiles(
#     urlTemplate = "tiles/layer/{z}/{x}/{y}.png",
#     options = tileOptions(tms = FALSE)
#   )

make_tiles_if_big <- function(
    input_folder,
    output_folder,
    size_limit_mb = 200,
    zoom = "0-14",
    gdal2tiles = "gdal2tiles.py"
) {

  files <- dir_ls(input_folder,
                  recurse = TRUE,
                  regexp = "\\.(tif|asc)$",
                  type = "file")
  browser()
  if (length(files) == 0) {
    message("No rasters found.")
    return(invisible(NULL))
  }

  for (f in files) {
    browser()
    file_size_mb <- file_info(f)$size / (1024^2)

    message("Checking: ", f,
            " (", round(file_size_mb,1), " MB)")

    if (file_size_mb >= size_limit_mb) {

      r <- rast(f)
      name <- path_ext_remove(path_file(f))
      out_dir <- path(output_folder, paste0(name, "_tiles"))

      dir_create(out_dir)

      # optional reprojection for leaflet
      tmp <- tempfile(fileext = ".tif")

      project(r,
              tmp,
              method = "near",
              overwrite = TRUE,
              crs = "EPSG:3857")

      cmd <- paste(
        shQuote(gdal2tiles),
        "-z", zoom,
        "--processes=4",
        shQuote(tmp),
        shQuote(out_dir)
      )

      message("Creating tiles...")
      system(cmd)

      unlink(tmp)

    } else {
      message("Skipping (below size threshold).")
    }
  }
}
