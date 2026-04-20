#' getCell2Fire
#' @description
#' Downloads cell2fire from the GitHub release, extracts it, and installs 
#' the binaries into the target directory.
#' 
#' @param outdir output directory where the executables will be copied.
#'  Defaults to the user's specific data directory for this package.
#' @param quiet logical; whether to suppress download messages. Default is FALSE.
#'
#' @returns TRUE if successful, FALSE otherwise (invisibly).
#' @export
getCell2Fire <- function(
    outdir=file.path(tools::R_user_dir("Cell2FireR", which = "data"), "C2F"),
    quiet = FALSE) {
  zip_path <- file.path(tempdir(), "Cell2FireW_v1.0.3.zip")
  extract_dir <- file.path(tempdir(), "Cell2FireR")
   
  out <- tryCatch({
    
    # 1. Download the file
    # mode = "wb" is absolutely critical on Windows for binary/zip files
    if (!quiet) message("Downloading Cell2Fire binary (10.2 MB)...")
    utils::download.file(
      "https://github.com/fire2a/C2F-W/releases/download/v1.0.3/Cell2FireW_v1.0.3.zip",
      destfile = zip_path,
      mode = "wb", 
      quiet = quiet
    )
    
    # 2. Extract the file
    if (!quiet) message("Extracting binaries...")
    utils::unzip(zipfile = zip_path, exdir = extract_dir)
    
    # 3. Define paths
    fp <- file.path(extract_dir, "C2F", "Cell2Fire")
    exe.linux <- file.path(fp, "Cell2Fire")
    exe.win <- file.path(fp, "Cell2Fire.exe")
    
    # Check for missing files
    if (!file.exists(exe.linux)) {
      warning("Linux compiled Cell2Fire does not exist in the downloaded archive.")
    }
    if (!file.exists(exe.win)) {
      warning("Windows compiled Cell2Fire does not exist in the downloaded archive.")
    }
     
    # Ensure the destination directory exists
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # 4. Copy files to the persistent user directory
    # Linux/Mac files
    Sys.chmod(list.files(fp, pattern = "*\\.so$|^Cell2Fire$", full.names = TRUE), mode = "0777", use_umask = TRUE)
    Sys.chmod(list.files(fp, pattern = "*\\.dll$|*\\.exe$", full.names = TRUE), mode = "0777", use_umask = TRUE)
    
    file.copy(list.files(fp, pattern = "*\\.so$|^Cell2Fire$", full.names = TRUE),
              to = outdir, overwrite = TRUE)
    # Windows files
    file.copy(list.files(fp, pattern = "*\\.dll$|*\\.exe$", full.names = TRUE),
              to = outdir, overwrite = TRUE)
    
    TRUE
    
  }, error = function(e) { 
    message("Error installing Cell2Fire: ", e$message) 
    return(FALSE) 
  }, finally = {
    # 5. CRAN requirement: Clean up temporary workspace
    if (file.exists(zip_path)) unlink(zip_path, force = TRUE)
    if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE, force = TRUE)
  })
  
  # 6. Success/Failure messaging
  # Use message() instead of packageStartupMessage() since this is a manual function call
  if (out) { 
    if (!quiet) message("Installation of the Cell2Fire executable was successful!")
  } else { 
    warning("Something went wrong in the installation of Cell2Fire, contact the developers.")
  }
  browser()
  return(invisible(out))
}