#' Title
#'
#' @param outdir output directory
#'
#' @returns Nothing
#'
#' @examples #none
getCell2Fire <- function(outdir) {
  dest <- file.path(tempdir(), "Cell2FireR")
  out <- tryCatch({
    
    if (!dir.exists(dest)) {
      utils::download.file(
        "https://github.com/fire2a/C2F-W/releases/download/v1.0.1/Cell2FireW_v1.0.1.zip",
        destfile = paste0(dest, ".zip")
      )
      utils::unzip(paste0(dest, ".zip"), exdir = dest)
    }
    
    fp <- file.path(dest, "C2F","Cell2Fire")
    exe.linux <- file.path(fp,"Cell2Fire")
    exe.win <- file.path(fp, "Cell2Fire.exe")
    
    if(!file.exists(exe.linux) ){
      warning("Linux compiled Cell2Fire does not exist")
    }
    
    if( !file.exists(exe.win)){
      warning("Windows compiled Cell2Fire does not exist")
    }
    Sys.chmod(exe.linux, mode = "0777", use_umask = TRUE)
    Sys.chmod(exe.win, mode = "0777", use_umask = TRUE)
    file.copy( list.files(fp, pattern="*\\.so$|^Cell2Fire$", full.names = T ) ,
               to = outdir, overwrite = T )
    file.copy( list.files(fp, pattern="*\\.dll$|*\\.exe$", full.names = T ) ,
               to = outdir, overwrite = T )
    TRUE
  },
  error = function(e) { message("  Error reading ground: ", e$message); return(FALSE) }
  )
  
  if(out){ 
    packageStartupMessage("Installation of the Cell2Fire C2F package,
successful")
  } else { 
    packageStartupMessage("Something went wrong in installation of Cell2Fire,
contact the developers")
  }
  

}
