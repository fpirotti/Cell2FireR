#' Get the Cell2Fire Data Directory
#' @export
c2f_dir <- function() {
  
  file.path(tools::R_user_dir("packageName", which = "data"), "C2F")
}

#' Get the Cell2Fire Binary Path
#' @export
c2f_bin_pathEnv <- function() {
  if (.Platform$OS.type == "windows") {
    bin_location <- file.path(file.path(tools::R_user_dir("packageName", which = "data"), "C2F"),
                              "Cell2Fire.exe")
  } else {
    bin_location <- system.file("bin", "C2F", "Cell2Fire", package = "Cell2FireR")
  }
  return(bin_location)
}




.onLoad <- function(libname, pkgname) {
   
  outdir <- file.path(tools::R_user_dir(pkgname, which = "data"), "C2F") 
  exewinfile <- file.path(outdir, "Cell2Fire.exe")
  
  
  if(!dir.exists(outdir)){
    dir.create(outdir, recursive=TRUE )
  } else{
    if(file.exists(exewinfile)){
      return()
    }
  }
 
  getCell2Fire(outdir)  

}
