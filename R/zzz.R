#' Get the Cell2Fire Data Directory
#' Get the Cell2Fire Binary Path
#' @export
c2f_bin_pathEnv <- function() {
  if (.Platform$OS.type == "windows") {
    bin_ <-  "Cell2Fire.exe"
  } else {
    bin_ <-  "Cell2Fire"
  }
  bin_location <- file.path(file.path(tools::R_user_dir("Cell2FireR", which = "data"), "C2F"),
                            bin_)
  return(bin_location)
}




.onLoad <- function(libname, pkgname) {
   
  outdir <- file.path(tools::R_user_dir(pkgname, which = "data"), "C2F")
  if(!dir.exists(outdir)){
    dir.create(outdir, recursive=TRUE )
  } 
  exewinfile <- file.path(outdir, "Cell2Fire.exe")
  verfile <- file.path(outdir, "ver.rda")
  verNow <- utils::packageVersion(pkgname)
 
  if(file.exists(verfile)){
    load(verfile)
    if(ver==verNow){
      return()
    }
    packageStartupMessage(
      paste0("Found new version of package... installing cell2fire")
    )
  } 

  ver <- verNow

  tryCatch({
    getCell2Fire(outdir) 
    save(ver, file=verfile) 
  }, warning=function(e){
     message("Warning installing cell2fire: ", e$message)
  }, error=function(e){
     message("Error installing cell2fire: ", e$message)
  })

}
