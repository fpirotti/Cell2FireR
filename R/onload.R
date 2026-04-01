.onLoad <- function(libname, pkgname) {
  
   

  bindir <- system.file("bin", package = pkgname)
  outdir <- file.path(bindir, "C2F")
  exewinfile <- file.path(outdir, "Cell2Fire.exe")
  if(!dir.exists(outdir)){
    dir.create(outdir )
  } else{
    if(file.exists(exewinfile)){
      return()
    }
  }

  outdir <- file.path(system.file("bin", package = pkgname), "C2F")
  getCell2Fire(outdir)  

}
