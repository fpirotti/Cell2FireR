.onLoad <- function(libname, pkgname) {
  if (interactive())  packageStartupMessage("Checking for Cell2Fire executable...")
  bindir <- system.file("bin", package = pkgname)
  outdir <- file.path(bindir, "C2F")
  exewinfile <- file.path(outdir, "Cell2Fire.exe")
  if(!dir.exists(outdir)){
    dir.create(outdir )
  } else{
    if(file.exists(exewinfile)){
      if (interactive())  packageStartupMessage("Cell2Fire already available.")
      return()
    }
  }

  if (interactive())  packageStartupMessage("Cell2Fire NOT available, installing...")

  outdir <- file.path(system.file("bin", package = pkgname), "C2F")
  packageStartupMessage(outdir)
  if(!getCell2Fire(outdir)){
    if (interactive()) packageStartupMessage("Something went wrong in installation of the C2F package,
                          contact the developers")
  } else {
    if (interactive())  packageStartupMessage("Cell2Fire successfully installed")

  }

}
