getCell2Fire <- function() {
  dest <- file.path(tempdir(), "Cell2FireR")

  if (!dir.exists(dest)) {
    download.file(
      "https://github.com/fire2a/C2F-W/releases/download/v1.0.1/Cell2FireW_v1.0.1.zip",
      destfile = paste0(dest, ".zip")
    )
    unzip(paste0(dest, ".zip"), exdir = dest)
  }

  src <- list.dirs(dest, recursive = FALSE, full.names = TRUE)
  fp <- file.path(dest, "C2F")
  exe.linux <- file.path(fp,"Cell2Fire")
  exe.win <- file.path(fp, "Cell2Fire.exe")

  if(!file.exists(exe.linux) ){
    warning("Linux compiled Cell2Fire does not exist")
  }

  if( !file.exists(exe.win)){
    warning("Windows compiled Cell2Fire does not exist")
  }
  file.access()
  system(sprintf("cd %s && make", file.path(dest, "C2F", "Cell2Fire") ))
  system(sprintf("cd %s && ls", file.path(dest, "C2F", "Cell2Fire") ))
}
