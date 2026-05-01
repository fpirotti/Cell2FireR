<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

[![R-CMD-check](https://github.com/fpirotti/Cell2FireR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fpirotti/Cell2FireR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
 
# Cell2FireR: R bindings and platform for Fire Spread Simulations using Cell2Fire

## Installation

Grab the latest version from GitHub then run the interactive app, or keep 
reading.

```         
remotes::install_github("fpirotti/Cell2FireR")
library(Cell2FireR)
Cell2FireR::runWildfireApp()
```
 
## Description

Installing this package provides two main elements:

 - R bindings to the [Cell2Fire](https://github.com/fire2a/C2F-W) software,
a Cell Based Forest Fire Growth Model. 
 - an online graphical user interface for learning how to use the fire behaviour
software.

The R bindings allow users to modify input arguments and run the
simulation inserting arguments and pipelining call to the Cell2Fire executable and
interpret the outputs converting them to maps and plots.


## Usage

In order to run the simulator and process the results, the following
command can be used:

-   via Rscript call

```         
Rscript -e "Cell2FireR::run_cell2fire()" --input-instance-folder ../data/Sub40x40/ --output-folder ../Sub40x40 --ignitions 1 --sim-years 1 --nsims 10 --grids --finalGrid --weather rows --nweathers 1 --Fire-Period-Length 1.0 --output-messages --ROS-CV 0.8 --seed 123 --stats --allPlots --IgnitionRad 1
```

-   directly

```         
library(Cell2FireR)

# Example wrapper or user call
template_location <- system.file("templates", package = "Cell2FireR")

# Choose binary based on OS automatically... or assign path of Cell2Fire to 
# bin_location variable
# 
# if (.Platform$OS.type == "windows") {
#   bin_location <- system.file("bin", "C2F", "Cell2Fire.exe", package = "Cell2FireR")
# } else {
#   bin_location <- system.file("bin", "C2F", "Cell2Fire", package = "Cell2FireR")
# }

bin_location <- "path to your cell2fire executable downloaded from https://github.com/fire2a/C2F-W/releases"
# Run the simulator
process_obj <- run_cell2fire(
  fuel = "path/to/fuel.tif",
  fuel_model = "0. Scott & Burgan",
  input_folder = "path/to/inputs",
  out_folder = "path/to/outputs",
  elevation = "path/to/elevation.tif",
  c2f_bin_path = bin_location,
  template_dir = template_location
)

# You can now monitor `process_obj` using processx methods like:
process_obj$process$wait()
process_obj$process$read_output_lines()


```

For the full list of arguments and their explanation use:

```         
run_cell2fire()
```

## Web app

An embedded web app is available in the folder `inst/app`. It allows
interactive insertion of ignition points, weather data etc.. and other
input arguments. Users can install in their own PC or in HPC servers
with multiple processors thus taking advantage from parallel processing
capabilities of [Cell2Fire](https://github.com/fire2a/C2F-W).


```         
shiny::runApp('inst/app')
```

Uploading user data can be done with a files inside a Zipped archive.
The name of the ZIP archive will be the one used for storing and viewing
the specific dataset. Fuel and canopy files should be stored directly in
the archive, without subfolders.

See a typical dataset to upload using the [FIRE-RES Pan-European Fuel
Map Server](https://www.cirgeo.unipd.it/fire-res/app/) platform for 100
m resolution maps or try one from the [direct link
HERE](https://www.cirgeo.unipd.it/FIRE-RES/ITC34/ITC34.zip)

<img src="man/images/clipboard-1986611967.png"   style="max-width:600px; " />


## Acknowledgements

This work was supported by the [Wildfire CE project "Fighting wildfires
better together across borders" Interreg Central
Europe](https://www.interreg-central.eu/projects/wildfire-ce/).
[![](man/images/wildfireLogo.png)](https://www.interreg-central.eu/projects/wildfire-ce/)

==> Many parts were taken from the excellent development and implementation of the QGIS plugin from the
[Fire Management & Advanced Analytics group](https://github.com/fire2a).

