# Cell2FireR: R bindings for Cell2Fire -

## Description

R bindings to the [Cell2Fire](https://github.com/cell2fire/Cell2Fire) software, a Cell Based Forest Fire Growth Model. It also provides an online graphical interface for learning how to use a wildfire behaviour software.

The R bindings try to improve efficiency by adding support for GeoTIFF files holding fuel model types and topographical (height above sea level, slope and aspect) information. It allows users to modify input arguments and run the simulation also providing a vector with a range of argument values, thus pipelining the testing of single-argument on the final results.

## Usage

In order to run the simulator and process the results, the following command can be used:

-   via Rscript call

```         
Rscript -e "Cell2FireR::cell2fire_run()" --input-instance-folder ../data/Sub40x40/ --output-folder ../Sub40x40 --ignitions 1 --sim-years 1 --nsims 10 --grids --finalGrid --weather rows --nweathers 1 --Fire-Period-Length 1.0 --output-messages --ROS-CV 0.8 --seed 123 --stats --allPlots --IgnitionRad 1
```

-   via direct

```         
library(Cell2FireR)

cell2fire_run(c("--input-instance-folder", "../data/Sub40x40/",
                "--output-folder", "../Sub40x40", 
                "--ignitions", "1"
                "--sim-years", "1"))
```

For the full list of arguments and their explanation use:

```         
cell2fire_run()
```


## Web app

An embedded web app is available. It allows interactive insertion of 
ignition points and other input arguments.

```         


```


## Acknowledgements

This work was supported by the [Wildfire CE project "Fighting wildfires better together across borders" Interreg Central Europe](https://www.interreg-central.eu/projects/wildfire-ce/). [![](https://www.interreg-central.eu/wp-content/uploads/2024/08/WildfireCE_Logo_Standard_medium.png){width="400"}](https://www.interreg-central.eu/projects/wildfire-ce/)
