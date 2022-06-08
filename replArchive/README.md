## Replication instructions for the paper

The base directory of the replication archive contains all the figures and tables shown in the paper. There are three  subdirectories that contain the code necessary to produce these outputs (note that these files are also available on Github at [https://github/s7minhas/victimization](https://github.com/s7minhas/victimization)):

- **1_simulation**: contains the data files and scripts necessary to reproduce the simulation results in the paper and the appendix
- **2_applications**: contains the data files and scripts necessary to reproduce the applications results in the paper and the appendix
  - Within **2_applications/** each of the scripts correspond to a particular analysis
  - **2_applications/application_data/[gibler, reiter_stam, weeks]**: contains the relevant .rda files that are inputted and outputted during the analysis of these various papers

Replicating the figures and tables in the **main** text will take only a few minutes on a standard laptop if the provided `.rda` files are used.

#### Setup information

All of the analyses reported in the manuscript and the appendix are run with the following specification (further information on packages used in the analysis is included at the end of the README):

##### R information

```
> sessionInfo()
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22000)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252
[2] LC_CTYPE=English_United States.1252
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C
[5] LC_TIME=English_United States.1252

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

loaded via a namespace (and not attached):
[1] compiler_4.0.5

> benchmarkme::get_cpu()
$vendor_id
[1] "AuthenticAMD"

$model_name
[1] "AMD Ryzen 9 5950X 16-Core Processor"

$no_of_cores
[1] 32

> benchmarkme::get_ram()
137 GB
```

##### Python information

```
> import sys
> print(sys.version)
3.7.11 (default, Jul 27 2021, 09:42:29) [MSC v.1916 64 bit (AMD64)]
> import csv
> print(csv.__version__)
1.0
> import numpy
> print(numpy.__version__)
1.21.2
> import networkx
> print(networkx.__version__)
2.6.3
```

#### Reproducing figures in the manuscript

The `here` package is used to manage paths in this replication archive for R scripts. The .here file is stored in the top-most folder of the replication archive. Scripts are listed in order of the figures and tables in the manuscript. For python related scripts, the path will need to be set manually in one file: `compModelSim/abmRunning.py`.

- **figure_1.R**: Creates the hypothetical network used to show scenarios of low and high network competition. No inputs necessary. Output is stored in `graphics/figure1.png`.
- Figures 2 and 3 are constructed manually and are not based on any code.
- Table 1 is constructed manually. The parameter listed in that table accord with the simulation parameters used in `compModelSim/abmRunning.py`.
- **figure_4.R**: Summarizes the results of a regression analysis on the simulated data. The `figure_4.R` script just create a coefficient plot using two inputs: `results/abm_feCoefs.rda` and `results/abm_reCoefs.rda`. The output of this script is stored in `graphics/figure4.png`. Steps to reproduce these results from scratch involve running the computational model, processing the data, and conducting the regression analysis; each of the scripts necessary to perform these steps are in the `compModelSim` directory:
  - `1_abmRunning.py`: Runs the computational model using the parameters specified in the paper, the code for the model itself is located in `0_VicForViz.py` (This file takes several hours to run).
  - `2_getVicCount.R`: Calculates victimization counts from the game.
  - `3_getNetStats.R`: Calculates network statistics from the game (This file utilizes parallelization, adjust number of cores in the script).
  - `4_abmDataPrep.R`: Organizing the data for analysis.
  - `5_fe_abmAnalysis.R`: Runs the fixed effects regression analysis (This file takes several hours to run).
  - `5_re_abmAnalysis.R`: Runs the random effects regression analysis.
- **figure_5.R**: Creates a descriptive visualization of how the number of actors in armed conflicts have changed over time as well as our key independent variable, network competition. The inputs for this script are: `data/rawModelData.rda` and `data/actorCntsID.rda`. The output of this script is stored in `graphics/figure5.png`.
- Table 2 is constructed manually.
- **figure_6_7.R**: Summarizes the results of a regression analysis on data from ACLED. The script requires three inputs: `results/baseMods.rda`, `results/cnt1Mods.rda`, and `results/cnt2Mods.rda`. The output of this script is stored in `graphics/figure6.png` and `graphics/figure7.png`. Steps to reproduce these results from scratch involve imputing data, running base model, running model with first set of controls, and running model with second set of controls; each of the scripts necessary to perform these steps are in the `acledAnalysis` directory:
  - `1_genImpData.R`: Performs multiple imputation analysis.
  - `2a_runModels_base.R`: Runs models without any controls not being measured from ACLED.
  - `2b_runModels_cnt1.R`: Runs models with first set of controls specified in the manuscript.
  - `2c_runMOdels_cnt2.R`: Runs models with second set of controls specified in the manuscript.
- **figure8.R**: Performs a simulation analysis to estimate the substantive effect of network competition across the model results presented in the manuscript. The script requires three inputs: `results/baseMods.rda`, `results/cnt1Mods.rda`, and `results/cnt2Mods.rda`. The output of this script is stored in `graphics/figure8.png`. The inputs for this script should have already been generated when following the steps laid out for `figure_6_7.R`.

#### Reproducing figures in the appendix

All of the scripts necessary to reproduce the figures in the appendix are located in the `appendix/` directory.

- **01_tableA_1.R**: Generates Table A1 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/table_A1.tex`.
- **02_tableA_2_3_4.R**: Generates Tables A2-4 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/table_A2.tex`, `graphics/appendix/table_A3.tex`, and `graphics/appendix/table_A4.tex`.
- **03_figureA_1.R**: Generates Figure A1 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A1.png`.
- **04_figureA_2_3_4.R**: Generates Figures A2-4 in the Appendix. Inputs: `results/baseMods.rda`, `results/cnt1Mods.rda`, and `results/cnt2Mods.rda`. Outputs: `graphics/appendix/figure_A2.png`, `graphics/appendix/figure_A3.png`, and `graphics/appendix/figure_A4.png`.
- **05_figureA_5_6.R**: Generates Figures A5-6 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A5.png` and `graphics/appendix/figure_A6.png`.
- **06_figureA_7_8.R**: Generates Figures A7-8 in the Appendix. Inputs: `data/rawModelData.rda` and `data/geoSpread_acled.rda`. Outputs: `graphics/appendix/figure_A7.png` and `graphics/appendix/figure_A8.png`.
- **07_figureA_9.R**: Generates Figure A9 in the Appendix. Inputs: `results/abm_feCoefs_allyProp.rda` and `results/abm_reCoefs_allyProp.rda`. Outputs: `graphics/appendix/figure_A9.png`.
- **08_figureA_10_11.R**: Generates Figures A10-11 in the Appendix. Inputs: `data/rawModelData.rda` and `data/allyProp_acled.rda`. Outputs: `graphics/appendix/figure_A10.png` and `graphics/appendix/figure_A11.png`.
- **09_figureA_12.R**: Generates Figure A12 in the Appendix. Inputs: `data/netStats.rda`. Outputs: `graphics/appendix/figure_A12.png`.
- **10_figureA_13.R**: Generates Figure A13 in the Appendix. Inputs: `data/modelDataCnt2.rda`. Outputs: `graphics/appendix/figure_A13.png`.
- **11_figureA_14.R**: Generates Figure A14 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A14.png`.
- **12_figureA_15.R**: Generates Figure A15 in the Appendix. Inputs: `data/data.rda`. Outputs: `graphics/appendix/figure_A15.png`.
- **13_figureA_16.R**: Generates Figure A16 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A16.png`.
- **14_figureA_17_18.R**: Generates Figure A17-18 in the Appendix. Inputs: `data/GEDEvent_v21_1.RData`, `data/rawModelData.rda`, and `data/modelDataCnt2.rda`. Outputs: `graphics/appendix/figure_A17.png` and `graphics/appendix/figure_A18.png`.
- **15_figureA_19_20.R**: Generates Figure A19-20 in the Appendix. Inputs: `data/acled_1997-01-01-2020-07-02.csv` and `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A19.png` and `graphics/appendix/figure_A20.png`.


#### R package build notes

Below we provide the version of each of the libraries that our project relies on (each library was built using R 3.5.0). Additionally, please note that we use a tailored version of [Peter Hoff's AMEN package](http://pdhoff.github.io/amen/). Installing the version of AMEN that is used in this paper can be done  with the `devtools` package by running the following command in an R session: `devtools::install_github('s7minhas/amen', ref='pa2018_version')`.

|                   |                    |                |                    |                |
|:------------------|:-------------------|:---------------|:-------------------|:---------------|
|amen: 1.4          |Cairo: 1.5-12       |caTools: 1.18.0 |countrycode: 0.16   |devtools: 2.3.0 |
|doParallel: 1.0.15 |dplyr: 0.8.5        |extrafont: 0.17 |foreach: 1.5.0      |ggplot2: 3.3.0  |
|ggrepel: 0.8.2     |grid: 4.0.3         |here: 1.0.0     |latex2exp: 0.4.0    |magrittr: 2.0.1 |
|mvtnorm: 1.1-0     |png: 0.1-7          |PRROC: 1.3.1    |RColorBrewer: 1.1-2 |reshape2: 1.4.4 |
|ROCR: 1.0-11       |separationplot: 1.3 |stringr: 1.4.0  |tidyr: 1.0.3        |xtable: 1.8-4   |


If you find any errors or have any further questions, please address them to me via email at minhassh@msu.edu.
