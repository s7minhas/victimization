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
- **figure_4.R**: Summarizes the results of a regression analysis on the simulated data. The `figure_4.R` script just create a coefficient plot using two inputs: `results/abm_feCoefs.rda` and `results/abm_reCoefs.rda`. The output of this script is stored in `graphics/figure4.png`. Steps to reproduce these results from scratch involve running the computational model, processing the data, and conducting the regression analysis, each of the scripts necessary to perform these steps are in the `compModelSim` directory:
  - `1_abmRunning.py`: Runs the computational model using the parameters specified in the paper, the code for the model itself is located in `0_VicForViz.py`.
  - `2_getVicCount.R`: Calculates victimization counts from the game.
  - `3_getNetStats.R`: Calculates network statistics from the game.
  - `4_abmDataPrep.R`: Organizing the data for analysis.
  - `5_fe_abmAnalysis.R`: Runs the fixed effects regression analysis.
  - `5_re_abmAnalysis.R`: Runs the random effects regression analysis.
- **figure_5.R**: Creates a descriptive visualization of how the number of actors in armed conflicts have changed over time as well as our key independent variable, network competition. The inputs for this script are: `data/rawModelData.rda` and `data/actorCntsID.rda`. The output of this script is stored in `graphics/figure5.png`.
- Table 2 is constructed manually.
- **figure_6_7.R**: Summarizes the results of a regression analysis on data from ACLED. The script requires three inputs: `results/baseMods.rda`, `results/cnt1Mods.rda`, and `results/cnt2Mods.rda`. The output of this script is stored in `graphics/figure6.png` and `graphics/figure7.png`.

##### Simulation: Manuscript

Scripts for the simulation portion of the manuscript should be run in the following order (each script assumes that the working directory matches the directory in which the script is stored):

- **1_simulation/figure3.R**: Creates the visualization for the regression parameter estimates from the simulation and stores the resulting figure in `replArchive/figure3.pdf`. This script depends on the presence of `replArchive/ameSim50.rda` and `replArchive/ameSim100.rda`. Both of these .rdas are created by `1_simulation/1_simManuscript.R`, which takes approximately 6 hours to complete. If the .rdas are not present, then `1_simulation/1_simManuscript.R` will be run via source.
- **1_simulation/figure4.R**: Creates the visualization showcasing the calibration of the various models from the simulation and stores the resulting figure in `replArchive/figure4.pdf`. This script also depends on the simulation results from `1_simulation/1_simManuscripts.R`.
- **1_simulation/figure5.R**: Creates the visualization showing the correlation between the omitted variable and the latent multiplicative effect term and stores the resulting figure in `replArchive/figure5.pdf`. This script also depends on the simulation results from `1_simulation/1_simManuscripts.R`.

##### Simulation: Appendix

Scripts for the simulation portion of the appendix can be found in `replArchive/1_simulation/appendixC` and `replArchive/1_simulation/appendixD`. These should be run in the following order:

- **1_simulation/appendixC/appendix_figureC1.R**: Creates the visualization for the regression parameter estimates from the simulation and stores the resulting figure in `replArchive/appendix_figureC1.pdf`. This script depends on the presence of `replArchive/ameSim50_asaProbit.rda` and `replArchive/ameSim50_asaProbit.rda`. Both of these .rdas are created by `1_simulation/appendixC/1_ameSim_asaProbit.R`, which takes 36 minutes to complete. If the .rdas are not present, then `1_simulation/appendixC/1_ameSim_asaProbit.R` will be run via source.
- **1_simulaton/appendixC/appendix_figureC2.R**: Creates the visualization showcasing the calibration of the various models from the simulation and stores the resulting figure in `replArchive/appendix_figureC2.pdf`. This script also depends on the simulation results from `1_simulation/appendixC/1_ameSim_asaProbit.R`.
- **1_simulation/appendixD/appendix_figureD1.R**: Creates the visualization for the regression parameter estimates from the simulation and stores the resulting figures in `replArchive/appendix_figureD1a.pdf` and `replArchive/appendix_figureD1b.pdf`. This script depends on the presence of `replArchive/ameSim50_corrProbitMed.rda`, `replArchive/ameSim100_corrProbitMed.rda`, `replArchive/ameSim50_corrProbitHi.rda`, and `replArchive/ameSim100_corrProbitHi.rda`. Each of these .rdas are created by `1_simulation/appendixD/1_ameSim_rev2_probitCorrel.R`, which takes 13 hours to complete. If the .rdas are not present, then `1_simulation/appendixD/1_ameSim_rev2_probitCorrel.R` will be run via source.

##### Applications: Manuscript

- **2_application/figure7.R**: This script creates the sender and receiver effects visualization from the Reiter & Stam replication and stores the resulting figure in `replArchive/figure7a.pdf` and `replArchive/figure7b.pdf` (these pieces are combined together via .tex to create one figure in the manuscript). This script depends on the presence of `replArchive/2_applications/application_data/reiter_stam/ameFitReiterStam.rda`, which is created by `2_application/reiter_stam_ameRun.R` and takes approximately a day to run.
- **2_application/figure8.R**: This script creates the multiplicative effects visualization from the Weeks replication and stores the resulting figure in `replArchive/figure8.pdf`. This script depends on the presence of `replArchive/2_applications/application_data/weeks/ameFitWeeks.rda`, which is created by `2_application/weeks_ameRun.R` and takes approximately a day to run.
- **2_application/figure9.R**:
  - This script creates the substantive effects visualization from the Gibler replication and stores the resulting figure in `replArchive/figure9.pdf`. This script depends on the presence of `replArchive/2_applications/application_data/gibler/ameFitGibler_s[119, 1571, 1922, 211, 3316, 3466, 3508, 4087, 6516, 806].rda`, which are created by `2_application/gibler_ameRun.R` and takes approximately four days to run. This script also depends on the presence of `replArchive/2_applications/application_data/gibler/glmFitGibler.rda`, which is created by `2_applications/gibler_glmRun.R` and runs relatively quickly.
- **2_applications/figure6.R**:
  - This script creates the performance summary visualization in the manuscript and stores the resulting set of figures in the following files (these pieces are combined together via .tex to create one figure in the manuscript):
    - `replArchive/figure6_reiter_stam_roc_outSample.pdf`
    - `replArchive/figure6_reiter_stam_pr_outSample.pdf`
    - `replArchive/figure6_weeks_roc_outSample.pdf`
    - `replArchive/figure6_weeks_pr_outSample.pdf`
    - `replArchive/figure6_gibler_roc_outSample.pdf`
    - `replArchive/figure6_gibler_pr_outSample.pdf`
  - `2_applications/figure6.R` depends on the presence of a number of .rdas. These are listed below and for each the source script and computation time is also provided.
		- 'replArchive/ameOutSampReiterStam.rda'
			- `replArchive/2_applications/reiter_stam_ameCrossVal.R` (14 hours)
		- 'replArchive/glmOutSampReiterStam.rda'
			- `replArchive/2_applications/reiter_stam_glmCrossVal.R` (runs relatively quickly)
		- 'replArchive/ameOutSampWeeks.rda'
			- `replArchive/2_applications/weeks_ameCrossVal.R` (8 hours)
		- 'replArchive/glmOutSampWeeks.rda'
			- `replArchive/2_applications/weeks_glmCrossVal.R` (runs relatively quickly)
		- 'replArchive/ameOutSampGibler.rda'
			- `replArchive/2_applications/gibler_ameCrossVal.R` (8 hours)
		- 'replArchive/glmOutSampGibler.rda'
			- `replArchive/2_applications/gibler_glmCrossVal.R` (runs relatively quickly)
	- If any of the .rdas are missing, then the required scripts will be called via the source function. Running the cross-validation analyses is too computationally intensive for most desktops and laptops as the amount of ram required for the Gibler cross-validation alone exceeds 200GB. To deal with this computational burden, the analyses are run on Amazon's EC2 service using a m5.24xlarge instance and Ubuntu Server 20.04 LTS AMI. Details of how the workspace is set up are shown below:

		```
		sudo adduser minhas ; sudo apt-get update -y ; sudo apt-get upgrade -y ; sudo apt update -qq ; sudo apt install --no-install-recommends software-properties-common dirmngr ; sudo apt install --no-install-recommends software-properties-common dirmngr ; sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 ; sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" ; sudo apt install --no-install-recommends r-base ; sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+ ; sudo apt-get install gdebi-core ; wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-1.4.1717-amd64.deb ; sudo gdebi rstudio-server-1.4.1717-amd64.deb ; sudo apt-get install libapparmor1 ; sudo apt-get install libblas-dev liblapack-dev ; sudo apt-get install gfortran; sudo apt-get install pkg-config ; sudo apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libnlopt-dev ; sudo apt-get install htop ; sudo apt-get install tree ; sudo apt install curl ; sudo apt-get install zip unzip
		```

		- Once the instance is setup per the instructions above, the workspace should have the attributes shown below and the crossVal scripts can be run to generated the .rdas necessary for Figure 6. When porting over files into EC2 it will not be necessary to upload the entire replArchive directory only the files that are specifically called by the _crossVal.R scripts.

		```
		> sessionInfo()
		R version 4.1.0 (2021-05-18)
		Platform: x86_64-pc-linux-gnu (64-bit)
		Running under: Ubuntu 20.04.2 LTS

		Matrix products: default
		BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
		LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

		locale:
		 [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8        LC_COLLATE=C.UTF-8
		 [5] LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8    LC_PAPER=C.UTF-8       LC_NAME=C
		 [9] LC_ADDRESS=C           LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C

		attached base packages:
		[1] stats     graphics  grDevices utils     datasets  methods   base

		loaded via a namespace (and not attached):
		[1] compiler_4.1.0 tools_4.1.0

		> benchmarkme::get_cpu()
		$vendor_id
		[1] "GenuineIntel"

		$model_name
		[1] "Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz"

		$no_of_cores
		[1] 96

		> benchmarkme::get_ram()
		401 GB
		```

##### Applications: Appendix

- **2_applications/appendix_tableB1.R**: Creates the table showcasing differences in parameter estimates from GLM vs AME for the Reiter & Stam replication. This script depends on the presence of `replArchive/2_applications/application_data/reiter_stam/ameFitReiterStam.rda`, which is created by `2_application/reiter_stam_ameRun.R`. Script also depends on the presence of `replArchive/2_applications/application_data/reiter_stam/glmFitReiterStam.rda`, which is created by `2_applications/reiter_stam_glmRun.R`.
- **2_applications/appendix_tableB2.R**: Creates the table showcasing differences in parameter estimates from GLM vs AME for the Weeks replication. This script depends on the presence of `replArchive/2_applications/application_data/weeks/ameFitWeeks.rda`, which is created by `2_application/weeks_ameRun.R`. Script also depends on the presence of `replArchive/2_applications/application_data/weeks/glmFitWeeks.rda`, which is created by `2_applications/weeks_glmRun.R`.
- **2_applications/appendix_tableB3.R**: Creates the table showcasing differences in parameter estimates from GLM vs AME for the Gibler replication. This script depends on the presence of `replArchive/2_applications/application_data/gibler/ameFitGibler_s[119, 1571, 1922, 211, 3316, 3466, 3508, 4087, 6516, 806].rda`, which are created by `2_application/gibler_ameRun.R`. Script also depends on the presence of `replArchive/2_applications/application_data/gibler/glmFitGibler.rda`, which is created by `2_applications/gibler_glmRun.R`.

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
