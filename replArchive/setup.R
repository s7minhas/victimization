########
rm(list=ls())
pth = paste0(here::here(), '/')
pathData = paste0(pth, 'data/')
pathResults = paste0(pth, 'results/')
pathGraphics = paste0(pth, 'graphics/')
funcPath = paste0(pth, 'funcs/')
########

########
# install/load libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressWarnings(
			suppressMessages( library(lib, character.only=TRUE) ))
	}
}

pkgs = c(
	'readr', 'abind', 'reshape2',
	'dplyr', 'tidyr', 'magrittr',
	'network', 'igraph',
	'ggplot2', 'RColorBrewer', 'latex2exp',
	'countrycode', 'extrafont', 'Cairo'
	)
loadPkg(pkgs)

# load fonts
suppressMessages(loadfonts())

# Set a theme for gg
theme_set(theme_bw())
smTheme = theme(
  axis.ticks=element_blank(),
  panel.border=element_blank()
)

#
if(!'simHelper' %in% installed.packages()[,1]){
  devtools::install_github('s7minhas/simHelper', ref='vic') }

# warnings
options(warn=-1)
########

########
# load panel data
load(paste0(pathData, 'panel.rda'))
########

########
# call helper functions
source(paste0(funcPath, 'miscHelpers.R'))
source(paste0(funcPath, 'netSimHelpers.R'))
source(paste0(funcPath, 'allyPropHelpers.R'))
source(paste0(funcPath, 'modelSummaryHelpers.R'))
########
