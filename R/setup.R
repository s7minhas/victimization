########
rm(list=ls())
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	pathGit = paste0(
		'C:/Users/',Sys.info()['user'],'/Research/victimization/')
	pathDrop = paste0(
		'C:/Users/',Sys.info()['user'],'/Dropbox/Research/victimization/')
	pathData = paste0(pathDrop, 'data/')
	pathResults = paste0(pathDrop, 'results/')
	pathGraphics = paste0(pathDrop, 'graphics/')
	abmPath = paste0(pathDrop, "abm/")
	funcPath = paste0(pathGit, 'R/funcs/')
}

if(Sys.info()['user'] %in% c('dorffc')){
	pathGit = '~ProjectsGit/victimization/'
	pathDrop = '~/Dropbox/Research/nothingbutnet/victimization/'
	pathData = paste0(pathDrop, 'data/')
	pathGraphics = paste0(pathDrop, 'graphics/')
	abmPath = paste0(pathDrop, "abm/")
	funcPath = paste0(pathGit, 'R/funcs/')
}

if(Sys.info()['user'] %in% c('maxgallop')){
	pathGit = '~/Documents/victimization/'
	pathDrop = '~/Dropbox/intraConfNetDyn/'
	pathData = paste0(pathDrop, 'data/')
	abmPath = paste0(pathDrop, "abm/")
	funcPath = paste0(pathGit, 'R/funcs/')
}
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
