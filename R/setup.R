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


# #### pkg version notes
# pkgs = c(
# 	'readr', 'abind', 'reshape2',
# 	'dplyr', 'tidyr', 'magrittr',
# 	'network', 'igraph',
# 	'ggplot2', 'RColorBrewer', 'latex2exp',
# 	'countrycode', 'extrafont', 'Cairo',
# 	'igraph', 'ggraph', 'tidygraph',
# 	'stringr', 'reshape2', 'sna',
# 	'network', 'doParallel', 'foreach',
# 	'MASS', 'glmmTMB', 'glmmADMB',
# 	'patchwork', 'sbgcop', 'xtable'
# 	)
#
# pkgs = sort(unique(pkgs))
# pkgV = installed.packages()[pkgs,c('Version')]
# pkgInfo = apply( cbind(pkgs, pkgV), 1, function(x){ paste(x[1], x[2], collapse=': ') } )
# pkgMat = matrix(c(pkgInfo, '', ''), ncol=4, byrow=TRUE)
# knitr::kable(pkgMat, format='markdown')
# |                |                   |                 |                  |
# |:---------------|:------------------|:----------------|:-----------------|
# |abind 1.4-5     |Cairo 1.5-12.2     |countrycode 0.16 |doParallel 1.0.16 |
# |dplyr 1.0.6     |extrafont 0.17     |foreach 1.5.1    |ggplot2 3.3.5     |
# |ggraph 2.0.5    |glmmADMB 0.8.3.3   |glmmTMB 1.0.2.1  |igraph 1.2.6      |
# |latex2exp 0.5.0 |magrittr 2.0.1     |MASS 7.3-54      |network 1.17.1    |
# |patchwork 1.1.1 |RColorBrewer 1.1-3 |readr 1.4.0      |reshape2 1.4.4    |
# |sbgcop 0.980    |sna 2.6            |stringr 1.4.0    |tidygraph 1.2.0   |
# |tidyr 1.1.3     |xtable 1.8-4       |                 |                  |
