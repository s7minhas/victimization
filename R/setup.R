########
rm(list=ls())
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	pathGit = '~/Research/intraConfNetDyn/'
	pathDrop = '~/Dropbox/Research/intraConfNetDyn/'
	pathData = paste0(pathDrop, 'data/')
}
########

########
# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
########

########
# install/load libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

pkgs = c(
	'dplyr', 'tidyr', 'magrittr',
	'network', 'igraph', 
	'ggplot2', 'RColorBrewer', 'latex2exp'
	)
loadPkg(pkgs)

# Set a theme for gg
theme_set(theme_bw())
########