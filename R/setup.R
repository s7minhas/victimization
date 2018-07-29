########
rm(list=ls())
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	pathGit = '~/Research/victimization/'
	pathDrop = '~/Dropbox/Research/victimization/'
	pathData = paste0(pathDrop, 'data/')
}

if(Sys.info()['user'] %in% c('cassydorff')){
	pathGit = '~ProjectsGit/victimization/'
	pathDrop = '~/Dropbox/Research/nothingbutnet/victimization/'
	pathData = paste0(pathDrop, 'data/')
}
if(Sys.info()['user'] %in% c('maxgallop')){
	pathGit = '~/Documents/victimization/'
	pathDrop = '~/Dropbox/intraConfNetDyn/'
	pathData = paste0(pathDrop, 'data/')
}

########

########
# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
pasteMult = function(x,y,sepZ){
	apply(expand.grid(x,y), 1, paste, collapse=sepZ) }
cname = function(x) {countrycode(x,'country.name','country.name')}
simpleMerge = function(toData, fromData, vars, toID, fromID, lagVars=TRUE){
	if(lagVars){
		fromData$yrForMerge = unlist(lapply(strsplit(fromData[,fromID],'_'),function(x){x[2]}))
		fromData$unitForMerge = unlist(lapply(strsplit(fromData[,fromID],'_'),function(x){x[1]}))
		fromData$yrForMerge = num(fromData$yrForMerge) + 1
		fromData[,fromID] = with(fromData, paste0(unitForMerge, '_', yrForMerge))
	}
	for(v in vars){
		toData$tmp = fromData[match(toData[,toID], fromData[,fromID]), v]
		names(toData)[ncol(toData)] = v }
	return(toData) }
########

########
# load panel data
load(paste0(pathData, 'panel.rda'))
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
	'ggplot2', 'RColorBrewer', 'latex2exp',
	'countrycode'
	)
loadPkg(pkgs)

# Set a theme for gg
theme_set(theme_bw())
########