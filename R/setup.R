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
}

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	pathGit = '~/Research/victimization/'
	# pathDrop = '~/Dropbox/Research/victimization/'
	pathDrop = '/Volumes/Samsung_X5/Dropbox/Research/victimization/'
	pathData = paste0(pathDrop, 'data/')
	pathResults = paste0(pathDrop, 'results/')
	pathGraphics = paste0(pathDrop, 'graphics/')
	abmPath = paste0(pathDrop, "abm/")
}

if(Sys.info()['user'] %in% c('dorffc')){
	pathGit = '~ProjectsGit/victimization/'
	pathDrop = '~/Dropbox/Research/nothingbutnet/victimization/'
	pathData = paste0(pathDrop, 'data/')
	pathGraphics = paste0(pathDrop, 'graphics/')
	abmPath = paste0(pathDrop, "abm/")
}

if(Sys.info()['user'] %in% c('maxgallop')){
	pathGit = '~/Documents/victimization/'
	pathDrop = '~/Dropbox/intraConfNetDyn/'
	pathData = paste0(pathDrop, 'data/')
	abmPath = paste0(pathDrop, "abm/")
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
		fromData$yrForMerge = unlist(
			lapply(strsplit(fromData[,fromID],'_'),function(x){x[2]}))
		fromData$unitForMerge = unlist(
			lapply(strsplit(fromData[,fromID],'_'),function(x){x[1]}))
		fromData$yrForMerge = num(fromData$yrForMerge) + 1
		fromData[,fromID] = with(fromData,
			paste0(unitForMerge, '_', yrForMerge))
	}
	for(v in vars){
		toData$tmp = fromData[match(toData[,toID], fromData[,fromID]), v]
		names(toData)[ncol(toData)] = v }
	return(toData) }
options(warn=-1)
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
	'readr', 'abind',
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
# combine model results from imputed data using
# rubin's rules
rubinCoef = function(modCoef, matrixFormat=FALSE){

  modSumm = Amelia::mi.meld(
    q=matrix(modCoef[,1],ncol=length(unique(rownames(modCoef))), byrow=TRUE),
    se=matrix(modCoef[,2],ncol=length(unique(rownames(modCoef))), byrow=TRUE),
    byrow=TRUE) %>% lapply(., t) %>% do.call('cbind',.) %>% data.frame(.)

  names(modSumm) = c('beta', 'se')
  modSumm$t = modSumm$beta/modSumm$se
  modSumm$var = unique(rownames(modCoef))

  if(matrixFormat){
    names(modSumm) = c('Estimate', 'Std. Error', 't value', 'var')
    rownames(modSumm) = modSumm$var
    modSumm = data.matrix(modSumm[,-ncol(modSumm)]) }

  return(modSumm)
}
########

########
# calc CI ranges, beta df must have mean and sd cols
getCIVecs = function(beta){
	beta$lo95 = beta$mean - qnorm(.975)*beta$sd
	beta$hi95 = beta$mean + qnorm(.975)*beta$sd
	beta$lo90 = beta$mean - qnorm(.95)*beta$sd
	beta$hi90 = beta$mean + qnorm(.95)*beta$sd
	return(beta)
}

# colors for sig
coefp_colors = c(
	"Positive"=rgb(54, 144, 192, maxColorValue=255),
	"Negative"= rgb(222, 45, 38, maxColorValue=255),
	"Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
	"Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	"Insignificant" = rgb(150, 150, 150, maxColorValue=255)
	)

# add sig col to beta df gen from getCIVecs
getSigVec = function(beta){
	beta$sig = NA
	beta$sig[beta$lo90 > 0 & beta$lo95 < 0] = "Positive at 90"
	beta$sig[beta$lo95 > 0] = "Positive"
	beta$sig[beta$hi90 < 0 & beta$hi95 > 0] = "Negative at 90"
	beta$sig[beta$hi95 < 0] = "Negative"
	beta$sig[beta$lo90 < 0 & beta$hi90 > 0] = "Insignificant"
	return(beta)
}
########
