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
addCat = function(baseVar, labs, breaks){
	catVar = rep(labs[1], length(baseVar))
	for(ii in 1:length(breaks)){
		catVar[baseVar>=breaks[ii]] = labs[ii+1] }
	catVar = factor(catVar,
		levels = labs)
	return(catVar) }
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
	  suppressWarnings(
			suppressMessages( library(lib, character.only=TRUE) ))
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

########
# process coefs
coefProcess = function(coefList, labs=mLabs, vKey=varKey){
	out = lapply(1:length(coefList), function(ii){
		x = coefList[[ii]] ; lab = labs[ii]
		colnames(x)[1:3] = c('mean', 'sd', 'tstat')
		if( !('var' %in% names(x)) ){
			x = data.frame(x[,1:3], stringsAsFactors=FALSE)
			x$var = rownames(x) ; rownames(x) = NULL }
		x = x %>% getCIVecs(.) %>%
			getSigVec(.) %>%
			mutate(model=lab)
		return(x) })

	# combine
	out = do.call('rbind', out)

	# clean variable labels
	out$model = factor(out$model, levels=mLabs)
	out$varName = vKey$clean[match(out$var, vKey$dirty)]
	out = out[!is.na(out$varName),]
	out$varName = factor(out$varName, levels=rev(vKey$clean))
	return(out) }
########

########
# viz coefs
coefViz = function(coefData, fName, path=pathGraphics){
	ggCoef = ggplot(coefData, aes(x=varName, y=mean, color=sig)) +
		geom_hline(aes(yintercept=0), linetype=2, color = "black") +
		geom_point(size=4) +
		geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
		geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +
		scale_colour_manual(values = coefp_colors, guide=FALSE) +
		ylab('') + xlab('') + facet_wrap(~model) +
		coord_flip() + theme_light(base_family="Source Sans Pro") +
		theme(
			legend.position='top', legend.title=element_blank(),
			panel.border=element_blank(), axis.ticks=element_blank(),
			axis.text.y=element_text(hjust=0),
			strip.text.x = element_text(size = 9, color='white'),
			strip.background = element_rect(
				fill = "#525252", color='#525252'))
	ggsave(ggCoef, width=8, height=6,
		file=paste0(path, fName), device=cairo_pdf) }
########
