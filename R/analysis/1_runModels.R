####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg('MASS')
####

####
# load data
load(paste0(pathData, 'data.rda'))
####

####
# run models
dv = 'civVicCount'
vars = c(
	'graph_dens', 'graph_recip', 'graph_trans', 'nActors', 
	'polity2'
	# 'rebsStronger',
	,'ethTens', 'anyPeaceKeeper'
	# 'rebSupportGov', 'govSupportGov'
	)
# modData = na.omit(data[,c('cname','ccode','year',dv,vars)])
modData=data

# sample restriction
toDrop = c(
	'CONGO, REPUBLIC OF',
	'GUINEA',
	'GUINEA-BISSAU',
	'MOZAMBIQUE',
	'RWANDA',
	'SIERRA LEONE',
	'ZIMBABWE', 
	'LIBERIA',
	'CAMEROON',
	'MADAGASCAR',
	'TUNISIA'
	)
# modData = modData[which(!modData$cname %in% c(toDrop)),]

# naive just impute everything
loadPkg('sbgcop')
impData = data.matrix(modData[,c(5:7,11,19,20,23,24:36,44,48,49,52,54)])
sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
sbgFrom = sbgData$Y.pmean
modData = cbind(modData[,c('cname','year','cnameYear','nActors')], sbgFrom)

# run mod
mod = glm.nb(
	civVicCount ~  # dv
		graph_dens
		+ polity2   # structural controls
		+ rebsStronger # capabilities gov/rebels
		+ ethTens
		+ anyPeaceKeeper 
		+ rebSupportGov + govSupportGov # external shit
	, data=modData
	)
summary(mod)

# cross val analysis
cntries = unique(modData$cname)
coefCross = lapply(cntries, function(c){
	slice = modData[which(modData$cname != c),]
	sMod = glm.nb(civVicCount~graph_dens+polity2+anyPeaceKeeper,data=slice)
	beta = data.frame(matrix(summary(sMod)$'coefficients'[2,],nrow=1))[1:2]
	names(beta) = c('est','stdErr')
	beta$exclCntry = c
	return(beta) }) %>% do.call('rbind', .)

# viz cross val
coefCross$qHi = coefCross$est + qnorm(.975)*coefCross$stdErr
coefCross$qLo = coefCross$est - qnorm(.975)*coefCross$stdErr
coefCross$exclCntry = factor(coefCross$exclCntry, 
	levels=c(
		coefCross$exclCntry[order(coefCross$est)]
		)
	)
ggplot(coefCross, aes(x=exclCntry, y=est)) +
	geom_point() + 
	geom_linerange(aes(ymin=qLo, ymax=qHi)) +
	geom_hline(aes(yintercept=0), color='red', linetype='dashed') +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.x=element_text(angle=45, size=5,hjust=1)
		)

# modData = na.omit(data[,c('cname','ccode','year',dv,vars)])
library(glmmADMB)
modData$cname = factor(modData$cname)
modRE = glmmadmb(
	civVicCount ~  # dv
		graph_dens
		# + nActors # net measures
		+ polity2   # structural controls
		# # + rebsStronger # capabilities gov/rebels
		# + ethTens
		+ anyPeaceKeeper 
		# # + rebSupportGov + govSupportGov # external shit
		+ (1|cname)
	, data=na.omit(modData[,c('cname','ccode','year',dv,'graph_dens','polity2','anyPeaceKeeper')]), 
	family='nbinom1'	
	)

summary(modRE)
####

####
cor(data[,c(dv,'graph_recip','graph_trans','graph_dens')], use='pairwise.complete.obs')[-1,1]

cntries = unique(data$cname)
corStats = lapply(cntries, function(x){
	slice = data[which(data$cname==x),]
	stats=cor(slice[,c(dv,'graph_recip','graph_trans','graph_dens')], use='pairwise.complete.obs')[-1,1]	
	return(round(stats,3))
})
corDF = data.frame(do.call('rbind', corStats))
corDF$cname = cntries
corDF[order(corDF$graph_dens),]
####