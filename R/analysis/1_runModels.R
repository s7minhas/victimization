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
	# 'ethTens', 'anyPeaceKeeper'
	# 'rebSupportGov', 'govSupportGov'
	)
modData = na.omit(data[,c('cname','ccode','year',dv,vars)])

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
modData = modData[which(!modData$cname %in% c(toDrop)),]

# run mod
mod = glm.nb(
	civVicCount ~  # dv
		graph_dens*nActors + graph_dens + nActors + # net measures
		polity2 +   # structural controls
		# rebsStronger + # capabilities gov/rebels
		ethTens +
		anyPeaceKeeper 
		# + rebSupportGov + govSupportGov # external shit
	, data=data
	)
summary(mod)

library(glmmADMB)
modData$cname = factor(modData$cname)
mod = glmmadmb(
	civVicCount ~  # dv
		graph_dens # net measures
		# polity2   # structural controls
		# rebsStronger + # capabilities gov/rebels
		# ethTens +
		# anyPeaceKeeper 
		# + rebSupportGov + govSupportGov # external shit
		+ (1|cname)
	, data=modData, 
	family='nbinom1'	
	)

summary(mod)
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