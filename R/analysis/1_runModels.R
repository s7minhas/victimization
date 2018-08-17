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
####