########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('Owner')){
	source('C:/Users/Owner/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg('MASS')
########################################################

########################################################
# load data
load(paste0(pathData, 'iData_acled.rda'))
########################################################

########################################################
# run base model
modBase_noImp = glm.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors + factor(cname) -1
	, data=data
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'

modsBase = lapply(iData, function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors
			 + factor(cname) -1
		, data=data
		)
	return(mod) })

# run mod with controls
modsCntrls = lapply(iData, function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors
			+ polity2 + popLog + gdpCapLog   # structural controls
			+ ethfrac
			+ anyPeaceKeeper
			+ rebsStronger # capabilities gov/rebels
			+ rebSupportGov + govSupportGov # external shit
			+ factor(cname) -1
		, data=data
		)
	return(mod) })

# summarize
summBase = rubinCoef(modsBase, TRUE)
summCntrls = rubinCoef(modsCntrls, TRUE)

summBase
summCntrls

# library(randomForest)
# netStats = iData[[4]]
# netStats$numConf = netStats$nConf
# netStats$n_actors = netStats$nActors
# netStats$vic = netStats$civVicCount

# rfMod = randomForest(x=data.matrix(
# 	netStats[,c('graph_dens', 'numConf', 'n_actors')]
# 	), y=netStats[,'vic'], type='regression')
# partialPlot(
	# rfMod, pred.data=netStats[,
		# c('graph_dens', 'numConf', 'n_actors')], x.var='graph_dens')

# save
save(
	modBase_noImp,
	modsBase, summBase,
	modsCntrls, summCntrls,
	file=paste0(pathResults, 'nbMods_acled.rda')
	)
########################################################
