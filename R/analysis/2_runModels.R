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
loadPkg(c('MASS', 'glmmTMB'))
########################################################

########################################################
# load raw model data (no imputation)
load(paste0(pathData, 'rawModelData.rda'))
########################################################

########################################################
# run base model
modBase_noImp = glm.nb(
	civVicCount ~  # dv
		graph_dens + nEvents + nActors + factor(cname) -1
	, data=dataBase
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'
summBase_noImp[1:5,]

modBase_noImp = glm.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors + factor(cname) -1
	, data=dataBase
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'
summBase_noImp[1:5,]
########################################################

########################################################
# run cnt1 model with no imp
mod = glm.nb(
	civVicCount ~  # dv
		graph_dens + nEvents + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ factor(cname) -1
	, data=dataCnt1
	)

summary(mod)$'coefficients'[1:5,]

mod = glm.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ factor(cname) -1
	, data=dataCnt1
	)

summary(mod)$'coefficients'[1:5,]
########################################################

########################################################
# run cnt2 model with no imp
mod = glm.nb(
	civVicCount ~  # dv
		graph_dens + nEvents + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ anyPeaceKeeper
		+ rebsStronger + rebSupportGov + govSupportGov
		+ factor(cname) -1
	, data=dataCnt2
	)

summary(mod)$'coefficients'[1:5,]

mod = glm.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ anyPeaceKeeper
		+ rebsStronger + rebSupportGov + govSupportGov
		+ factor(cname) -1
	, data=dataCnt2
	)

summary(mod)$'coefficients'[1:5,]
########################################################

cntries = unique(data$cname)
coef=NULL

for(cntry in cntries){
		slice = data[data$cname!=cntry,]
		modBase_noImp = glm.nb(
			civVicCount ~  # dv
				graph_dens + nActors + factor(cname) -1
			, data=slice
			)
		summBase_noImp = summary(modBase_noImp)$'coefficients'
		out = summBase_noImp[1,,drop=FALSE]
		coef = rbind(coef, out)
}

head(coef)
summary(coef)

modBase_noImp = glm.nb(
	civVicCount ~  # dv
		graph_dens + nActors + factor(cname) -1
	, data=data
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'
summBase_noImp[1,,drop=FALSE]


########################################################

########################################################
# load data
load(paste0(pathData, 'iData_acled.rda'))
########################################################

dim(na.omit(data[,c('cname', 'cnameYear')]))
dim(
	na.omit(
		data[,c(
			'civVicCount', 'graph_dens', 'nConf2', 'nActors',
			'cname', 'polity2', 'popLog', 'gdpCapLog',
			'ethfrac', 'anyPeaceKeeper', 'rebsStronger',
			'rebSupportGov', 'govSupportGov'
			)]
	)
)

########################################################
# run base model
modBase_noImp = glm.nb(
	civVicCount ~  # dv
		graph_dens + nConf2 + nActors + factor(cname) -1
	, data=data
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'
summBase_noImp[1:5,]

a = glm.nb(
	civVicCount ~  # dv
		graph_avgDeg + nConf2 + nActors + factor(cname) -1
	, data=data
	)
summary(a)$'coefficients'[1:5,]

# pick slice of imputed data
set.seed(6886)
impPicks = sample(1:length(iData), 10)

# run base mods on imputed data
modsBase = lapply(iData[impPicks], function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf2 + nActors
			 + factor(cname) -1
		, data=data
		)
	return(mod) })

# run mod with controls on imputed data
modsCntrls = lapply(iData[impPicks], function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf2 + nActors
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

modsBase2 = lapply(iData[impPicks], function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_avgDeg + nConf2 + nActors
			 + factor(cname) -1
		, data=data
		)
	return(mod) })

# run mod with controls
modsCntrls2 = lapply(iData[impPicks], function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_avgDeg + nConf2 + nActors
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
summBase2 = rubinCoef(modsBase2, TRUE)
summCntrls2 = rubinCoef(modsCntrls2, TRUE)

#
summBase[1:5,]
summCntrls[1:5,]
summBase2[1:5,]
summCntrls2[1:5,]

# library(randomForest)
# netStats = iData[[4]]
# netStats$numConf = netStats$nConf2
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
