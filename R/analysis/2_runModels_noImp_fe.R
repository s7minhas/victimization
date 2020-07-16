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
loadPkg(c('MASS'))
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
