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
modBase_noImp = glmmTMB(
	civVicCount ~  # dv
		graph_dens + nEvents + nActors + (1|cname)
	, data=dataBase, family='nbinom2'
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'$cond
summBase_noImp

modBase_noImp = glmmTMB(
	civVicCount ~  # dv
		graph_dens + nConf + nActors + (1|cname)
	, data=dataBase, family='nbinom2'
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'$cond
summBase_noImp
########################################################

########################################################
# run cnt1 model with no imp
mod = glmmTMB(
	civVicCount ~  # dv
		graph_dens + nEvents + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ (1|cname)
	, data=dataCnt1, family='nbinom2'
	)

summary(mod)$'coefficients'$cond

mod = glmmTMB(
	civVicCount ~  # dv
		graph_dens + nConf + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ (1|cname)
	, data=dataCnt1, family='nbinom2'
	)

summary(mod)$'coefficients'$cond
########################################################

########################################################
# run cnt2 model with no imp
mod = glmmTMB(
	civVicCount ~  # dv
		graph_dens + nEvents + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ anyPeaceKeeper
		+ rebsStronger + rebSupportGov + govSupportGov
		+ (1|cname)
	, data=dataCnt2, family='nbinom2'
	)

summary(mod)$'coefficients'$cond

mod = glmmTMB(
	civVicCount ~  # dv
		graph_dens + nConf + nActors
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ exclpop
		+ anyPeaceKeeper
		+ rebsStronger + rebSupportGov + govSupportGov
		+ (1|cname)
	, data=dataCnt2, family='nbinom2'
	)

summary(mod)$'coefficients'$cond
########################################################
