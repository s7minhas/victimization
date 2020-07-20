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
# load imputed data for cnt1
load(paste0(pathData, 'modelDataCnt1.rda'))

names(iData[[1]]) %>% cbind()

dataCnt1 = data
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
# load imputed data for cnt2
load(paste0(pathData, 'modelDataCnt2.rda'))
dataCnt2 = data
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
