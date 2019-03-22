########################################################
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

# # check for kathman peace keeper
# data = data[data$year<=2013,]
# iData = lapply(iData, function(data){
# 	data[data$year<=2013,]
# })
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

# save
save(
	modBase_noImp,
	modsBase, summBase,
	modsCntrls, summCntrls,
	file=paste0(pathResults, 'nbMods_acled.rda')
	)
########################################################