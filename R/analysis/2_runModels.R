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
########################################################

########################################################
# run base model
modBase_noImp = glm.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors + factor(cname) -1 
	, data=data
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'
round(summBase_noImp[!grepl('factor',rownames(summBase_noImp)),], 3)

modsBase = lapply(iData, function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors + factor(cname) -1 
		, data=data
		)
	summary(mod)
	return(mod) })

# run mod with controls
modsCntrls = lapply(iData, function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors
			+ factor(cname) -1 
			+ polity2   # structural controls
			+ rebsStronger # capabilities gov/rebels
			+ ethTens
			+ anyPeaceKeeper 
			+ rebSupportGov + govSupportGov # external shit
		, data=data
		)
	summary(mod)
	return(mod) })

# summarize
summBase = rubinCoef(modsBase, TRUE)
summCntrls = rubinCoef(modsCntrls, TRUE)

#
round(summBase[!grepl('factor',rownames(summBase)),], 3)
round(summCntrls[!grepl('factor',rownames(summCntrls)),], 3)
########################################################