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
	return(mod) })

# run mod with controls
# toKeep = names(
# 	table(iData[[1]]$cname)[
# 	table(iData[[1]]$cname)>10] ) # check to make sure results consistent
modsCntrls = lapply(iData, function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors
			+ factor(cname) -1 
			+ polity2 + popLog + gdpCapLog   # structural controls
			+ ethfrac
			+ anyPeaceKeeper 
		, data=data
		)
	return(mod) })

# summarize
summBase = rubinCoef(modsBase, TRUE)
summCntrls = rubinCoef(modsCntrls, TRUE)

# quick glimpse
# round(summCntrls[!grepl('factor',rownames(summCntrls)),],3)

# save
save(
	modBase_noImp,
	modsBase, summBase,
	modsCntrls, summCntrls,
	file=paste0(pathResults, 'nbMods_acled.rda')
	)
########################################################