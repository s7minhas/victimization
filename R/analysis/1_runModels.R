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
load(paste0(pathData, 'data_acled.rda'))
load(paste0(pathData, 'cntriesGED_byAll.rda'))
cData = cData[,c('country','year')]
cData$cnt = 1
cData = cData %>% group_by(country, year) %>%
	summarize(nConf = sum(cnt)) %>%
	data.frame()
cData$cname = cname(cData$country)
cData$id = with(cData, paste(cname, year, sep="_"))
data$nConf = cData$nConf[match(data$id, cData$id)]
data$nConf[is.na(data$nConf)] = 0
########################################################

########################################################
# run models
dv = 'civVicCount'
vars = c(
	'graph_dens', 'graph_recip', 'graph_trans', 
	'nConf', 'nActors', 
	'polity2'
	# 'rebsStronger',
	,'ethTens', 'anyPeaceKeeper'
	# 'rebSupportGov', 'govSupportGov'	
	)
modData=data
########################################################

########################################################
# naive just impute everything
loadPkg('sbgcop')
if(!file.exists(paste0(pathData, 'imputedData_acledUndirected.rda'))){
	impData = data.matrix(modData[,c(5:6,10,18:19,22:38,42:43,47:48,51,53)])
	sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
	dimnames(sbgData$Y.impute)[[2]] = colnames(sbgData$Y.pmean)
	save(sbgData, file=paste0(pathData, 'imputedData_acledUndirected.rda'))
} else { load(file=paste0(pathData, 'imputedData_acledUndirected.rda')) }

# randomly pick a few imputed datasets to use
set.seed(6886)
# sbgToUse = sample(500:1000, 150, replace=FALSE)
iData = lapply(500:1000, function(i){
	sbgFrom = sbgData$Y.impute[,,i]
	modData = cbind(
		modData[,c('cname','year','cnameYear','nActors','nConf')], 
		sbgFrom
		)
	return(modData)	})
########################################################

########################################################
# run base model
modsBase = lapply(iData, function(modData){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors + factor(cname) -1 
		, data=modData
		)
	summary(mod)
	return(mod) })

# run mod with controls
modsCntrls = lapply(iData, function(modData){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors
			+ factor(cname) -1 
			+ polity2   # structural controls
			+ rebsStronger # capabilities gov/rebels
			+ ethTens
			+ anyPeaceKeeper 
			+ rebSupportGov + govSupportGov # external shit
		, data=modData
		)
	summary(mod)
	return(mod) })

# summarize
summBase = rubinCoef(modsBase, TRUE)
round(summBase[!grepl('factor',rownames(summBase)),], 3)

summCntrls = rubinCoef(modsCntrls, TRUE)
round(summCntrls[!grepl('factor',rownames(summCntrls)),], 3)
########################################################