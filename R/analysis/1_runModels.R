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
load(paste0(pathData, 'data.rda'))
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
if(!file.exists(paste0(pathData, 'imputedData.rda'))){
	impData = data.matrix(modData[,c(5:7,11,19,20,23,24:36,44,48,49,52,54)])
	sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
	dimnames(sbgData$Y.impute)[[2]] = colnames(sbgData$Y.pmean)
	save(sbgData, file=paste0(pathData, 'imputedData.rda'))
} else { load(file=paste0(pathData, 'imputedData.rda')) }

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
# run mod
mods = lapply(iData, function(modData){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors + 
			+ polity2   # structural controls
			+ rebsStronger # capabilities gov/rebels
			+ ethTens
			+ anyPeaceKeeper 
			+ rebSupportGov + govSupportGov # external shit
		, data=modData
		)
	summary(mod)
	return(mod) })

library(Amelia)
rubinCoef = function(mod, matrixFormat=FALSE){
  modCoef = lapply(mod, function(x){
    beta = coef(x)
    se = sqrt(diag(vcov(x)))
    return( cbind(beta, se) )
    }) %>% do.call('rbind',.) 

  modSumm = mi.meld(
    q=matrix(modCoef[,1],ncol=length(unique(rownames(modCoef))), byrow=TRUE), 
    se=matrix(modCoef[,2],ncol=length(unique(rownames(modCoef))), byrow=TRUE), 
    byrow=TRUE) %>% lapply(., t) %>% do.call('cbind',.) %>% data.frame(.)

  names(modSumm) = c('beta', 'se')
  modSumm$t = modSumm$beta/modSumm$se
  modSumm$var = unique(rownames(modCoef))

  if(matrixFormat){
    names(modSumm) = c('Estimate', 'Std. Error', 't value', 'var')
    rownames(modSumm) = modSumm$var
    modSumm = data.matrix(modSumm[,-ncol(modSumm)]) }

  return(modSumm)
}

rubinCoef(mods)
########################################################