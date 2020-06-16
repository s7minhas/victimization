########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('dorffc')){
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

load(paste0(pathData, 'cntriesACLED_byAll.rda'))
aData = aData[,c('COUNTRY', 'YEAR')]
aData$cnt = 1
aData = aData %>% group_by(COUNTRY, YEAR) %>%
	summarize(nConf = sum(cnt)) %>% data.frame()
aData$cname = cname(aData$COUNTRY)
aData$id = with(aData, paste(cname, YEAR, sep='_'))
data$nConf2 = aData$nConf[match(data$id, aData$id)]
data$nConf2[is.na(data$nConf2)] = 0
########################################################

########################################################
# impute
loadPkg('sbgcop')
impVars = c(
	'graph_trans', 'graph_dens', 'graph_avgDeg', 'graph_meanDist', 'graph_localTrans',
	'polity2', 'gdpCapLog', 'popLog', 'cinc', 'govtStab', 'socEconCon', 'invProf',
	'intConf', 'extConf', 'corr', 'milPol', 'relPol', 'lawOrd', 'ethTens', 'demAcct',
	'burQual', 'civVicCount', 'ethfrac', 'exclgrps', 'exclpop', 'totalPeacekeepers',
	'anyPeaceKeeper', 'rebsStronger', 'rebsFightCapHigh', 'rebSupportGov',
	'govSupportGov'
)
if(!file.exists(paste0(pathData, 'imputedData_acledUndirected_v2.rda'))){
	impData = data.matrix(data[,impVars])
	sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
	dimnames(sbgData$Y.impute)[[2]] = colnames(sbgData$Y.pmean)
	save(sbgData, file=paste0(pathData, 'imputedData_acledUndirected_v2.rda'))
} else { load(file=paste0(pathData, 'imputedData_acledUndirected_v2.rda')) }

# randomly pick a few imputed datasets to use
set.seed(6886)
# sbgToUse = sample(500:1000, 150, replace=FALSE)
iData = lapply(500:1000, function(i){
	sbgFrom = sbgData$Y.impute[,,i]
	data = cbind(
		data[,c('cname','year','cnameYear','nActors','nConf', 'nConf2')],
		sbgFrom
		)
	return(data)	})

# save imputed data
save(data, iData, file=paste0(pathData, 'iData_acled.rda'))
########################################################
