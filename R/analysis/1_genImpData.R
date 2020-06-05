########################################################
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
########################################################

########################################################
# impute
loadPkg('sbgcop')
if(!file.exists(paste0(pathData, 'imputedData_acledUndirected.rda'))){
	impData = data.matrix(data[,c(5:6,10,18:19,22:38,42:43,47:48,51,53)])
	sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
	dimnames(sbgData$Y.impute)[[2]] = colnames(sbgData$Y.pmean)
	save(sbgData, file=paste0(pathData, 'imputedData_acledUndirected.rda'))
} else { load(file=paste0(pathData, 'imputedData_acledUndirected.rda')) }

# randomly pick a few imputed datasets to use
set.seed(6886)
# sbgToUse = sample(500:1000, 150, replace=FALSE)
iData = lapply(500:1000, function(i){
	sbgFrom = sbgData$Y.impute[,,i]
	data = cbind(
		data[,c('cname','year','cnameYear','nActors','nConf')],
		sbgFrom
		)
	return(data)	})

# save imputed data
save(data, iData, file=paste0(pathData, 'iData_acled.rda'))
########################################################s
