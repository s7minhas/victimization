##########################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('dorffc')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('MASS', 'sbgcop'))
##########################

##########################
# load data
load(paste0(pathData, 'data_v2.rda'))
##########################

##########################
# define vars from model
# yrs when listed account for lag
ids = c('id', 'cname', 'ccode', 'year')
dv = 'civVicCount'
ivsBase = c(
	'graph_dens', 'graph_avgDeg',
	'nActors', 'nEvents', 'nConf', 'herf'
)
ivCnt1 = c(
	'polity2', # -2019
	'gdp', 'pop', 'gdpCap', 'gdpLog', 'popLog', 'gdpCapLog', # -2020
	'exclpop', 'anyPeaceKeeper' # -2018
)
ivCnt2 = c(
	'rebsStronger', 'rebSupportGov', 'govSupportGov' # -2015
)

# create subsets of data based on yr breaks
dataBase = data[,c(ids, dv, ivsBase)]
dataCnt1 = data[
	which(data$year<=2018),
	c(ids, dv, ivsBase, ivCnt1)]
dataCnt2 = data[
	which(data$year<=2015),
	c(ids, dv, ivsBase, ivCnt1, ivCnt2)]
##########################

##########################
# limit countries with too few cyears
dataList = list(base=dataBase, cnt1=dataCnt1, cnt2=dataCnt2)
toRemove = c(
	ids, "graph_avgDeg", "nEvents", "gdp", "pop", "gdpCap", "gdpLog" )
modVars = names(dataCnt2)[-which(names(dataCnt2) %in% toRemove)]

cntryCnts = lapply(dataList, function(x){
	vars = intersect(names(x), c('cname',modVars))
	x = x[,c('cname',vars)]
	x = na.omit(x)
	out = x %>% group_by(cname) %>% summarize(cnt=n())
	return(out) })

cntryKeep = lapply(cntryCnts, function(x){ x$cname[x$cnt>=3] })

dataList = lapply(1:length(cntryKeep), function(ii){
	toKeep = cntryKeep[[ii]] ; dat = dataList[[ii]]
	dat = dat[dat$cname %in% toKeep,]
	return(dat) })

dataBase = dataList[[1]]
dataCnt1 = dataList[[2]]
dataCnt2 = dataList[[3]]
##########################

##########################
# impute and save
getImps = function(data, remVars, fileName){

	# organize data
	keepVars = setdiff(names(data), remVars)
	idData = data[,remVars]
	impData = data.matrix(data[,keepVars])

	# for wb vars impute raw data
	toDrop = match(c('gdpLog','gdpCapLog','popLog'),colnames(impData))
	impData = impData[,-toDrop]

	# run sbgcop
	sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
	dimnames(sbgData$Y.impute)[[2]] = colnames(sbgData$Y.pmean)

	# pull out obs from posterior
	iData = lapply(500:1000, function(i){
		data = cbind( idData, sbgData$Y.impute[,,i] )
		out = data.frame(as_tibble(data), stringsAsFactors=FALSE)

		# create logged versions of wbvars
		out$gdpCapLog = log(out$gdpCap + 1)
		out$popLog = log(out$pop + 1)
		out$gdpLog = log(out$gdp + 1)

		# return
		return( out ) })

	save(iData, data, sbgData, file=paste0(pathData, fileName))
}

# run imp on cnt1
getImps(dataCnt1, ids[1:2], 'modelDataCnt1_v3.rda')

# run imp on cnt2
getImps(dataCnt2, ids[1:2], 'modelDataCnt2_v3.rda')

# save raw
save(
	dataBase, dataCnt1, dataCnt2,
	file=paste0(pathData, 'rawModelData_v3.rda'))
##########################
