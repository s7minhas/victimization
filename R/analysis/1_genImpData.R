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
load(paste0(pathData, 'data.rda'))
##########################

##########################
# define vars from model
ids = c('id', 'cname', 'ccode', 'year')
dv = 'civVicCount'
ivs = c(
	'graph_dens', 'graph_avgDeg',
	'nActors', 'nEvents', 'nConf',
	'polity2', # -2018
	'popLog', 'gdpCapLog', # -2019
	'exclpop', # -2017
	'anyPeaceKeeper', # -2012
	'rebsStronger', 'rebSupportGov', 'govSupportGov' # -2011
)

# subset data
data = data[,c(ids, dv, ivs)]

# year restriction
# last year of data from nsa is 2011
# so because of lag subset to end data at 2012
data = data[data$year<=2012,]

summary(data[,c(dv, ivs)])
dim(data)

apply(data[,ivs], 2, function(x){sum(is.na(x))}) %>% cbind()
##########################

##########################
# impute
if(!file.exists(paste0(pathData, 'imputedData_acledUndirected_v3.rda'))){
	impData = data.matrix(data[,c(dv, ivs)])
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
##########################
