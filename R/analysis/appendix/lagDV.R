########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }

# helpful pkgs
loadPkg(c('MASS', 'glmmTMB'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))

# set dv
dv = 'civVicCount'

# create lagged dv
dataBase = cbind(dataBase, lagVar(dataBase, dv))
dataCnt1 = cbind(dataCnt1, lagVar(dataCnt1, dv))
dataCnt2 = cbind(dataCnt2, lagVar(dataCnt2, dv))
########################################################

########################################################
# set ivs
ivs = c(paste0('lag1_',dv), 'herf', 'nConf', 'nActors')
lagBase = glm.nb(f(dv, ivs), data=dataBase)
summary(lagBase)$'coefficients'
########################################################

########################################################
ivs = c( ivs,
  'polity2', 'popLog', 'gdpCapLog',
  'exclpop', 'anyPeaceKeeper' )
lagCnt1 = glm.nb(f(dv,ivs), data=dataCnt1)
summary(lagCnt1)$'coefficients'
########################################################

########################################################
ivs = c( ivs,
  'rebsStronger', 'rebSupportGov', 'govSupportGov')
lagCnt2 = glm.nb(f(dv,ivs), data=dataCnt2)
summary(lagCnt2)$'coefficients'
########################################################
