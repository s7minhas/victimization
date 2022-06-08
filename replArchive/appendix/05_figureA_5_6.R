########################################################
source(paste0(here::here(), '/setup.R'))

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
# mod spec
ivs = c(paste0('lag1_',dv), 'herf', 'nConf', 'nActors')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))

# get mods
feModBase = glm.nb(feF, data=dataBase)
reModBase = glmmTMB(reF, data=dataBase, family='nbinom2')

# get coefs
feModBaseCoef = summary(feModBase)$'coefficients'[1:p,]
reModBaseCoef = summary(reModBase)$'coefficients'$cond[1:(p+1),]
########################################################

########################################################
# mod spec
ivs = c( ivs,
  'polity2', 'popLog', 'gdpCapLog',
  'exclpop', 'anyPeaceKeeper' )
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))

# get mods
feModCnt1 = glm.nb(feF, data=dataCnt1)
reModCnt1 = glmmTMB(reF, data=dataCnt1, family='nbinom2')

# get coefs
feModCnt1Coef = summary(feModCnt1)$'coefficients'[1:p,]
reModCnt1Coef = summary(reModCnt1)$'coefficients'$cond[1:(p+1),]
########################################################

########################################################
# mod spec
ivs = c( ivs,
  'rebsStronger', 'rebSupportGov', 'govSupportGov')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))

# get mods
feModCnt2 = glm.nb(feF, data=dataCnt2)
reModCnt2 = glmmTMB(reF, data=dataCnt2, family='nbinom2')

# get coefs
feModCnt2Coef = summary(feModCnt2)$'coefficients'[1:p,]
reModCnt2Coef = summary(reModCnt2)$'coefficients'$cond[1:(p+1),]
########################################################

############################
# get viz

# org coefs
coefFE = list(
	base=feModBaseCoef,
	cnt1=feModCnt1Coef,
	cnt2=feModCnt2Coef )

coefRE = list(
	base=reModBaseCoef,
	cnt1=reModCnt1Coef,
	cnt2=reModCnt2Coef )

# add labels
mLabs = c(
	'Base ACLED Model',
	'Base + Controls\n(1997-2018)', 'Base + Controls\n(1997-2015)' )

varKey = data.frame(
	dirty=rownames(coefFE$'cnt2'), stringsAsFactors = FALSE )
varKey$clean = c(
	'Lag DV',
	'Network\nCompetition', 'Number of\nConflicts', 'Number of\nActors',
	'Polity', 'Log(Population)', 'Log(GDP per Capita)',
	'Excluded\nPopulation', 'Presence of\nPeacekeepers',
	'Rebel(s) Stronger\nthan Govt.',
	'Rebel(s) Supported\nby Foreign Govt.',
	'Govt. Supported\nby Foreign Govt.' )

# process coefs
ggDataFE = coefProcess(coefFE)
ggDataRE = coefProcess(coefRE)

# create and save viz to pathGraphics
coefViz(ggDataFE, 'appendix/figure_A6.png')
coefViz(ggDataRE, 'appendix/figure_A5.png')
############################
