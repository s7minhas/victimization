#### geoSpread is calculated in the following dir:
#### R/data/getData/getGeoSpread.R

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

# load geoSpread data
load(paste0(pathData, 'geoSpread_acled.rda'))

# set dv
dv = 'civVicCount'

# add id var to geoSpread
geoSpread$id = with(geoSpread, paste(cname(country), year, sep='_'))

# merge
dataBase$geoSpread = geoSpread$spreadMu[match(dataBase$id,geoSpread$id)]
dataCnt1$geoSpread = geoSpread$spreadMu[match(dataCnt1$id,geoSpread$id)]
dataCnt2$geoSpread = geoSpread$spreadMu[match(dataCnt2$id,geoSpread$id)]
########################################################

########################################################
# mod spec
ivs = c('geoSpread', 'herf', 'nConf', 'nActors')
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
	'Geographic Spread',
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

pathGraphics = 'C:/Users/S7M/Desktop/'

coefViz(ggDataFE, 'coefPlot_FE_geoSpread.pdf')
coefViz(ggDataRE, 'coefPlot_RE_geoSpread.pdf')
############################
