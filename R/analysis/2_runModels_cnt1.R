########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('MASS', 'glmmTMB'))
########################################################

########################################################
# load imputed data for cnt1
# data = unimputed data for cnt1
# iData = list of imputed datasets
# sbgData = imputation model results
load(paste0(pathData, 'modelDataCnt1.rda'))

# set up spec
dv = 'civVicCount'
ivs = c(
  'graph_dens', 'nConf', 'nActors',
  'polity2', 'popLog', 'gdpCapLog', 'exclpop' )
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))
########################################################

########################################################
# run cnt1 model with no imp
feModCnt1 = glm.nb(feF, data=data)
reModCnt1 = glmmTMB(reF, data=data, family='nbinom2')

# get coefs
feModCnt1Coef = summary(feModCnt1)$'coefficients'
reModCnt1Coef = summary(reModCnt1)$'coefficients'$cond
########################################################

########################################################
# run cnt1 model with imp
loadPkg('glmmADMB')
# randomly sample iData
set.seed(54)
toSamp = sample(1:length(iData), 3, replace=FALSE)
dataList = iData[toSamp]

# iterate across imputed datasets
feModCnt1_imp = lapply(dataList, function(dataImp){
  glmmadmb(feF,
    data=dataImp,
    family='nbinom'
    ) })

lapply(feModCnt1_imp, function(x){summary(x)$'coefficients'[1:7,]})

reModCnt1_imp = lapply(dataList, function(dataImp){
  glmmTMB(reF, data=dataImp, family='nbinom2') })

# get coefs
feModCnt1Coef = summary(feModCnt1)$'coefficients'
reModCnt1Coef = summary(reModCnt1)$'coefficients'$cond
########################################################

# ########################################################
# # save
# save(
#   feModBase, reModBase,
#   feModBaseCoef, reModBaseCoef,
#   file=paste0(pathResults, 'cnt1Mods.rda')
# )
# ########################################################
