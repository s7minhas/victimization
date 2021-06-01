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
loadPkg(c('MASS', 'glmmADMB', 'glmmTMB'))
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
  'herf', 'nConf', 'nActors',
  'polity2', 'popLog', 'gdpCapLog', 'exclpop', 'anyPeaceKeeper' )
p = length(ivs)
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
feModCnt1Coef = summary(feModCnt1)$'coefficients'[1:p,]
reModCnt1Coef = summary(reModCnt1)$'coefficients'$cond[1:(p+1),]
########################################################

########################################################
# run cnt1 model with imp

# randomly sample iData
set.seed(6886)
toSamp = sample(1:length(iData), 10, replace=FALSE)
dataList = iData[toSamp]

# iterate across imputed datasets
# we do run into some estimation issues in the
# fe estimation because of the wb vars
feModCnt1_imp = lapply(dataList, function(dataImp){
  glmmadmb(feF, data=dataImp, family='nbinom') })
reModCnt1_imp = lapply(dataList, function(dataImp){
  glmmTMB(reF, data=dataImp, family='nbinom2') })

# get coefs
feModCnt1Coef_imp = lapply(feModCnt1_imp,
  function(x){ summary(x)$'coefficients'[1:p,] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
reModCnt1Coef_imp = lapply(reModCnt1_imp,
  function(x){ summary(x)$'coefficients'$cond[1:(p+1),] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
########################################################

feModCnt1Coef
reModCnt1Coef
feModCnt1Coef_imp
reModCnt1Coef_imp

########################################################
# save
save(
  feModCnt1, reModCnt1,
  feModCnt1Coef, reModCnt1Coef,
  feModCnt1_imp, reModCnt1_imp,
  feModCnt1Coef_imp, reModCnt1Coef_imp,
  file=paste0(pathResults, 'cnt1Mods.rda')
)
########################################################
