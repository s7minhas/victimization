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
load(paste0(pathData, 'modelDataCnt2_v3.rda'))

# set up spec
dv = 'civVicCount'
ivs = c(
  'herf', 'nConf', 'nActors',
  'polity2', 'popLog', 'gdpCapLog', 'exclpop',
 	'anyPeaceKeeper',
	'rebsStronger', 'rebSupportGov', 'govSupportGov')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))
########################################################

########################################################
# run cnt1 model with no imp
feModCnt2 = glm.nb(feF, data=data)
reModCnt2 = glmmTMB(reF, data=data, family='nbinom2')

# get coefs
feModCnt2Coef = summary(feModCnt2)$'coefficients'[1:p,]
reModCnt2Coef = summary(reModCnt2)$'coefficients'$cond[1:(p+1),]
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
feModCnt2_imp = lapply(dataList, function(dataImp){
  glmmadmb(feF, data=dataImp, family='nbinom') })
reModCnt2_imp = lapply(dataList, function(dataImp){
  glmmTMB(reF, data=dataImp, family='nbinom2') })

# get coefs
feModCnt2Coef_imp = lapply(feModCnt2_imp,
  function(x){ summary(x)$'coefficients'[1:p,] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
reModCnt2Coef_imp = lapply(reModCnt2_imp,
  function(x){ summary(x)$'coefficients'$cond[1:(p+1),] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
########################################################

feModCnt2Coef
reModCnt2Coef
feModCnt2Coef_imp
reModCnt2Coef_imp

########################################################
# save
save(
  feModCnt2, reModCnt2,
  feModCnt2Coef, reModCnt2Coef,
  feModCnt2_imp, reModCnt2_imp,
  feModCnt2Coef_imp, reModCnt2Coef_imp,
  file=paste0(pathResults, 'cnt2Mods_v3.rda')
)
########################################################
