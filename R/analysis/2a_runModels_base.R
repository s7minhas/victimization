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
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData_v3.rda'))

# set up spec
dv = 'civVicCount'
ivs = c('herf', 'nConf', 'nActors')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))
########################################################

########################################################
# run base mods
feModBase = glm.nb(feF, data=dataBase)
reModBase = glmmTMB(reF, data=dataBase, family='nbinom2')

# get coefs
feModBaseCoef = summary(feModBase)$'coefficients'[1:p,]
reModBaseCoef = summary(reModBase)$'coefficients'$cond[1:(p+1),]
########################################################

########################################################
# save
save(
  feModBase, reModBase,
  feModBaseCoef, reModBaseCoef,
  file=paste0(pathResults, 'baseMods_v3.rda')
)
########################################################
