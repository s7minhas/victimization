########################################################
source(paste0(here::here(), '/setup.R'))

# helpful pkgs
loadPkg(c('MASS', 'glmmTMB'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))

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
# restrict data to 2019
dataBase = dataBase[dataBase$year<=2019,]
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
coefMissFE = list(
	base = feModBaseCoef )

coefMissRE = list(
	base = reModBaseCoef )
########################################################

########################################################
varKey = data.frame(
	dirty=rownames(coefMissFE$'base'), stringsAsFactors = FALSE )
varKey$clean = c(
	'Network\nCompetition', 'Number of\nConflicts', 'Number of\nActors')

mLabs = 'Base ACLED Model'
ggDataMissFE = coefProcess(coefMissFE)
ggDataMissRE = coefProcess(coefMissRE)
########################################################

########################################################
# comparison of base model: fe vs re
baseFE = ggDataMissFE
baseRE = ggDataMissRE
baseFE$model = 'Base ACLED Model\n(Fixed Country Effects)'
baseRE$model = 'Base ACLED Model\n(Random Country Effects)'
ggDataBaseMod = rbind(baseFE, baseRE)
coefViz(ggDataBaseMod, 'appendix/figure_A16.png')
########################################################
