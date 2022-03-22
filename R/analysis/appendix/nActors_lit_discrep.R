########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('dorffc')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('MASS', 'glmmTMB'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))

# set up spec
dv = 'civVicCount'
ivs = c('herf', 'nConf', 'nActors')

# our base model which includes herf and has fixed effects
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))

# model whcih will exclude herf but include fixed effects
#### it's not just the inclusion of herf that is causing the diff
p_noHerf = length(ivs)-1
feF_noHerf = formula(paste0(
  dv, '~', paste(ivs[-1], collapse='+'), '+factor(cname)-1'))

# model which will exclude fixed effects and exclude herf
#### and here is where the lit finding of nactors being positive gets recovered
p_lit = length(ivs)
fe_lit = formula(paste0(
  dv, '~', paste(ivs[-1], collapse='+') ))

# org for mod run
forms = list( feF, feF_noHerf, fe_lit )
numIVs = list(p, p_noHerf, p_lit)
########################################################

########################################################
# run base mods
coefFE = lapply(1:length(forms), function(ii){

  # run mod
  feMod = glm.nb(forms[[ii]], data=dataBase)

  # extract coefs
  feModCoef = summary(feMod)$'coefficients'[1:numIVs[[ii]],,drop=FALSE]

  #
  return(feModCoef) })
########################################################

############################
# get viz

# add labels
mLabs = c(
	'Base ACLED Model\nwith Fixed Effects',
	'Model Excluding Network Competition\nwith Fixed Effects',
  'Model Excluding Network Competition\nwithout Fixed Effects' )

varKey = data.frame(
	dirty=rownames(coefFE[[1]]), stringsAsFactors = FALSE )
varKey$clean = c(
	'Network\nCompetition', 'Number of\nConflicts', 'Number of\nActors' )
varKey = rbind(varKey, c('(Intercept)', 'Intercept'))

# process coefs
ggDataFE = coefProcess(coefFE)
ggDataRE = coefProcess(coefRE)

# create and save viz to pathGraphics
coefViz(ggDataFE, fName='')
############################
