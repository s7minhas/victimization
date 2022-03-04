# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

loadPkg(c('MASS', 'foreach', 'doParallel'))
################################################

# load in data #################################
load(paste0(abmPath, 'netStats.rda'))
################################################

################################################
# mods to iterate through
parInfo = expand.grid(
	gVal = unique(netStats$govStrengthBin),
	mod = c('re', 'fe') )
for(i in 1:ncol(parInfo)){ parInfo[,i] = char(parInfo[,i]) }
################################################

################################################
# run base mod in parallel
cores = nrow(parInfo)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	ii = 1:nrow(parInfo),
	.packages=c( 'MASS', 'glmmTMB' ) ) %dopar% {

# get govStrength val and mod type
g = parInfo$gVal[ii]
mType = parInfo$mod[ii]

# run fe mod
if(mType=='fe'){
	form=formula(paste0('vic~numConf+n_actors+herf_und+ factor(game)-1'))
	mod = glm.nb(form, data=netStats)
	out = summary(x)$'coefficients' }

# run re mods
if(mType=='re'){
	form=formula(paste0('vic~numConf+n_actors+herf_und+ (1|game)'))
	mod = glmmTMB( form, data=netStats, family='nbinom2' )
	out = summary(x)$'coefficients'$cond }

return(out) }
stopCluster(cl)
################################################

################################################

################################################
