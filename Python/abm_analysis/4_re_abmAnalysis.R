# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

loadPkg(c('glmmTMB', 'foreach', 'doParallel'))
################################################

# load in data #################################
load(paste0(abmPath, 'netStats_v2_py39.rda'))
################################################

################################################
# check mean and var of vic count
# to justify use of neg binom
mean(netStats$vic)
var(netStats$vic)
################################################

################################################
# convert game counter to factor
netStats$game = factor(netStats$game)

# choose vars to test
vars = names(netStats)[c(1:11,13:15)]
perfVars = vars[c(1,3,6,12:14)]

# run in parallel
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c( 'glmmTMB' ) ) %dopar% {

form=formula(paste0('vic~numConf+n_actors+', v, '+ (1|game)'))
mod = glmmTMB(form, data=netStats, family='nbinom2')

return(mod) }
stopCluster(cl)
names(res) = perfVars

# save full models
save(res,
	file=paste0(pathResults, 'abm_reMods_v2_py39.rda'))

# save coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'$cond})
save(coefs,
	file=paste0(pathResults, 'abm_reCoefs_v2_py39.rda'))
################################################
