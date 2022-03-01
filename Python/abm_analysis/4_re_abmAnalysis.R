# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

loadPkg(c('glmmTMB', 'foreach', 'doParallel'))
################################################

# load in data #################################
load(paste0(abmPath, 'netStats.rda'))
################################################

################################################
# check mean and var of vic count
# to justify use of neg binom
mean(netStats$vic, na.rm=TRUE)
var(netStats$vic, na.rm=TRUE)
################################################

################################################
# run base mods in parallel
perfVars = c('herf_gen', 'herf_und')
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c( 'glmmTMB' ) ) %dopar% {

form=formula(paste0('vic~numConf+n_actors+', v, '+ (1|game)'))
mod = glmmTMB( form, data=netStats, family='nbinom2' )

return(mod) }
stopCluster(cl)
names(res) = perfVars

# save full models
save(res,
	file=paste0(pathResults, 'abm_reMods.rda'))

# save coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'$cond})
save(coefs,
	file=paste0(pathResults, 'abm_reCoefs.rda'))
################################################

################################################
# run base mods + allyProp in parallel
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c( 'glmmTMB' ) ) %dopar% {

form=formula(paste0('vic~numConf+n_actors+allyProp+', v, '+ (1|game)'))
mod = glmmTMB(form, data=netStats, family='nbinom2')

return(mod) }
stopCluster(cl)
names(res) = perfVars

# save full models
save(res,
	file=paste0(pathResults, 'abm_reMods_allyProp.rda'))

# save coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'$cond})
save(coefs,
	file=paste0(pathResults, 'abm_reCoefs_allyProp.rda'))
################################################
