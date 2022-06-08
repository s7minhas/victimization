# setup #########################################
source(paste0(here::here(), '/setup.R'))
loadPkg(c('MASS', 'foreach', 'doParallel'))
################################################

# load in data #################################
load(paste0(pathData, 'netStats.rda'))
################################################

################################################
# check mean and var of vic count
# to justify use of neg binom
mean(netStats$vic, na.rm=TRUE)
var(netStats$vic, na.rm=TRUE)
################################################

################################################
# run base mod in parallel
perfVars = c('herf_gen', 'herf_und')
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c( 'MASS' ) ) %dopar% {

form=formula(paste0('vic~numConf+n_actors+', v, '+ factor(game)-1'))
mod = glm.nb(form, data=netStats)

return(mod) }
stopCluster(cl)
names(res) = perfVars

# save just coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'})
save(coefs,
	file=paste0(pathResults, 'abm_feCoefs.rda') )
################################################

################################################
# run base mod + allyProp in parallel
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c( 'MASS' ) ) %dopar% {
form=formula(paste0('vic~numConf+n_actors+allyProp+', v, '+ factor(game)-1'))
mod = glm.nb(form, data=netStats)
summary(netStats)
return(mod) }
stopCluster(cl)
names(res) = perfVars

# save just coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'})
save(coefs,
	file=paste0(pathResults, 'abm_feCoefs_allyProp.rda') )
################################################
