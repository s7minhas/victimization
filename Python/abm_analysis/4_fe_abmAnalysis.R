# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

loadPkg(c('MASS', 'foreach', 'doParallel'))
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
# choose vars to testf
vars = names(netStats)[c(1:11,13:15)]
perfVars = vars[c(1,3,6,12:14)]

# run in parallel
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c( 'MASS' ) ) %dopar% {
form=formula(paste0('vic~numConf+n_actors+', v, '+ factor(game)-1'))
mod = glm.nb(form, data=netStats)
summary(netStats)
return(mod) }
stopCluster(cl)
names(res) = perfVars

# save full models
save(res,
	file=paste0(pathResults, 'abm_feMods_v2_py39.rda'))

# save just coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'})
save(coefs,
	file=paste0(pathResults, 'abm_feCoefs_v2_py39.rda') )
################################################
