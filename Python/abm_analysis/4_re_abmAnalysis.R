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
mean(netStats$vic)
var(netStats$vic)
################################################

################################################
# convert game counter to factor
netStats$game = factor(netStats$game)

# choose vars to test
vars = names(netStats)[c(1:11,13:15)]
perfVars = vars[c(12,14)]
################################################

mVars = c('game', 'vic', 'numConf', 'n_actors', perfVars)

# netStats = na.omit(netStats[,mVars])
# skip = names(table(netStats$game)[table(netStats$game)<11])
# netStats$game = char(netStats$game)
# netStats = netStats[which(!netStats$game %in% skip),]
# netStats$game = factor(netStats$game)
#
# netStats$game = char(netStats$game)
# ugh = unique(netStats$game)
# length(ugh)
# # netStats = netStats[netStats$game %in% ugh[800:979],]
# netStats$game = factor(netStats$game)

# summ = netStats %>%
# 	group_by(game) %>%
# 	summarize(
# 		mu1 = mean(n_actors),
# 		mu2 = mean(numConf)) %>%
# 	filter(mu2>5 & mu1>3) %>%
# 	data.frame(.,stringsAsFactors=F)
# summary(summ$mu1)
# summary(summ$mu2)
# toKeep = char(summ$game)
# length(toKeep)
# netStats$game = char(netStats$game)
# netStats = netStats[netStats$game %in% toKeep,]
# netStats$game = factor(netStats$game)
v = 'herf_und'
# summary(mod)

################################################
# run base mods in parallel
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c( 'glmmTMB' ) ) %dopar% {

form=formula(paste0('vic~numConf+n_actors+', v, '+ (1|game)'))
mod = glmmTMB(
	form, data=netStats[netStats$govStrengthBin==0,], family='nbinom2'
)
summary(mod)$'coefficients'

mod = glmmTMB(
	form, data=netStats[netStats$govStrengthBin==1,], family='nbinom2'
)
summary(mod)$'coefficients'

summary(lm(log(vic+1)~numConf+n_actors+herf_und+factor(game)-1, data=netStats[netStats$govStrengthBin==0,]))$'coefficients'[1:5,]
summary(lm(log(vic+1)~numConf+n_actors+herf_und+factor(game)-1, data=netStats[netStats$govStrengthBin==1,]))$'coefficients'[1:5,]

library(lme4)
summary(lmer(log(vic+1)~numConf+n_actors+herf_und+(1|game), data=netStats[netStats$govStrengthBin==0,]))$'coefficients'
summary(lmer(log(vic+1)~numConf+n_actors+herf_und+(1|game), data=netStats[netStats$govStrengthBin==1,]))$'coefficients'

summary(lm(log(vic+1)~numConf+n_actors+herf_und+factor(game)-1, data=netStats))$'coefficients'[1:5,]
summary(lmer(log(vic+1)~numConf+n_actors+herf_und+(1|game), data=netStats))$'coefficients'

summary(lmer(log(vic+1)~numConf+n_actors+herf_und+govStrengthBin + (1|game), data=netStats))$'coefficients'

return(mod) }
stopCluster(cl)
names(res) = perfVars

lapply(res, function(x){summary(x)$'coefficients'$cond})

# save full models
save(res,
	file=paste0(pathResults, 'abm_reMods_v3_py39.rda'))

# save coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'$cond})
save(coefs,
	file=paste0(pathResults, 'abm_reCoefs_v3_py39.rda'))
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
	file=paste0(pathResults, 'abm_reMods_v3_py39.rda'))

# save coefs
coefs = lapply(res, function(x){summary(x)$'coefficients'$cond})
save(coefs,
	file=paste0(pathResults, 'abm_reCoefs_allyProp_v3_py39.rda'))
################################################
