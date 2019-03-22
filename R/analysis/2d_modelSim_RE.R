########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

#
loadPkg(c('MASS','reshape2'))
########################################################

########################################################
# load data
load(paste0(pathData, 'iData_acled.rda'))
########################################################

########################################################
# check against random effect
loadPkg('lme4')
toKeep = names(table(data$cname)[table(data$cname)>5])
slice = data[data$cname %in% toKeep,]
slice$ccode = factor(slice$ccode)
for(v in c('nConf','nActors','graph_dens')){
	slice[,v] = (slice[,v] - mean(slice[,v]))/sd(slice[,v])
}
modBase_noImp_RE = glmer.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors + (1|ccode)
	, data=slice
	)

modsCntrls_noImp_RE = glmer.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors
		+ (1|ccode)
		+ polity2 + popLog + gdpCapLog   # structural controls
		+ ethfrac
		+ anyPeaceKeeper 
	, data=slice
	)
modsCntrls = list(modBase_noImp_RE)
########################################################

########################################################
# sim 
## pick a random model
set.seed(6886) ; randMod = sample(1:length(modsCntrls),1)
mod = modsCntrls[[randMod]]

# extract params from model
beta = fixef(mod)
varCov = vcov(mod)

# extract draws from model
set.seed(6886)
draws = mvrnorm(1000, beta, varCov)

# set up scenario matrix
densRange = sort(unique(slice$graph_dens))
densRange = quantile(slice$graph_dens, probs=c(0.25,0.75))
medNA = function(x){median(x,na.rm=TRUE)}
meaNA = function(x){mean(x,na.rm=TRUE)}
scen = cbind(
	1, 
	densRange,
	medNA(slice$nConf), medNA(slice$nActors)
	# ,meaNA(data$polity2), meaNA(data$popLog),
	# meaNA(data$gdpCapLog), meaNA(data$ethfrac),
	# medNA(data$anyPeaceKeeper)
	)

# generate predicted values
preds = scen %*% t(draws)
yHat = apply(preds, 2, exp)
colnames(yHat) = paste0('scen',1:ncol(yHat))
yHat = data.frame(cbind(densRange, yHat))
########################################################

########################################################
# viz
ggData = melt(yHat, id='densRange')
ggplot(ggData, 
	aes(x=value, fill=factor(densRange),color=factor(densRange))
	) +
	geom_density(alpha=.1) +
	facet_wrap(~factor(densRange),nrow=2)
########################################################