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

# load models
# load(paste0(pathResults, 'nbMods_acled.rda'))
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
modsCntrls = list(modsCntrls_noImp_RE)
########################################################

########################################################
# sim 
## pick a random model
set.seed(6886) ; randMod = sample(1:length(modsCntrls),1)
mod = modsCntrls[[randMod]]

# extract params from model
beta = coef(mod)
varCov = vcov(mod)

# extract draws from model
set.seed(6886)
draws = mvrnorm(1000, beta, varCov)

# pick random country
set.seed(6886)
randCntry = sample(
	names(beta)[grepl('factor',names(beta))],
	1 )

# subset draws
draws = draws[,
	c(
		# randCntry,
		names(beta)[!grepl('factor',names(beta))]
		)
	]

# set up scenario matrix
densRange = sort(unique(data$graph_dens))
medNA = function(x){median(x,na.rm=TRUE)}
meaNA = function(x){mean(x,na.rm=TRUE)}
scen = cbind(
	# 1, 
	densRange,
	medNA(data$nConf), medNA(data$nActors),
	meaNA(data$polity2), meaNA(data$popLog),
	meaNA(data$gdpCapLog), meaNA(data$ethfrac),
	medNA(data$anyPeaceKeeper)
	)

# generate predicted values
preds = scen %*% t(draws)
yHat = apply(preds, 2, exp)
colnames(yHat) = paste0('scen',1:ncol(yHat))
yHat = data.frame(cbind(densRange, yHat))

# org data for plotting
ggData = melt(yHat, id='densRange')

agg = ggData %>% group_by(densRange) %>% summarize(mu=mean(value))
ggplot(agg, aes(x=densRange,y=mu)) + geom_line()
########################################################

########################################################
# viz
ggplot(ggData, 
	aes(x=densRange, y=value, group=variable)
	) +
	geom_line()
########################################################