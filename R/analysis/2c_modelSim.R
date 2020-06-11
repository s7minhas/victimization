########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
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
load(paste0(pathResults, 'nbMods_acled.rda'))
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
		randCntry,
		names(beta)[!grepl('factor',names(beta))]
		)
	]

# set up scenario matrix
densRange = sort(unique(data$graph_dens))
medNA = function(x){median(x,na.rm=TRUE)}
meaNA = function(x){mean(x,na.rm=TRUE)}
scen = cbind(
	1,
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
########################################################

########################################################
# viz
agg = ggData %>%
	group_by(densRange) %>%
	summarize(
		med=median(value),
		hi95=quantile(value,.975),
		hi90=quantile(value,.95),
		lo95=quantile(value,.025),
		lo90=quantile(value,.05)
		)
ggplot(agg, aes(x=densRange,y=mu)) + geom_line()
########################################################
