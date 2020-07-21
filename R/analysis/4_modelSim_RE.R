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
loadPkg(c('MASS','reshape2', 'glmmTMB'))
########################################################

########################################################
# load data
load(paste0(pathData, 'modelDataCnt2.rda'))

# load models
load(paste0(pathResults, 'cnt2Mods.rda'))
load(paste0(pathResults, 'baseMods.rda'))
########################################################

########################################################
# sim
## pick a random model
# modsCntrls = reModCnt2_imp
# set.seed(876) ; randMod = sample(1:length(modsCntrls),1)
# mod = modsCntrls[[randMod]]
mod = reModBase

# extract params from model
beta = summary(mod)$'coefficients'$cond[,1]
varCov = vcov(mod, full=TRUE)[names(beta),names(beta)]

# extract draws from model
set.seed(6886)
draws = mvrnorm(10000, beta, varCov)

# set up scenario matrix
# data = iData[[1]]
slice = data
densRange = sort(unique(slice$graph_dens))
# densRange = quantile(slice$graph_dens, probs=seq(0,1,.25))
densRange = quantile(slice$graph_dens, c(0.025, 0.975))
medNA = function(x){median(x,na.rm=TRUE)}
meaNA = function(x){mean(x,na.rm=TRUE)}
scen = cbind(
	1,
	densRange,
	medNA(data$nConf), medNA(data$nActors)
	# meaNA(data$polity2), meaNA(data$popLog),
	# meaNA(data$gdpCapLog), meaNA(data$exclpop),
	# medNA(data$anyPeaceKeeper),
	# meaNA(data$rebsStronger), meaNA(data$rebSupportGov),
	# meaNA(data$govSupportGov)
	)

# generate predicted values
preds = scen %*% t(draws)
yHat = apply(preds, 2, exp)
colnames(yHat) = paste0('scen',1:ncol(yHat))
yHat = data.frame(cbind(densRange, yHat))
########################################################

########################################################
# observed value approach
gDensRange = with(data, seq(
	min(graph_dens), max(graph_dens), .01))

predCnts = lapply(gDensRange, function(gDensVal){

	# set up scenario
	X = with(data,
				cbind(
					1, gDensVal,
					nConf, nActors
					# , polity2, popLog,
					# gdpCapLog, exclpop,
					# anyPeaceKeeper,
					# rebsStronger, rebSupportGov,
					# govSupportGov
				))

	# matrix mult
	preds = X %*% t(draws)

	# calculate summ stats for specified gDensVal
	p = c(exp(preds))
	mu = mean(p)
	med = median(p)
	qhi95 = quantile(p, 0.975)
	qlo95 = quantile(p, 0.025)
	qhi90 = quantile(p, 0.95)
	qlo90 = quantile(p, 0.05)

	# org data
	summ = data.frame(
		gDensVal,
		mu, med, qhi95, qlo95, qhi90, qlo90,
		stringsAsFactors=FALSE
	)

	#
	return(summ)
})

ggData = data.frame(
	do.call('rbind', predCnts),
	stringsAsFactors=FALSE
)

ggplot(ggData, aes(x=gDensVal)) +
	geom_line(aes(y=med)) +
	geom_ribbon(aes(ymin=qlo90, ymax=qhi90), alpha=.4) +
	geom_ribbon(aes(ymin=qlo95, ymax=qhi95), alpha=.2) +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank()
	)
########################################################

########################################################
agg = yHat %>%
	reshape2::melt(., id='densRange') %>%
	dplyr::group_by(densRange) %>%
	dplyr::summarize(
		med = mean(value),
		hi95=quantile(value,.975),
		hi90=quantile(value,.95),
		lo95=quantile(value,.025),
		lo90=quantile(value,.05)
	)

# viz
ggplot(agg, aes(x=densRange, y=med)) +
	geom_line() +
	geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha=.2, colour='grey') +
	geom_ribbon(aes(ymin=lo90, ymax=hi90), alpha=.4, colour='grey') +
	xlab('Graph Density') + ylab('Civilian Victimization') +
	theme(
		axis.ticks = element_blank(),
		panel.border = element_blank()
	)
########################################################
