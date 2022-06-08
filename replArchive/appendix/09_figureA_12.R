# setup #########################################
source(paste0(here::here(), '/setup.R'))

#
loadPkg(c('MASS', 'foreach', 'doParallel', 'glmmTMB'))

# load simhelper
if(!'simHelper' %in% installed.packages()[,1]){
  devtools::install_github('s7minhas/simHelper', ref='vic') }
library(simHelper)
################################################

# load in data #################################
load(paste0(pathData, 'netStats.rda'))
################################################

################################################
# mods to iterate through
parInfo = expand.grid(
	gVal = unique(netStats$govStrengthBin),
	mod = c('re', 'fe') )
for(i in 1:ncol(parInfo)){ parInfo[,i] = char(parInfo[,i]) }
################################################

################################################
# rescale herf for interp purposes
netStats$herf_und2 = 1-netStats$herf_und

# run model
form=formula(
	paste0(
		'vic~numConf+n_actors+herf_und2+govStrengthBin+
		herf_und2*govStrengthBin+(1|game)'))
mod = glmmTMB( form, data=netStats, family='nbinom2' )
out = summary(mod)$'coefficients'$cond
################################################

################################################
# sim analysis to understand interaction effect
# org model results
beta = summary(mod)$'coefficients'$cond[,1]
varcov = vcov(mod, full=TRUE)[names(beta),names(beta)]

# set up scen
scen = scenBuild(
		mData = mod$frame,
		ivs = names(beta)[-c(1,6)],
		ivStats = rep('mean', length(names(beta)[-c(1,6)])),
		treatVar = c('herf_und2','govStrengthBin'),
		treatCategorical=c(FALSE, TRUE) )

# get predictions
preds = getPreds(beta, varcov, scen, 'count', 6886, 1000)

# summarize by scen
summ = preds %>%
	group_by(herf_und2, govStrengthBin) %>%
	summarize(
		mu = mean(pred),
		hi95 = quantile(pred, 0.975),
		hi90 = quantile(pred, 0.95),
		lo95 = quantile(pred, 0.025),
		lo90 = quantile(pred, 0.05) )

# relabel
summ$govStrengthBin[summ$govStrengthBin==0] = 'Rebels Stronger'
summ$govStrengthBin[summ$govStrengthBin==1] = 'Rebels Weaker'
summ$govStrengthBin = factor(
	summ$govStrengthBin,
	levels=rev(sort(unique(summ$govStrengthBin))) )

#
simViz = ggplot(summ, aes(
	x=herf_und2, y=mu,
	color=factor(govStrengthBin),
	group=factor(govStrengthBin),
	fill=factor(govStrengthBin)) ) +
	geom_line() +
	geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha=.5) +
	geom_ribbon(aes(ymin=lo90, ymax=hi90), alpha=.7) +
	scale_fill_manual(values=c('#969696', '#252525')) +
	scale_color_manual(values=c('#969696', '#252525'))	+
  labs(
    x='Network Competition',
    y='Predicted Number\nof Civilian Fatalities (ABM)',
		color='', fill=''
  ) +
  theme_light(base_family = "Source Sans Pro") +
	theme(
		legend.position='top',
    axis.ticks=element_blank(),
    panel.border=element_blank()	)

ggsave(simViz,
	width=7, height=3, dpi=600,
	file=paste0(pathGraphics, 'appendix/figure_A12.png'))
################################################
