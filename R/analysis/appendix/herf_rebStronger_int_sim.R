########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('MASS', 'glmmADMB', 'glmmTMB'))

# load simhelper
if(!'simHelper' %in% installed.packages()[,1]){
  devtools::install_github('s7minhas/simHelper', ref='vic') }
library(simHelper)
########################################################

########################################################
# load imputed data for cnt2
# data = unimputed data for cnt2
# iData = list of imputed datasets
# sbgData = imputation model results
load(paste0(pathData, 'modelDataCnt2.rda'))

# set up spec
dv = 'civVicCount'
ivs = c(
  'herf', 'nConf', 'nActors',
  'polity2', 'popLog', 'gdpCapLog', 'exclpop',
 	'anyPeaceKeeper',
	'rebsStronger', 'rebSupportGov', 'govSupportGov')
p = length(ivs)
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+herf*rebsStronger +(1|cname)'))
########################################################

########################################################
# run model
data$rebsStronger[data$rebsStronger>0] = 1
reModCnt2 = glmmTMB(reF, data=data, family='nbinom2')

# randomly sample iData
set.seed(6886)
toSamp = sample(1:length(iData), 10, replace=FALSE)
dataList = iData[toSamp]

reModCnt2_imp = lapply(dataList, function(dataImp){
  glmmTMB(reF, data=dataImp, family='nbinom2') })
########################################################

########################################################
# org mod
mod = reModCnt2
beta = summary(mod)$'coefficients'$cond[,1]
varcov = vcov(mod, full=TRUE)[names(beta),names(beta)]

# ranges for vars of interest
herfVals = seq(
	quantile(mod$frame$herf,.1),
	quantile(mod$frame$herf,.9), length.out=20)
rebVals = c(0,1)

# set up scen
scen = scenBuild(
		mData = mod$frame,
		ivs = names(beta)[-c(1,13)],
		ivStats = rep('median', length(names(beta)[-c(1,13)])),
		treatVar = c('herf','rebsStronger'),
		treatCategorical=c(FALSE, TRUE),
		treatVals = list(herfVals, rebVals) )

# get preds
preds = getPreds(beta, varcov, scen, 'count', 6886, 1000)

# summarize by scen
summ = preds %>%
	group_by(herf, rebsStronger) %>%
	summarize(
		mu = median(pred),
		hi95 = quantile(pred, 0.975),
		hi90 = quantile(pred, 0.95),
		lo95 = quantile(pred, 0.025),
		lo90 = quantile(pred, 0.05) )

# relabel
summ$rebsStronger[summ$rebsStronger==0] = 'Rebels Weaker'
summ$rebsStronger[summ$rebsStronger==1] = 'Rebels Stronger'
summ$rebsStronger = factor(
	summ$rebsStronger,
	levels=rev(sort(unique(summ$rebsStronger))) )

# viz
simViz= ggplot(summ, aes(
	x=herf, y=mu,
	color=factor(rebsStronger),
	group=factor(rebsStronger),
	fill=factor(rebsStronger)) ) +
	geom_line() +
	geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha=.5) +
	geom_ribbon(aes(ymin=lo90, ymax=hi90), alpha=.7) +
	facet_wrap(~factor(rebsStronger), scales='free') +
	scale_fill_manual(values=c('#969696', '#252525')) +
	scale_color_manual(values=c('#969696', '#252525'))	+
  labs(
    x='Network Competition',
    y='Predicted Number of Civilian Fatalities',
		color='', fill=''
  ) +
  theme_light(base_family = "Source Sans Pro") +
	theme(
		legend.position='top',
    axis.ticks=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(size = 9,color = "white"),
    strip.background = element_rect(fill = "#525252", color = "#525252") )

#
ggsave(simViz,
  file=paste0(pathGraphics, 'emp_rebStrengthSim.pdf'),
  width=8, height=3, device=cairo_pdf)
########################################################
