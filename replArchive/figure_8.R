########################################################
source(paste0(here::here(), '/setup.R'))
library(devtools)
# load simhelper
if(!'simHelper' %in% installed.packages()[,1]){
  devtools::install_github('s7minhas/simHelper', ref='vic') }
library(simHelper)
########################################################

########################################################
load(paste0(pathResults, 'baseMods.rda'))
load(paste0(pathResults, 'cnt1Mods.rda'))
load(paste0(pathResults, 'cnt2Mods.rda'))
########################################################

########################################################
# get complete herf dat
herfData = reModBase$frame ; herfData$mu = 1

# mods to iterate through with labs
## results similar no matter which imputed model we use
## and also similar if using listwise deletion data
mods = list(reModBase, reModCnt1_imp[[1]], reModCnt2_imp[[1]])
modLabs = c(
	'Base ACLED Model',
	'Base + Controls\n(1997-2018)', 'Base + Controls\n(1997-2015)' )

# iterate through mods and conduct sim analysis
# to estimate eff of herf
summPreds = lapply( 1:length(mods), function(ii){

  # subset to a mod
  mod = mods[[ii]]

  # get par info from mod
  beta = summary(mod)$'coefficients'$cond[,1]
  varcov = vcov(mod, full=TRUE)[names(beta),names(beta)]

  # set up scen
  scen = scenBuild(
  		mData = mod$frame,
  		ivs = names(beta)[-1],
  		ivStats = rep('mean', length(names(beta)[-1])),
  		treatVar = 'herf',
  		treatCategorical=FALSE )

  # get preds
  preds = getPreds(beta, varcov, scen, 'count', 6886, 1000)

  # summarize by scen
  summ = preds %>%
  	group_by(herf) %>%
  	summarize(
  		mu = mean(pred),
  		hi95 = quantile(pred, 0.975),
  		hi90 = quantile(pred, 0.95),
  		lo95 = quantile(pred, 0.025),
  		lo90 = quantile(pred, 0.05) )

  # add mod lab
  summ$modLab = modLabs[ii]

  #
  return(summ) })

# org
summPreds = do.call('rbind', summPreds)
summPreds$modLab = factor(summPreds$modLab, levels=modLabs)

# viz
herfSimViz = ggplot(summPreds, aes(x=herf, y=mu)) +
	geom_line() +
	geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha=.5) +
	geom_ribbon(aes(ymin=lo90, ymax=hi90), alpha=.7) +
  labs(
    x='Network Competition',
    y='Predicted Number of Civilian Fatalities'
  ) +
  # geom_rug(
  #   data=herfData, aes(x=herf),
  #   sides='b', alpha=.2, position='jitter') +
  facet_wrap(~modLab, nrow=1, scales='free_y') +
  theme_light(base_family = "Source Sans Pro") +
  theme(
    axis.ticks=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(size = 9,color = "white"),
    strip.background = element_rect(fill = "#525252", color = "#525252") )

#
ggsave(herfSimViz,
  file=paste0(pathGraphics, 'figure8.png'),
  width=8, height=3, dpi=600)
########################################################
