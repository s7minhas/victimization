# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }
################################################

# load in data #################################
load(paste0(abmPath, 'netStats.rda'))
################################################

# peak at results ##############################
# basic look at results
loadPkg('ggcorrplot')
corr = round(
	cor(
		netStats[,c(1:12, 30, 26)],
		use='pairwise.complete.obs'),3)
ggcorrplot(corr, colors=c('red','white','blue'))

cor(netStats[,1:12])
################################################

################################################
# run neg binom
loadPkg(c('MASS'
	# ,'KRLS','bigKRLS','randomForest'
	))

mod = glm.nb(
	vic ~ graph_dens + numConf + n_actors,
	data=netStats)

mod_pois = glm(
	vic ~ graph_dens + numConf + n_actors,
	data=netStats,
	family='poisson')

summary(mod)$'coefficients'
summary(mod_pois)$'coefficients'

vars = names(netStats)[1:11]
res = lapply(vars, function(v){
  form=formula(paste0('vic~numConf+n_actors+', v))
  mod = glm.nb(form, data=netStats)
  out = summary(mod)$'coefficients'
  return(out)
  })
names(res) = vars
res

# nonlinMod=krls(
# 	X=data.matrix(
# 		netStats[,c('graph_dens', 'numConf', 'n_actors')]
# 		),
# 	y=netStats[,'vic']
# 	)

# nonlinMod2=bigKRLS(
# 	X=data.matrix(
# 		netStats[,c('graph_dens', 'numConf', 'n_actors')]
# 		),
# 	y=netStats[,'vic']
# 	)

# library(randomForest)
# rfMod = randomForest(x=data.matrix(
# 	netStats[,c('graph_dens', 'numConf', 'n_actors')]
# 	), y=netStats[,'vic'], type='regression')
# partialPlot(rfMod, pred.data=netStats, x.var='graph_dens')

# library(BART)
# set.seed(6886)
# nd = 200
# burn = 50
# x = data.matrix(
# 	netStats[,c('graph_dens', 'numConf', 'n_actors')]
# 	)
# y = netStats[,'vic']
# post = wbart(x, y, nskip=burn, ndpost=nd)

# plot(post$sigma, type="l")
# abline(v=burn, lwd=2, col="red")

# library(BayesTree)

# pdb1 = pdbart(
# 	x,y,xind=c(1,2),
# 	levs=list(seq(-1,1,.2),seq(-1,1,.2)),pl=FALSE,
# 	keepevery=10,ntree=100,nskip=100,ndpost=200) #should run longer!
# plot(pdb1,ylim=c(-.6,.6))

# viz of results
raw = summary(mod)$'coefficients'[-1,]
coefData = raw %>%
		data.frame(.,stringsAsFactors=FALSE) %>%
		setNames(c('mean','sd','zstat','pval')) %>%
		mutate(
			var=rownames(.),
			varName=c(
				'Graph Density',
				'Number of\nConflicts',
				'Number of\nActors'
				),
			model='ABM Simulation Model'
			) %>%
		getCIVecs(.) %>%
		getSigVec(.)

# org for plotting
coefData$varName = factor(
	coefData$varName,
	levels=varKey$clean
	)
########################################################

########################################################
# viz
ggCoef = ggplot(coefData, aes(x=varName, y=mean, color=sig)) +
	geom_hline(aes(yintercept=0), linetype=2, color = "black") +
	geom_point(size=4) +
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	ylab('') + xlab('') +
	facet_wrap(~model) +
	coord_flip() +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0),
		strip.text.x = element_text(size = 9, color='white'),
		strip.background = element_rect(fill = "#525252", color='#525252')
	)
ggsave(ggCoef,
	width=7, height=4,
	file=paste0(pathGraphics, 'abm_coefPlot.pdf'),
	device=cairo_pdf
	)
########################################################
