# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

loadPkg(c('MASS'
	# ,'KRLS','bigKRLS','randomForest'
	))
################################################

# load in data #################################
load(paste0(abmPath, 'netStats.rda'))
################################################

cor(netStats[,c("numConf", 'graph_avgDeg', 'graph_dens', 'n_actors')])

################################################
# run neg binom
# mod = glm.nb(
# 	vic ~ graph_avgDeg + numConf + n_actors +  graph_centrz,
# 	data=netStats)
#
# mod_pois = glm(
# 	vic ~ graph_avgDeg + numConf + n_actors +  graph_centrz,
# 	data=netStats,
# 	family='poisson')
# summary(mod)$'coefficients'
# summary(mod_pois)$'coefficients'

# run across relev net stat vars
vars = names(netStats)[1:11]
perfVars = vars[c(1,3,6)]
res = lapply(perfVars, function(v){
  form=formula(paste0('vic~numConf+n_actors+', v))
  mod = glm.nb(form, data=netStats)
  out = summary(mod)$'coefficients'
  return(out)
  })
names(res) = perfVars

# compare performance of metrics via cross val
loadPkg(c('doParallel', 'foreach'))
nFolds = 10
folds = letters[1:nFolds]
set.seed(6886)
netStats$fold = sample(folds, nrow(netStats), TRUE)
parDF = expand.grid(perfVars, folds, stringsAsFactors=FALSE)

#
cbind(names(netStats))
netStatsAgg = netStats %>%
	group_by(game) %>%
	summarize(
		vic = sum(vic, na.rm=TRUE),
		graph_dens = mean(graph_dens,na.rm=TRUE),
		graph_avgDeg = mean(graph_avgDeg, na.rm=TRUE),
		graph_localTrans = mean(graph_localTrans, na.rm=TRUE),
		numConf = sum(numConf, na.rm=TRUE),
		n_actors = max(n_actors, na.rm=TRUE)
	)

netStatsAggStdz = apply(netStatsAgg, 2, function(x){(x-mean(x, na.rm=TRUE))/sd(x,na.rm=TRUE)})
head(netStatsAggStdz)
vars = names(netStats)[1:11]
perfVars = vars[c(1,3,6)]
res = lapply(perfVars, function(v){
  form=formula(paste0('vic~numConf+n_actors+', v))
  mod = lm(form, data=netStatsAggStdz)
  out = summary(mod)$'coefficients'
  return(out)
  })
names(res) = perfVars
res

# run in parallel
cl = makeCluster(20)
registerDoParallel(cl)
perfRes = foreach(ii = 1:nrow(parDF), .packages=c('MASS')) %dopar% {
	# get instr from parDF
	v = parDF[ii,1] ; f = parDF[ii,2]

	# set up form
	form=formula(paste0('vic~numConf+n_actors+', v))

	# divide data
	train = netStats[netStats$fold!=f,]
	test = netStats[netStats$fold==f,]
	test = test[!is.na(test[,v]),]

	# run model on train
	mod = glm.nb(form, data=train)

	# eval
	preds = predict(mod, test, type='response')
	rmse = sqrt( mean( (preds-test$vic)^2 ) )

	#
	out = data.frame(
		var=v, fold=f, rmse=rmse, stringsAsFactors=FALSE)
	return(out) }
stopCluster(cl)

# org results
perfRes = do.call('rbind', perfRes)

# quick analysis
res
perfRes %>% group_by(var) %>% summarize(mean(rmse))
ggplot(perfRes, aes(x=var, y=rmse)) +
	geom_point()

summary(netStats$vic)
sd(netStats$vic)
mean(netStats$vic)

head(netStats)
dim(netStats)
cbind(names(netStats))

toDrop = -which(names(netStats) %in% c('game', 'turn', 'fold', 'id', 'V1', 'V10', 'V9','graph_meanDist', 'graph_dens'))

slice = na.omit(netStats[,toDrop])
y = slice[,'vic']
ftrs = slice[,-which(names(slice) %in% 'vic')]
ftrs = apply(ftrs, 2, function(x){ (x-mean(x))/sd(x) })

library(glmnet)
lassoMod = cv.glmnet(x=data.matrix(
	ftrs,
	), y=y, family='poisson', alpha=1)
coef(lassoMod)
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

library(randomForest)
rfMod = randomForest(x=data.matrix(
	netStats[,c('graph_dens', 'numConf', 'n_actors', 'graph_avgDeg')]
	), y=netStats[,'vic'], type='regression')

preds = predict(rfMod, type='response')
head(preds)
sqrt( mean( (preds - netStats[,'vic'])^2 ) )
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
#should run longer!
# 	keepevery=10,ntree=100,nskip=100,ndpost=200)
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
ggCoef = ggplot(
		coefData,
		aes(x=varName, y=mean, color=sig)) +
	geom_hline(
		aes(yintercept=0), linetype=2, color = "black") +
	geom_point(size=4) +
	geom_linerange(
		aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
	geom_linerange(
		aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +
	scale_colour_manual(
		values = coefp_colors, guide=FALSE) +
	ylab('') + xlab('') +
	facet_wrap(~model) +
	coord_flip() +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0),
		strip.text.x = element_text(
			size = 9, color='white'),
		strip.background = element_rect(
			fill = "#525252", color='#525252')
	)
ggsave(ggCoef,
	width=7, height=4,
	file=paste0(pathGraphics, 'abm_coefPlot.pdf'),
	device=cairo_pdf
	)
########################################################
