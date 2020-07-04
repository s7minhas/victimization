# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

loadPkg(c(
	'MASS', 'glmmTMB', 'foreach', 'doParallel'
	))
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
# for RE model
netStats$game = factor(netStats$game)

# choose vars to test
vars = names(netStats)[1:11]
perfVars = vars[c(1,3,6)]

# run in parallel
cores = length(perfVars)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	v = perfVars,
	.packages=c('glmmTMB')
) %dopar% {
form=formula(paste0('vic~numConf+n_actors+', v, '+ (1|game)'))
mod = glmmTMB(form, data=netStats, family='nbinom2')
return(mod)
}
stopCluster(cl)
names(res) = perfVars
lapply(res, function(x){summary(x)$'coefficients'})
################################################

################################################
# compare performance of metrics via cross val

# assign folds by game
set.seed(6886)
nFolds = 10 ; folds = letters[1:nFolds]
gameFold = data.frame(
	game=sort(num(unique(netStats$game))),
	stringsAsFactors=FALSE )
gameFold$folds = sample(folds, nrow(gameFold), replace=TRUE)
netStats$folds = gameFold$folds[match(netStats$game, gameFold$game)]

# create instructions for prlz
parDF = expand.grid(perfVars, folds, stringsAsFactors=FALSE)

# run in parallel
cores = detectCores() - 4
cl = makeCluster(cores)
registerDoParallel(cl)
perfRes = foreach(
	ii = 1:nrow(parDF),
	.packages=c('MASS', 'glmmTMB')
	) %dopar% {
	# get instr from parDF
	v = parDF[ii,1] ; f = parDF[ii,2]

	# set up form
	form=formula(paste0('vic~numConf+n_actors+', v, '+ (1|game)'))

	# divide data
	train = netStats[netStats$folds!=f,]
	test = netStats[netStats$folds==f,]
	test = test[!is.na(test[,v]),]

	# run model on train
	mod = glmmTMB(form, data=train, family='nbinom2')

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
########################################################

########################################################
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
