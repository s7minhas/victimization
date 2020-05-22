if(Sys.info()['user'] %in% c('Owner')){
	source('C:/Users/Owner/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

# abm path
abmPath = paste0(pathDrop, 'abm/')
abmPath = paste0(pathGit, "python/")
# load in file
# abmData = read.csv(paste0(abmPath, 'terrBigRun.csv'), header=FALSE)
# add = read.csv(paste0(abmPath, 'terrBigRun2.csv'), header=FALSE)
# abmData = rbind(abmData,add)
abmData = read.csv(paste0(abmPath, 'abmNewSelectProb.csv'))

# clean stuff up
abmData$V12 = char(abmData$V12)
tmp = strsplit(abmData$V12, '],', fixed=TRUE)
cleaner = function(x){
	out = trim(
		gsub('}','',
			gsub(']','',
				gsub(':','',
					gsub('[0-99]', '',
						gsub('[','',
							gsub('{', '', x,
								fixed=TRUE),fixed=TRUE)))),fixed=TRUE))
	return(out) }
out = lapply(tmp, cleaner)

# create turn level db
turnsPerGame = unlist(lapply(out, length))
numGames = length(out)
df = lapply(1:numGames, function(i){
	mat = cbind(i, 1:turnsPerGame[i])
	return(mat) }) %>% do.call('rbind', .)
df = data.frame(df, stringsAsFactors = FALSE)
names(df) = c('gameID', 'turnID')

# overall count of vic per turn
if(!file.exists(paste0(abmPath, 'df_withVicCount.rda'))){
	df$vicCount = 0
	for(i in 1:nrow(df)){
		turnResults = out[[ df$gameID[i] ]][ df$turnID[i] ]
		if(nchar(turnResults)!=0){
			concatResult=trim(gsub(', ', '', turnResults, fixed=TRUE))
			vicCount = nchar(concatResult) } else { vicCount=0 }
		df$vicCount[i] = vicCount }
	save(df, file=paste0(abmPath, 'df_withVicCount.rda'))
} else { load(paste0(abmPath, 'df_withVicCount.rda')) }

# net stats
abmData$V13 = char(abmData$V13)
tmp = strsplit(abmData$V13, '],', fixed=TRUE)
out = lapply(tmp, cleaner)
dyadConf = do.call('rbind', lapply(1:length(out), function(gameIter){
	x = out[[gameIter]]
	toExtractFrom = strsplit(x, '),',fixed=TRUE)
	gameSumm = lapply(1:length(toExtractFrom), function(turnIter){
		z = toExtractFrom[[turnIter]]
		z = gsub(')','',gsub('(', '', z, fixed=TRUE),fixed=TRUE)
		zMat = do.call('rbind', lapply(strsplit(z, ', ', z, fixed=TRUE), function(u){t(cbind(u))}))
		zMat[,1] = trim(zMat[,1]) ; zMat[,2] = trim(zMat[,2])
		numConf = length(z)
		zMat = cbind(gameIter, turnIter, zMat, numConf)
		if(length(z)==0){ zMat = NULL }
		return(zMat) })
	return(do.call('rbind', gameSumm)) }) )

# clean up edge list
dyadConf = data.frame(dyadConf, stringsAsFactors = FALSE)
dyadConf$gameIter = num(dyadConf$gameIter)
dyadConf$turnIter = num(dyadConf$turnIter)
dyadConf$V3 = char(dyadConf$V3)
dyadConf$V4 = char(dyadConf$V4)
dyadConf$numConf = num(dyadConf$numConf)
rownames(dyadConf) = NULL

# get actor list for each game iter
abmData$V14 = char(abmData$V14)
tmp = strsplit(abmData$V14, '],', fixed=TRUE)
out = lapply(tmp, cleaner)
actorSet = lapply(out, function(x){
	lapply(x, function(turn){
		actors=sort(trim(unlist(strsplit(turn, '),'))))
		actors = gsub(" ,", "", actors)
		actors = gsub(" )", "", actors, fixed = T)
		actors = gsub("(", "", actors, fixed = T)
		actors = gsub(",", "", actors, fixed = T)
		n=length(actors)
		adjMat=matrix(0,nrow=n,ncol=n,dimnames=list(actors,actors))
		diag(adjMat) = NA
		return(adjMat)
	})	 })

# fill in adjmat
numGames = 1:length(actorSet)
numTurns = lapply(actorSet, function(x){1:length(x)})
for(i in 1:nrow(dyadConf)){
	game = dyadConf$gameIter[i]
	turn = dyadConf$turnIter[i] - 1
	sender = dyadConf$V3[i]
	receiver = dyadConf$V4[i]
	if(game %in% numGames){
		if(turn %in% numTurns[[game]]){
			actorSet[[game]][[turn]][sender,receiver] = 1
		}
	}
}

# calc graph stats
loadPkg(c('sna','igraph','network'))
stat = function(expr, object){
	x=try(expr(object),TRUE)
	if(class(x)=='try-error'){x=NA}
	return(x) }

# if(!file.exists(paste0(abmPath, 'abmNetStats.rda'))){
	netStats = lapply(1:length(actorSet), function(game){
		gameList = actorSet[[game]]
		out = lapply(1:length(gameList), function(turn){
			mat = gameList[[turn]]
			grph = graph_from_adjacency_matrix(mat,
				mode='directed', weighted=NULL )
			sgrph = network::network(mat, matrix.type="adjacency",directed=TRUE)
			graph_recip = stat(sna::grecip, sgrph)
			graph_trans = stat(sna::gtrans, sgrph)
			graph_dens = stat(sna::gden, sgrph)
			n_actors = nrow(mat)
			out = c(
				graph_recip=graph_recip,
				graph_trans=graph_trans,
				graph_dens=graph_dens,
				n_actors=n_actors,
				game=game, turn=turn
				)
		})
		res = do.call('rbind', out)
	})
	netStats = do.call('rbind', netStats)
	netStats[,"turn"] = netStats[,"turn"] + 1
	netStats = netStats[!is.nan(netStats[,"graph_dens"]),]
	# save(netStats, file=paste0(abmPath, 'abmNetStats.rda'))
# } else { load(paste0(abmPath, 'abmNetStats.rda')) }

# merge in hyperparams
netStats = data.frame(netStats)
abmData$game = 1:nrow(abmData)
hyperparams = paste0('V',1:11)
for(v in hyperparams){
	netStats$tmp = abmData[match(netStats$game,abmData$game),v]
	names(netStats)[ncol(netStats)] = v }

# merge in vic
netStats$vic = 0
netStats$id = with(netStats, paste(game, turn, sep='_'))
df$id = with(df, paste(gameID, turnID, sep='_'))
netStats$vic = df$vicCount[match(netStats$id,df$id)]

# merge in numConf
dyadConf$id = with(dyadConf, paste(gameIter, turnIter, sep='_'))
netStats$numConf = dyadConf$numConf[match(netStats$id, dyadConf$id)]
netStats$numConf[is.na(netStats$numConf)] = 0

#
abmPath = paste0(pathDrop, 'abm/')
# save(netStats, file=paste0(abmPath, 'abmResults.rda'))
########################################################

########################################################
# basic look at results
library(ggcorrplot)
corr = round(cor(netStats[,-c(5:6,ncol(netStats)-1)], use='pairwise.complete.obs'),3)
ggcorrplot(corr, colors=c('red','white','blue'))
########################################################

########################################################
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

# pdb1 = pdbart(x,y,xind=c(1,2),
#               levs=list(seq(-1,1,.2),seq(-1,1,.2)),pl=FALSE,
#               keepevery=10,ntree=100,nskip=100,ndpost=200) #should run longer!
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
