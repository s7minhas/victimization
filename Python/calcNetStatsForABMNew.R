if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){ source('~/Documents/victimization/R/setup.R') }

# abm path
abmPath = paste0(pathDrop, 'abm/')
abmPath = paste0(pathGit, "python/")
# load in file
abmData = read.csv(paste0(abmPath, 'ActorVarRun.csv'), header=FALSE)
#add = read.csv(paste0(abmPath, 'terrBigRun2.csv'), header=FALSE)
#abmData = rbind(abmData,add)
# V12: actors that have victimized
# V13: which actor is fighting whom, directed
# V14: actual actors in the game

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

# if you want counts of vic by actor
# then finish organizing this code
# getCount = function(x){
# 	res = table(trim(unlist(strsplit(x,','))))
# 	if(length(res)==0){return(0)}
# 	actors = names(res)

# }

# sapply(x, getCount)

# overall count of vic per turn
#if(!file.exists(paste0(abmPath, 'df_withVicCount.rda'))){
	df$vicCount = 0
	for(i in 1:nrow(df)){
		turnResults = out[[ df$gameID[i] ]][ df$turnID[i] ]
		if(nchar(turnResults)!=0){
			concatResult=trim(gsub(', ', '', turnResults, fixed=TRUE))
			vicCount = nchar(concatResult) } else { vicCount=0 }
		df$vicCount[i] = vicCount }
	save(df, file=paste0(abmPath, 'df_withVicCount.rda'))
} #else { load(paste0(abmPath, 'df_withVicCount.rda')) }

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
	save(netStats, file=paste0(abmPath, 'abmNetStats.rda'))
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
save(netStats, file=paste0(abmPath, 'abmResults.rda'))

# basic look at results
library(ggcorrplot)
corr = round(cor(netStats[,-c(5:6,ncol(netStats)-1)], use='pairwise.complete.obs'),3)
ggcorrplot(corr, colors=c('red','white','blue'))

# run quick pois
mod = glm(vic ~ numConf + graph_dens + n_actors, family='poisson', data=netStats)
summary(mod)

netStats$vicBin = netStats$vic > 0
mod = glm(vicBin ~ numConf + graph_dens + n_actors, family='binomial', data=netStats)
summary(mod)

# run neg binom
loadPkg('MASS')
mod = glm.nb(vic ~ numConf + graph_dens + n_actors, data=netStats)
summary(mod)