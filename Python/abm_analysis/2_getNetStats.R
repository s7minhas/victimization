# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

# pkgs
loadPkg('stringr')

# helpers
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
################################################

# load in data #################################
load(paste0(abmPath, 'abmData.rda'))
################################################

# get dyad frame of battles ####################
# extract dyadic battle info from v3
abmData$V13 = char(abmData[,13])
tmp = strsplit(abmData$V13, '],', fixed=TRUE)
out = lapply(tmp, cleaner)
dyadConf = do.call('rbind', lapply(1:length(out), function(gameIter){
	x = out[[gameIter]]
	toExtractFrom = strsplit(x, '),',fixed=TRUE)
	gameSumm = lapply(1:length(toExtractFrom), function(turnIter){
		z = toExtractFrom[[turnIter]]
		z = gsub(')','',gsub('(', '', z, fixed=TRUE),fixed=TRUE)
		zMat = do.call('rbind',
			lapply(
				strsplit(z, ', ', z, fixed=TRUE), function(u){t(cbind(u))}))
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

# add id for gov actor
abmData$V14 = char(abmData[,14])
govActor = data.frame( gov = unlist( lapply(
	strsplit(abmData$V14, '],', fixed=TRUE),
	function(x){
		actors=cleaner(x[1])
		govActor=str_extract(
			trim(gsub(',|)','',actors)), '\\w\\Z')
		return(govActor) }) ), stringsAsFactors=FALSE )
govActor$govID = with(govActor,
	paste0(gov, '_', 1:nrow(govActor)))

# merge into dyadconf
dyadConf$senID = with(dyadConf,
	paste0(V3,'_',gameIter))
dyadConf$recID = with(dyadConf,
	paste0(V4,'_',gameIter))

# tag events by whether or not they involve the gov
dyadConf$senGov = 1*(dyadConf$senID %in% govActor$govID)
dyadConf$recGov = 1*(dyadConf$recID %in% govActor$govID)

#
save(dyadConf, file=paste0(abmPath, 'dyadConf.rda'))
################################################

# actor list to construct adj mats #############
# get actor list for each game iter
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
################################################

# calc net measures ############################
loadPkg(c('sna','igraph','network'))
stat = function(expr, object){
	x=suppressWarnings(try(expr(object),TRUE))
	if(class(x)=='try-error'){x=NA}
	return(x) }

netStats = lapply(1:length(actorSet), function(game){
	gameList = actorSet[[game]]
	out = lapply(1:length(gameList), function(turn){
		mat = gameList[[turn]]
		grph = graph_from_adjacency_matrix(mat,
			mode='directed', weighted=NULL )
		sgrph = network::network(
			mat, matrix.type="adjacency",directed=TRUE)
		graph_recip = stat(sna::grecip, sgrph)
		graph_trans = stat(sna::gtrans, sgrph)
		graph_dens = stat(sna::gden, sgrph)
		graph_hier_recip = stat(sna::hierarchy, sgrph)
		graph_hier_krack = hierarchy(sgrph, measure='krackhardt')
		graph_conn_krack = stat(sna::connectedness, sgrph)
		graph_eff_krack = stat(sna::efficiency, sgrph)
		graph_centrz = centr_degree(grph)$centralization
		graph_lubness = stat(sna::lubness, sgrph)
		n_actors = nrow(mat)
		out = c(
			graph_recip=graph_recip,
			graph_trans=graph_trans,
			graph_dens=graph_dens,
			graph_hier_recip = graph_hier_recip,
			graph_hier_krack = graph_hier_krack,
			graph_conn_krack = graph_conn_krack,
			graph_centrz = graph_centrz,
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
################################################
