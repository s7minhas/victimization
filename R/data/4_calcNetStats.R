############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }

# load extra libs
loadPkg(c('igraph', 'sna', 'network', 'doParallel', 'foreach'))
############################

###########################
# load data
  # actorDates, actorsCT, yListAll,
  # actCnts, actCntsSumm, toKeep,
load(paste0(pathData, 'actorAdjList.rda'))
############################

############################
# cntries and years
cntries = names(yListAll)
yrs = names(yListAll[[1]])
############################

############################
# calc net stats
stat = function(expr, object){
	x=try(expr(object),TRUE)
	if(class(x)=='try-error'){x=NA}
	return(x) }
localTrans = function(x){
  igraph::transitivity(x, type='average') }
cntry = names(yListAll)[1] ; t = yrs[1]
cl = makeCluster(20)
registerDoParallel(cl)
netStats <- foreach(
	cntry = names(yListAll),
	.packages=c('igraph','sna','network')
	) %dopar% {
	cntryStats = lapply(yrs, function(t){
		mat = yListAll[[cntry]][[char(t)]]
		if(is.null(mat)){return(NULL)}

		# mats are undirected by construction
		# for any country-t we just look for the
		# presence of an event
		grph = graph_from_adjacency_matrix(mat,
			mode='undirected', weighted=NULL )
		sgrph = network::network(mat,
			matrix.type="adjacency", directed=FALSE)

    nActors = nrow(mat)
		nEvents = sum(c(mat))/2
		totDegree = igraph::degree(grph)
		btwn = igraph::betweenness(grph)
		totClose = igraph::closeness(grph)
		eigenCent = igraph::evcent(grph)$vector # eigenvector centrality
		graph_trans = stat(igraph::transitivity, grph)
    graph_localTrans = stat(localTrans, grph)
		graph_dens = sna::gden(sgrph, mode='graph')
		graph_avgDeg = mean(stat(sna::degree, sgrph))
		graph_meanDist = mean_distance(grph)
		graph_recip = stat(sna::grecip, sgrph)
		graph_hier_krack = hierarchy(sgrph, measure='krackhardt')
		graph_conn_krack = stat(sna::connectedness, sgrph)
		graph_eff_krack = stat(sna::efficiency, sgrph)
		graph_centrz = centr_degree(grph)$centralization
		graph_lubness = stat(sna::lubness, sgrph)
		out = data.frame(
      nActors=nActors,
			nEvents=nEvents,
			totDegree, btwn,
			totClose, eigenCent,
			graph_trans, graph_dens,
			graph_localTrans, graph_avgDeg,
			graph_meanDist, graph_recip,
			graph_hier_krack, graph_conn_krack,
			graph_eff_krack, graph_centrz, graph_lubness,
			year=t )
		out$country = cntry ; out$actor = rownames(mat)
		rownames(out) = NULL ; return(out) })
	cntryDF = do.call('rbind', cntryStats)
	return(cntryDF)
}
names(netStats) = names(yListAll)
stopCluster(cl)

save(netStats,
	file=paste0(pathData, 'netStats.rda'))
############################
