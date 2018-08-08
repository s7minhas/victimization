if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ source('~/ProjectsGit/victimization/R/setup.R') }
load(paste0(pathData, 'cntriesGED_byAll.rda'))

#################
# clean actor names
cData$a1 = trim(cData$side_a) ; cData$a2 = trim(cData$side_b)
# cData$aa1 = trim(cData$ALLY_ACTOR_1) ; cData$aa2 = trim(cData$ALLY_ACTOR_2)

# remove unidentified groups
ids = c('a1','a2')
for(id in ids[1:2]){ cData = cData[which(!grepl('Unidentified', cData[,id])),] }
#################

#################
# get dates actors were active
# flip over dataset to get actor dates
orig = cData ; revOrig = orig
revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(cData$year), max(cData$year), by=1)
loadPkg('doBy') ; actorDatesGED = doBy::summaryBy(year ~ a1 + country, data=tmp, FUN=c(min, max))
actorDatesGED$yrsActive = actorDatesGED$year.max - actorDatesGED$year.min # length of years active

#save(actorDatesGED, file=paste0(pathData, 'actorDatesGED_all.rda'))
#write.csv(actorDatesGED, 
# 	file=paste0(pathData, 'actorDatesGED_toClean_allGED.csv'),
# 	row.names=FALSE
# 	)

actorDatesGED = actorDatesGED[actorDatesGED$yrsActive >= 0,] # keep any actor
# actorDatesGED = actorDatesGED[actorDatesGED$yrsActive > 4,]  # only keep actors involved in 4 yrs of conflict

# list of actors by country-year
actorsCT = lapply(unique(actorDatesGED$country), function(cntry){
	aDateSlice = actorDatesGED[which(actorDatesGED$country==cntry),]
	actorsT = lapply( yrs, function(t){
	  actors = NULL
	  for( ii in 1:nrow(aDateSlice)){
	     if( t %in% aDateSlice$year.min[ii]:aDateSlice$year.max[ii] ) {  
	      actors = append(actors, aDateSlice$a1[[ii]]) } }
	  return(actors)
	}) ; names(actorsT) = yrs
	return(actorsT) }) ; names(actorsCT) = unique(actorDatesGED$country)
#################

#################
# create list of adj mats
yListAll = lapply(names(actorsCT), function(cntry){
	nData = cData[cData$country==cntry,]
	nData$dv = 1 ; yVar = 'dv'
	actorsT = actorsCT[[cntry]]
	yList = lapply(yrs, function(ii){ 		
		if(
			is.null(actorsT[[char(ii)]]) | 
			length(actorsT[[char(ii)]])<5){
			return(NULL)
		}
		actorSlice = actorsT[[char(ii)]]
		slice = nData[ which( 
			nData$year==ii & 
			nData$a1 %in% actorSlice &
			nData$a2 %in% actorSlice
			), c('a1', 'a2', yVar) ]
		if(nrow(slice)==0){return(NULL)}
		adjMat = matrix(0, 
			nrow=length(actorSlice), ncol=length(actorSlice),
			dimnames=list(actorSlice,actorSlice) )
		for(r in 1:nrow(slice)){ adjMat[slice$a1[r],slice$a2[r]]=1  }
		return(adjMat)
	}) ; names(yList) = yrs
	return(yList)
}) ; names(yListAll) = names(actorsCT)
#################

#################
loadPkg(c('igraph', 'sna', 'network', 'doParallel', 'foreach'))
stat = function(expr, object){
	x=try(expr(object),TRUE)
	if(class(x)=='try-error'){x=NA}
	return(x) }

cl = makeCluster(6)
registerDoParallel(cl)
netStats <- foreach(
	cntry = names(yListAll), 
	.packages=c('igraph','sna','network')
	) %dopar% {
	cntryStats = lapply(yrs, function(t){
		mat = yListAll[[cntry]][[char(t)]]
		if(is.null(mat)){return(NULL)}
		grph = graph_from_adjacency_matrix(mat, 
			mode='directed', weighted=NULL )
		sgrph = network::network(mat, matrix.type="adjacency",directed=TRUE)

		inDegree = igraph::degree(grph, mode='in')
		outDegree = igraph::degree(grph, mode='out')
		totDegree = igraph::degree(grph, mode='total')
		btwn = igraph::betweenness(grph)
		power = stat(igraph::bonpow,grph) # bonaich 1987
		inClose = igraph::closeness(grph, mode='in') 
		outClose = igraph::closeness(grph, mode='out')
		totClose = igraph::closeness(grph, mode='total')
		eigenCent = igraph::evcent(grph)$vector # eigenvector centrality
		flow = stat(sna::flowbet,sgrph) # flow betweenness
		gcent = stat(sna::graphcent, sgrph) # graph centrality (harary)
		icent = stat(sna::infocent, sgrph) # info centrality
		if(sum(totDegree)<3){lcent = NA} else {
			lcent = stat(sna::loadcent,sgrph) } # load centrality
		prestige = stat(sna::prestige, sgrph) # prestige
		graph_recip = stat(sna::grecip, sgrph)
		graph_trans = stat(sna::gtrans, sgrph)
		graph_dens = stat(sna::gden, sgrph)
		out = data.frame(
			inDegree, outDegree, totDegree, btwn, power, 
			inClose, outClose, totClose, eigenCent, flow, gcent, 
			icent, lcent, prestige, 
			graph_recip, graph_trans, graph_dens,
			year=t )
		out$country = cntry ; out$actor = rownames(mat)
		rownames(out) = NULL ; return(out) })
	cntryDF = do.call('rbind', cntryStats)
	return(cntryDF)	
}
names(netStats) = names(yListAll)

netStatsGED = netStats

save(netStatsGED, file=paste0(pathData, 'netStatsGED.rda'))	



