#################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }
load(paste0(pathData, 'cntriesACLED_byAll.rda'))
#################

#################
# clean actor names and remove special characters
ptrn = "[^[:alnum:][:blank:]?&/\\-]"
aData$a1 = gsub(ptrn, "",trim(aData$ACTOR1))
aData$a2 = gsub(ptrn, "",trim(aData$ACTOR2))
aData$aa1 = gsub(ptrn, "",trim(aData$ALLY_ACTOR_1))
aData$aa2 = gsub(ptrn, "",trim(aData$ALLY_ACTOR_2))

# remove unidentified groups
ids = c('a1','a2','aa1','aa2')
for(id in ids[1:2]){
	aData = aData[which(!grepl('Unidentified', aData[,id])),] }

# remove groups that are igos
groupType = read.csv(
	paste0(pathData, 'stupidActorAcled_Max.csv'),
	stringsAsFactors=FALSE)

# clean up some errors
groupType$grouping[
	grepl('United Nations', groupType$x)] = 'international organization'

# remove events that involve intl actors
toRemove = trim( groupType$x[
	groupType$grouping=='international organization'] )
for(remove in toRemove){
	for(id in ids[1:2]){
		aData = aData[which(!grepl(remove, aData[,id])),] } }
#################

#################
# get dates actors were active
# flip over dataset to get actor dates
orig = aData ; revOrig = orig
revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(aData$YEAR), max(aData$YEAR), by=1)
loadPkg('doBy')
actorDates = doBy::summaryBy(
	YEAR ~ a1 + COUNTRY, data=tmp, FUN=c(min, max))
# length of years active
actorDates$yrsActive = with(actorDates, YEAR.max - YEAR.min)

actorDates = actorDates[actorDates$yrsActive >= 0,] # keep any actor

# list of actors by country-year
actorsCT = lapply(unique(actorDates$COUNTRY), function(cntry){
	aDateSlice = actorDates[which(actorDates$COUNTRY==cntry),]
	actorsT = lapply( yrs, function(t){
	  actors = NULL
	  for( ii in 1:nrow(aDateSlice)){
	     if( t %in% aDateSlice$YEAR.min[ii]:aDateSlice$YEAR.max[ii] ) { 
	      actors = append(actors, aDateSlice$a1[[ii]]) } }
	  return(actors)
	}) ; names(actorsT) = yrs
	return(actorsT) }) ; names(actorsCT) = unique(actorDates$COUNTRY)
#################

#################
# create list of adj mats
yListAll = lapply(names(actorsCT), function(cntry){
	nData = aData[aData$COUNTRY==cntry,]
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
			nData$YEAR==ii & 
			nData$a1 %in% actorSlice &
			nData$a2 %in% actorSlice
			), c('a1', 'a2', yVar) ]
		if(nrow(slice)==0){return(NULL)}
		adjMat = matrix(0, 
			nrow=length(actorSlice), ncol=length(actorSlice),
			dimnames=list(actorSlice,actorSlice) )
		for(r in 1:nrow(slice)){ adjMat[slice$a1[r],slice$a2[r]]=1  }
		# sum i-j and j-i entries
		adjMat = adjMat + t(adjMat)
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

		# mats are undirected by construction
		# for any country-t we just look for the 
		# presence of an event
		grph = graph_from_adjacency_matrix(mat, 
			mode='undirected', weighted=NULL )
		sgrph = network::network(mat, 
			matrix.type="adjacency", directed=FALSE)

		totDegree = igraph::degree(grph)
		btwn = igraph::betweenness(grph)
		totClose = igraph::closeness(grph)
		eigenCent = igraph::evcent(grph)$vector # eigenvector centrality
		graph_trans = stat(sna::gtrans, sgrph)
		graph_dens = sna::gden(sgrph, mode='graph')
		out = data.frame(
			totDegree, btwn, 
			totClose, eigenCent, 
			graph_trans, graph_dens,
			year=t )
		out$country = cntry ; out$actor = rownames(mat)
		rownames(out) = NULL ; return(out) })
	cntryDF = do.call('rbind', cntryStats)
	return(cntryDF)	
}
names(netStats) = names(yListAll)

save(netStats, 
	file=paste0(pathData, 'netStats_acled.rda'))
#################