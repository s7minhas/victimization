if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/intraConfNetDyn/R/setup.R') }
# load(paste0(pathData, 'cntriesACLED_byConf.rda'))
# load(paste0(pathData, 'cntriesACLED_byActors.rda'))
# load(paste0(pathData, 'cntriesACLED_byDyads.rda'))
load(paste0(pathData, 'cntriesACLED_byAll.rda'))

#################
# clean actor names
aData$a1 = trim(aData$ACTOR1) ; aData$a2 = trim(aData$ACTOR2)
aData$aa1 = trim(aData$ALLY_ACTOR_1) ; aData$aa2 = trim(aData$ALLY_ACTOR_2)

# remove unidentified groups
ids = c('a1','a2','aa1','aa2')
for(id in ids[1:2]){ aData = aData[which(!grepl('Unidentified', aData[,id])),] }
#################

#################
# get dates actors were active
# flip over dataset to get actor dates
orig = aData ; revOrig = orig
revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(aData$YEAR), max(aData$YEAR), by=1)
loadPkg('doBy') ; actorDates = doBy::summaryBy(YEAR ~ a1 + COUNTRY, data=tmp, FUN=c(min, max))
actorDates$yrsActive = actorDates$YEAR.max - actorDates$YEAR.min # length of years active

# save(actorDates, file=paste0(pathData, 'actorDates_all.rda'))
# write.csv(actorDates, 
# 	file=paste0(pathData, 'actorDates_toClean_all.csv'),
# 	row.names=FALSE
# 	)

actorDates = actorDates[actorDates$yrsActive > 2,] # only keep actors involved in 3 yrs of conflict
# actorDates = actorDates[actorDates$yrsActive > 4,] 

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
		return(adjMat)
	}) ; names(yList) = yrs
	return(yList)
}) ; names(yListAll) = names(actorsCT)
#################

#################
loadPkg('igraph')
lapply(names(yListAll, function()){
	lapply(yrs, function(t){

	})
})

#################


# save(actorDates, file=paste0(pathData, 'actorsByCountry_topByConf.rda'))
#################