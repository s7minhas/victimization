############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }

# load extra libs
loadPkg(c('doParallel', 'foreach', 'abind', 'reshape2'))
############################

###########################
# load data
load(
  paste0(
    pathData, 'acled_cleanedActorCols.rda')) # acled
############################

############################
# create actor-country level df

# get min and max active year from slice for every actor
# also get total number of fatalities for events in
# which they were involved
actorDates = abind(
	acled[,c('actor1', 'year', 'country', 'fatalities')],
	acled[,c('actor2', 'year', 'country', 'fatalities')],
	along=1 ) %>% data.frame(.,stringsAsFactors=FALSE) %>%
	group_by(actor2, country, .drop=FALSE) %>%
	summarize(
		startYear = min(num(year)),
		endYear = max(num(year)),
		fatalities = sum(num(fatalities), na.rm=TRUE)
	 )

# only include rows with greater than
# 10 fatalities
actorDates = actorDates[actorDates$fatalities>=10,]

# remove NAs
actorDates = actorDates[!is.na(actorDates$actor2),]
############################

############################
# cntries and years
cntries = sort(unique(actorDates$country))
yrs = sort(unique(acled$year))
############################

############################
# list of actors by country-year
# actors will show up for every year
# between their min and max year event in R
actorsCT = lapply(cntries, function(cntry){
	aDateSlice = actorDates[which(actorDates$country==cntry),]
	actorsT = lapply( yrs, function(t){
	  actors = NULL
	  for( ii in 1:nrow(aDateSlice)){
	     if( t %in% aDateSlice$startYear[ii]:aDateSlice$endYear[ii] ) {
	      actors = append(actors, aDateSlice$actor2[[ii]]) } }
	  return(actors)
	}) ; names(actorsT) = yrs
	return(actorsT) })
names(actorsCT) = cntries
############################

############################
# create list of adj mats
yListAll = lapply(names(actorsCT), function(cntry){
  # subset acled by relevant country
	nData = acled[acled$country==cntry,]
  # create binary dv for adjmat
	nData$dv = 1 ; yVar = 'dv'
  # get relev actors from actorsCT,
  # which incorps fatality threshold
  actorsT = actorsCT[[cntry]]
  # organize as list of adjMats
  # by every year
	yList = lapply(yrs, function(ii){
		if(
      # if no actors dont create adjmat
			is.null(actorsT[[char(ii)]]) |
      # if less than three actors
      # dont create adjmat
			length(actorsT[[char(ii)]])<3
      ){ return(NULL) }
    # pull out actors for year ii
    actorSlice = actorsT[[char(ii)]]
    # slice acled data by year
    # and actors
		slice = nData[ which(
			nData$year==ii &
			nData$actor1 %in% actorSlice &
			nData$actor2 %in% actorSlice
			), c('actor1', 'actor2', yVar) ]
		if(nrow(slice)==0){return(NULL)}
    # fill in adMat and set dims as
    # equal to n actors for the
    # relev cntry-year
		adjMat = matrix(0,
			nrow=length(actorSlice), ncol=length(actorSlice),
			dimnames=list(actorSlice,actorSlice) )
		for(r in 1:nrow(slice)){
      adjMat[slice$actor1[r],slice$actor2[r]]=1  }
		# sum i-j and j-i entries (undirected)
		adjMat = adjMat + t(adjMat)
		return(adjMat)
	}) ; names(yList) = yrs
	return(yList)
}) ; names(yListAll) = names(actorsCT)
############################

############################
# keep only countries that have more than
# three actors
cnt = function(x){sqrt(length(x))}
actCnts = melt(lapply(yListAll, function(x){lapply(x, cnt)}))
actCntsSumm = actCnts %>% group_by(L1) %>% summarize(cnt=max(value))
actCntsSumm=actCntsSumm[order(actCntsSumm$cnt, decreasing=TRUE),]
toKeep = actCntsSumm$L1[actCntsSumm$cnt>0]
############################

############################
# save
save(
  actorDates, actorsCT, yListAll,
  actCnts, actCntsSumm, toKeep,
  file=paste0(pathData, 'actorAdjList.rda'))
############################
