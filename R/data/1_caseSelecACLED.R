############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# load extra libs
loadPkg(c('readr', 'abind', 'doParallel', 'foreach'))
############################

############################
# load data from acled
acled = suppressMessages( read_csv(
	paste0(
		pathData,
		"acled_raw_1997-01-01-2020-06-03.csv")))
############################

############################
# subset by event types
acledCiv = acled[
	which(
		acled$event_type=='Violence against civilians'),]

acled = acled[which(acled$event_type=='Battles'),]
############################

############################
# get some desc stats by country
cntries = unique(acled$country)
cores = 20
cl = makeCluster(cores)
registerDoParallel(cl)
summStatsACLED = foreach(
	c = cntries,
	.packages=c('abind', 'dplyr')
) %dopar% {

# pick cntry
slice = acled[acled$country==c,]

# number of conflicts
cntConf = nrow(slice)

# length of conflict
yrCnt = max(slice$year) - min(slice$year)

# get min and max active year from slice for every actor
# also get total number of fatalities for events in
# which they were involved
actors = abind(
	slice[,c('actor1', 'year', 'fatalities')],
	slice[,c('actor2', 'year', 'fatalities')],
	along=1 ) %>% data.frame(.,stringsAsFactors=FALSE) %>%
	group_by(actor2) %>%
	summarize(
		startYear = min(num(year)),
		endYear = max(num(year)),
		fatalities = sum(num(fatalities), na.rm=TRUE)
	 )
names(actors)[1] = 'dirty'

# subset to actors that were involved in events
# which produced at least 10 fatalities
actors = actors[actors$fatalities>=10,]

# combine military/police forces of country
govRefPattern = paste0(
	c('Military Forces of ', 'Police Forces of '),
	c ) %>% paste(., collapse='|')
actors$clean = actors$dirty
actors$clean[
	grepl(govRefPattern, actors$dirty)
	] = paste0('Gov Forces of ', c)

# get rid of military forces from other countries
actors$clean[grepl('Military Forces of',actors$clean)] = NA

# get rid of peacekeepers/observers
actors$clean[grepl('United Nations', actors$dirty)] = NA
actors$clean[grepl('African Union', actors$dirty)] = NA
actors$clean[grepl('Observer', actors$dirty)] = NA

# get rid of unidentified actors
actors$clean[grepl('Unidentified', actors$dirty)] = NA

# drop actors not in clean
actors = actors[!is.na(actors$clean),]

# if no actors returned, exit
# num NAs corresponds to length of out stats
if(nrow(actors)==0){return(rep(NA, 7))}

# get counts of actors
yrs = sort(unique(slice$year))
actorRanges = lapply(1:nrow(actors), function(ii){
	actors$startYear[ii]:actors$endYear[ii]})
aCntStats = lapply(yrs, function(yr){
	lapply(actorRanges, function(tRange){ yr %in% tRange }) %>%
		unlist() %>% sum() }) %>% unlist()

# number of actors
cntActorsMedian = median(aCntStats, na.rm=TRUE)
cntActorsMin = min(aCntStats, na.rm=TRUE)
cntActorsMean = mean(aCntStats, na.rm=TRUE)

# number of years with mult actors
yrsWithFiveActors =  sum(aCntStats>=5)
yrsWithTenActors =  sum(aCntStats>=10)

# org
c(
	cntConf=cntConf,
	yrCnt=yrCnt+1,
	cntActorsMedian=cntActorsMedian,
	cntActorsMin=cntActorsMin,
	cntActorsMean=cntActorsMean,
	yrsWithFiveActors=yrsWithFiveActors,
	yrsWithTenActors=yrsWithTenActors
	)
} ; stopCluster(cl)

# org
summStatsACLED = do.call('rbind', summStatsACLED) %>%
	data.frame(., stringsAsFactors=FALSE)
summStatsACLED$cntry = cntries
summStatsACLED$continent = countrycode(
	summStatsACLED$cntry, 'country.name', 'continent')
summStatsACLED$continent[summStatsACLED$cntry=='Kosovo'] = 'Europe'
summStatsACLED$continent[summStatsACLED$cntry=='eSwatini'] = 'Africa'

# get rid of countries that had NAs .. these are countries
# that had zero actors after processing
summStatsACLED = summStatsACLED[
	!is.na(summStatsACLED$cntActorsMedian),]

# reorg
summStatsACLED = summStatsACLED[order(
	summStatsACLED$cntActorsMedian, decreasing=TRUE),]

# convert yr cnts to proportion
summStatsACLED$yrsWithFiveActors = with(summStatsACLED,
	yrsWithFiveActors/yrCnt)
summStatsACLED$yrsWithTenActors = with(summStatsACLED,
	yrsWithTenActors/yrCnt)

# subset to countries in africa
summStatsACLED = summStatsACLED[
	summStatsACLED$continent=='Africa',]

# get out country names
cntriesACLED_five=summStatsACLED$cntry[
	summStatsACLED$cntActorsMedian>=5,]
cntriesACLED_ten=summStatsACLED$cntry[
	summStatsACLED$cntActorsMedian>=10,]
############################

############################
save(
	aData,
	cntriesACLED_five, cntriesACLED_ten, 
	file=paste0(pathData, 'cntriesACLED_byAll.rda')
)
############################
