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
loadPkg('readr')
############################

############################
# load data from acled
acled = suppressMessages( read_csv(
	paste0(
		pathData,
		"acled_raw_1997-01-01-2020-06-03.csv")))

acled = acled[which(acled$year<=2017),]
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
summStatsACLED = data.frame(
	do.call(
		'rbind', lapply(
			cntries, function(c){

# c= 'Somalia'

		slice = acled[acled$country==c,]

		# number of conflicts
		cntConf = nrow(slice)

		# length of conflict
		yrCnt = max(slice$year) - min(slice$year)

		# clean up actors
		actors = data.frame(
			dirty = unique( c(slice$actor1, slice$actor2) ),
			stringsAsFactors=FALSE )

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

		# only keep actors that are not NA in clean
		actors = actors[!is.na(actors$clean),]

		slice$actor1Clean = actors$clean[match(slice$actor1, actors$dirty)]
		slice$actor2Clean = actors$clean[match(slice$actor2, actors$dirty)]
		slice = slice[!is.na(slice$actor1Clean) & !is.na(slice$actor2Clean),]
		slice$dyadClean = with(slice, paste(actor1Clean, actor2Clean, sep='_'))

		aCntStats = lapply(unique(slice$year), function(t){
			slice2 = slice[slice$year==t,]
			sActors = unique(
				c( slice2$actor1Clean, slice2$actor2Clean ) )
			cnt = length(sActors)
			return(cnt)
			}) %>% unlist()

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
			yrCnt=yrCnt,
			cntActorsMedian=cntActorsMedian,
			cntActorsMin=cntActorsMin,
			cntActorsMean=cntActorsMean,
			yrsWithFiveActors=yrsWithFiveActors,
			yrsWithTenActors=yrsWithTenActors
			)
	})))
summStatsACLED$cntry = cntries

# reorg
summStatsACLED = summStatsACLED[order(
	summStatsACLED$cntActorsMedian, decreasing=TRUE),]

# write to csv to share info on
# sample from acled
write.csv( summStatsACLED,
		file=paste0(pathData, 'acledSample.csv') )

# get out country names
cntriesACLED=summStatsACLED[order(
	summStatsACLED$cntDyads, decreasing = TRUE),][,'cntry']
############################

############################
aData = acled = data.frame(acled[which(acled$COUNTRY %in% cntriesACLED), ])
save(aData, cntriesACLED, file=paste0(pathData, 'cntriesACLED_byAll.rda'))
############################
