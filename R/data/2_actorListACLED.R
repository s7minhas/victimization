if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/intraConfNetDyn/R/setup.R') }
load(paste0(pathData, 'cntriesACLED_byConf.rda'))
# load(paste0(pathData, 'cntriesACLED_byActors.rda'))
# load(paste0(pathData, 'cntriesACLED_byDyads.rda'))

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
actorDates = actorDates[actorDates$yrsActive > 2,] # only keep actors involved in 3 yrs of conflict
# actorDates = actorDates[actorDates$yrsActive > 4,] 

# list of actors by country
head(actorDates)

save(actorDates, file=paste0(pathData, 'actorsByCountry_topByConf.rda'))
#################