###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg(c('abind'))
###########################################################

###########################################################
# load data
load(
  paste0(
    pathData, 'acled_cleanedActorCols.rda')) # acled
###########################################################

###########################################################
# fatality threshold (at least )
# across all events how many fatalities was an actor
# involved in

# first create actor level df
actors = abind(
  acled[,c('actor1','fatalities')],
  acled[,c('actor2','fatalities')], along=1 ) %>%
  data.frame(., stringsAsFactors=FALSE)

# fix types
actors$fatalities = num(actors$fatalities)

# group by actor and sum up fatalities
actors = data.frame(actors, stringsAsFactors=FALSE) %>%
  group_by(actor2) %>%
  summarize(
    fatalities = sum(fatalities, na.rm=TRUE)
  )

actorVec = actors$actor2
###########################################################

###########################################################
# take the actors identified in actorVec
# and subset our acled dataset

# only include events in which both
# actors come from our parsed actor list
acledParsed = acled[which(acled$actor1 %in% actorVec),]
acledParsed = acledParsed[
  which(acledParsed$actor2 %in% actorVec),]
###########################################################

###########################################################
# get out desc stats at the conflict level using our
# parsed actor list

# create conflict id
acledParsed$cyear = with(
  acledParsed, paste(country, year, sep='_'))

# count up how many actors show up in a country-year
ids = sort(unique(acledParsed$cyear))
actorCntsID = lapply( ids, function(id){

  # create slice by the id
  slice = acledParsed[acledParsed$cyear==id,]

  # get vector of actors
  actors = unique(c(slice$actor1, slice$actor2))

  # return n
  n = length(actors)

  #
  out = c(
    'country'=slice$country[1],
    'year'=slice$year[1],
    'nActors'=n
  )
  return(out)
}) %>% do.call('rbind', .) %>%
  data.frame(.,stringsAsFactors=FALSE)

# fix up object types
actorCntsID$year = num(actorCntsID$year)
actorCntsID$nActors = num(actorCntsID$nActors)

# create categorical summary of actors over time
actorCntsID$grFive = 1*(actorCntsID$nActors>5)

#
save(actorCntsID, file=paste0(pathData, 'actorCntsID.rda'))
###########################################################
