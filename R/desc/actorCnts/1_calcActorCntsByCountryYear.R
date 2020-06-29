###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg(c(
  'readr', 'abind'
  ))
###########################################################

###########################################################
acled = suppressMessages(read_csv(
  paste0(pathData, "acled-armedactors-battles.csv")))
###########################################################

###########################################################
# how many unique actors per country?
# first select a few key vars
acled <- acled %>%
  select(
    'event_type','year',
    'actor1', 'actor2',
    'country','latitude',
    'longitude','fatalities'
  )

###########################
# aggregate military and police forces into gov
acled$actor1[
  grepl('Military Forces of|Police Forces of',acled$actor1)
  ] = paste0(
  'Gov Forces of ',
  acled$country[
    grepl('Military Forces of|Police Forces of',acled$actor1)
    ] )

acled$actor2[
  grepl('Military Forces of|Police Forces of',acled$actor2)
  ] = paste0(
  'Gov Forces of ',
  acled$country[
    grepl('Military Forces of|Police Forces of',acled$actor2)
    ] )
###########################

###########################
# military forces from other countries

# # code to remove observations
# acled = acled[!grepl('Military Forces of',acled$actor1),]
# acled = acled[!grepl('Military Forces of',acled$actor2),]

# code to set as NA so that we keep
# the actors that were fighting against
# military from other countries
acled$actor1[grepl('Military Forces of', acled$actor1)] = NA
acled$actor2[grepl('Military Forces of', acled$actor2)] = NA
###########################

###########################
# peacekeepers/observers
pkpObsTags = c(
  'African Union', 'United Nations', 'Observer'
)

# cycle through tags and remove
for(tag in pkpObsTags){
  # # code to remove observations
  # acled = acled[!grepl(tag, acled$actor1),]
  # acled = acled[!grepl(tag, acled$actor2),]

  # code to set as NA so that we keep
  # the actors that were fighting against
  # pkprs and obs from other countries
  acled$actor1[grepl(tag, acled$actor1)] = NA
  acled$actor2[grepl(tag, acled$actor2)] = NA
}
###########################

###########################
# unidentified actors

# # code to remove observations
# acled = acled[!grepl('Unidentified', acled$actor1), ]
# acled = acled[!grepl('Unidentified', acled$actor2), ]

# code to set as NA so that we keep
# the actors that were fighting against
# unidentified actors
acled$actor1[grepl('Unidentified', acled$actor1)] = NA
acled$actor2[grepl('Unidentified', acled$actor2)] = NA
###########################

###########################
# remove some broad opposition labels
toRemove = c(
  'Opposition Rebels (Syria)',
  'Former Opposition Rebels (Syria)',
  'Opposition Parties'
)
acled = acled[which(!acled$actor1 %in% toRemove),]
acled = acled[which(!acled$actor2 %in% toRemove),]
###########################

###########################
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

actorVec = actors$actor2[actors$fatalities>=10]
###########################

###########################
# take the actors identified in actorVec
# and subset our acled dataset

# include any event that involed
# an actor at all from our parsed actor list
# acledParsed = acled[
#   acled$actor1 %in% actorVec |
#   acled$actor2 %in% actorVec,]

# only include events in which both
# actors come from our parsed actor list
acledParsed = acled[which(acled$actor1 %in% actorVec),]
acledParsed = acledParsed[
  which(acledParsed$actor2 %in% actorVec),]
###########################

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
actorCntsID$nCat = '1-4'
actorCntsID$nCat[actorCntsID$nActors>5] = '5-9'
actorCntsID$nCat[actorCntsID$nActors>10] = '>10'
actorCntsID$nCat = factor(
  actorCntsID$nCat,
  levels=rev(c('1-4','5-9','>10'))
)

#
save(actorCntsID, file=paste0(pathData, 'actorCntsID.rda'))
###########################################################
