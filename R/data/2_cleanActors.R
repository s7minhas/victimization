############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }

# load extra libs
loadPkg(c('doParallel', 'foreach', 'abind'))
############################

############################
# load processed acled data
load(paste0(pathData, "acled.rda"))

# store orig actor labs in sep cols
acled$orig1 = acled$actor1
acled$orig2 = acled$actor2
############################

###########################
# aggregate military and police forces into new
# actor label that is Gov forces of x
# set actor label for military forces from other
# countries to NA
# (yeah, there's probably a better way to do this)

# actor 1 rows to modify
ids = which(
  grepl(
    'Military Forces of|Police Forces of', acled$actor1) )
for(id in ids){
  if(grepl(acled$country[id], acled$actor1[id])){
    acled$actor1[id] = paste0(
      'Gov Forces of ', acled$country[id] )
  } else { acled$actor1[id] = NA } }

# actor 2 rows to modify
ids = which(
  grepl(
    'Military Forces of|Police Forces of', acled$actor2) )
for(id in ids){
  if(grepl(acled$country[id], acled$actor2[id])){
    acled$actor2[id] = paste0(
      'Gov Forces of ', acled$country[id] )
  } else { acled$actor2[id] = NA } }
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

tst = unique(acled[is.na(acled$actor2),c('actor2','orig2')])
nrow(tst)
tst$orig2 %>% cbind()
write.csv(tst$orig2, file='C:/Users/Owner/Desktop/tmp.csv')
system('open C:/Users/Owner/Desktop/tmp.csv')

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

############################
# id cols for actors
ids = c( paste0('actor', 1:2) )

# get actors by country
cntries = unique(acled$country)
# cores = detectCores()-4
# cl = makeCluster(cores)
# registerDoParallel(cl)
# actorData = foreach(
#   cntry = cntries,
#   .packages=c('abind','dplyr')
# ) %dopar% {

cntry = 'Somalia'

# pick cntry
slice = acled[acled$country==cntry,]

# reorg to actor level
actors = lapply(ids, function(id){
  out=slice[,c(id, 'year','fatalities')]
  names(out)[1] = 'dirty'
  return(out)
  }) %>% do.call('rbind', .) %>%
  data.frame(.,stringsAsFactors=FALSE)

# get rid of NA rows
actors = actors[!is.na(actors$dirty),]

# cleanup cols
actors$clean = trim(actors$dirty)
actors$year = num(actors$year)
actors$fatalities = num(actors$fatalities)

# subset to actors that were involved in events
# which produced at least 10 fatalities
actors = actors[actors$fatalities>=10,]

# combine military/police forces of country
govRefPattern = paste0(
	c('Military Forces of ', 'Police Forces of '),
	cntry ) %>% paste(., collapse='|')
actors$clean = actors$dirty
actors$clean[
	grepl(govRefPattern, actors$dirty)
	] = paste0('Gov Forces of ', cntry)

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

# }
############################
