# clean actor labels for actor1 and actor2 cols
# aggregate military/police of source country into gov
# set actor1/actor2 as NA for:
	# military/police from other countries
	# peacekeepers from United Nations and African Union
	# Observers
	# Unidentified

############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
############################

############################
# load processed acled data
load(paste0(pathData, 'acled_wMelt_assocActors.rda') )

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
# set peacekeepers/observers and
# unidentified actors as NA
tagsToRemove = c(
  'African Union', 'United Nations', 'Observer',
	'Unidentified' )

for(tag in tagsToRemove){
  acled$actor1[grepl(tag, acled$actor1)] = NA
  acled$actor2[grepl(tag, acled$actor2)] = NA }
###########################

###########################
# save
save(acled, file=paste0(pathData, 'acled_cleanedActorCols.rda'))
############################
