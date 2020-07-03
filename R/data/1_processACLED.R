# store assoc_actors in
# acled dataset in long
# instead of semicolon
# concatenated wide format

############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }

# load extra libs
loadPkg(c('doParallel', 'foreach'))
############################

############################
# load data from acled
acled = suppressMessages( read_csv(
	paste0(
		pathData,
		"acled_1997-01-01-2020-07-02.csv")))

# position of actor variables
toRem = -which(
	names(acled) %in%
	c(paste0('actor',1:2),paste0('assoc_actor_',1:2)))
############################

############################
# subset to battles
acled = acled[which(acled$event_type=='Battles'),]
############################

############################
# function to turn semicol concat vec into vec
actorVec = function(main, assoc){
	side = paste(c(main, assoc), collapse='; ')
	sideVec = trim(unlist(strsplit(side, '; ')))
	sideVec[sideVec=='NA'] = NA
	return(sideVec) }

# function to combine vecs into new df
actorRows = function(main1, assoc1, main2, assoc2){
	side1Vec = actorVec(main1, assoc1)
	side2Vec = actorVec(main2, assoc2)
	newRows = expand.grid(
		side1Vec, side2Vec,
		stringsAsFactors=FALSE)
	return(newRows) }

# divide acled dataset into
# part with and w/o assoc actors
acledMult = acled[ which(
	!is.na(acled$assoc_actor_1) |
	!is.na(acled$assoc_actor_2) ),]
acledSing = acled[ which(
	is.na(acled$assoc_actor_1) &
	is.na(acled$assoc_actor_2) ),]

# convert acled slice with assoc actors
# into long format
cores = detectCores() - 4
cl = makeCluster(cores)
registerDoParallel(cl)
tmp <- foreach(
	row = 1:nrow(acledMult)
) %dopar% {
	slice = acledMult[row,]
	newSlice = with(slice,
		actorRows(
			actor1, assoc_actor_1,
			actor2, assoc_actor_2 ))
	newSlice = na.omit(newSlice)
	names(newSlice) = c('actor1', 'actor2')
	newSlice = cbind(newSlice, slice[,toRem])
	return(newSlice)
} %>% do.call('rbind', .)
stopCluster(cl)

# recombine
acled = rbind(
	acledMult,
	acledSing[,names(acledMult)]
)
############################

############################
# save
save(acled, file=paste0(pathData, 'acled.rda'))
############################
