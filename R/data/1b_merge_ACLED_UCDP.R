########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }

#
loadPkg(c('meltt','lubridate'))
########################################################

########################################################
# load data
load(paste0(pathData, 'cntriesGED_byAll.rda'))
ged = cData ; rm(cData)
load(paste0(pathData, 'cntriesACLED_byAll.rda'))
acled = aData ; rm(aData)

# remove utf-8 characters
cleanVars = c(
	paste0('ACTOR',1:2),
	'COUNTRY',
	paste0('ADMIN',1:3),
	'LOCATION',
	'SOURCE'
	)
for(v in cleanVars){
	acled[,v] = gsub("[^[:alnum:][:blank:]?&/\\-]", "", acled[,v])}

# remove utf-8 characters
cleanVars = c(
	'side_a',
	'side_b',
	'country'
	)
for(v in cleanVars){
	ged[,v] = gsub("[^[:alnum:][:blank:]?&/\\-]", "", ged[,v])}
########################################################

########################################################
# taxonomies
load(paste0(pathData, 'meltt_taxonomy.rda'))

# clean up taxonomy for our usecase
taxonomy = lapply(taxonomy, function(x){
	x = x[x$data.source %in% c('acled','ged'),] })

# actor taxonomy
aAvT = taxonomy$actor_tax[
	taxonomy$actor_tax$data.source=='acled',]
gAvT = taxonomy$actor_tax[
	taxonomy$actor_tax$data.source=='ged',]	

# construct acled actor taxonomy
acledActors = data.frame(
	data.source='acled',
	base.categories=unique(c(acled$ACTOR1,acled$ACTOR2)),
	# default given event type restriction will be violent group
	actor_level_1_txt='violent groups', 
	# all level 2 will be violent groups
	actor_level_2_txt='violent groups',
	stringsAsFactors = FALSE
	)

# any mention of military and police typically
# goes with govt
acledActors$actor_level_1_txt[
	grepl(
		'Military Forces of|Police Forces of', 
		acledActors$base.categories
		)] = 'government'

# fix up some mutiny and former groupings
acledActors$actor_level_1_txt[
	grepl(
		'Mutiny of|Former Military Forces of', 
		acledActors$base.categories
		)] = 'violent group'

### manual check
# unique(acledActors$base.categories[acledActors$actor_level_1_txt=='government'])

#### needs manual check
if(!file.exists(paste0(pathData, 'sillyActorGroupingCheck.csv'))){
write.csv(
	unique(
		acledActors$base.categories[
			acledActors$actor_level_1_txt=='violent groups']),
	file=paste0(pathData, 'sillyActorGroupingCheck.csv') ) }	

# load in cleaned file
cleaned = read.csv(
	paste0(pathData, 'sillyActorGroupingCheck.csv'), 
	stringsAsFactors=FALSE
	)

# construct ged actor taxonomy
ged$side_a = char(ged$side_a) ; ged$side_b = char(ged$side_b)
gedActors = data.frame(
	data.source='ged',
	base.categories=unique(c(ged$side_a,ged$side_b)),
	# default given event type restriction will be violent group
	actor_level_1_txt='violent groups', 
	# all level 2 will be violent groups
	actor_level_2_txt='violent groups',
	stringsAsFactors = FALSE
	)

# combine ged and acled actor taxonomies
taxonomy$actor_tax = rbind(acledActors,gedActors)

# any mention of government of
# goes with govt
gedActors$actor_level_1_txt[
	grepl(
		'Government of', 
		gedActors$base.categories
		)] = 'government'

# event taxonomy
aEvT = taxonomy$event_tax[
	taxonomy$event_tax$data.source=='acled',]
gEvT = taxonomy$event_tax[
	taxonomy$event_tax$data.source=='ged',]	
aEvT = aEvT[grepl('Battle',aEvT$base.categories),]
gEvT = gEvT[gEvT$base.categories %in% 1:2,]
taxonomy$event_tax = rbind(aEvT,gEvT) ; rm(aEvT,gEvT)

# prec taxonomy
### gonna trust them on this for now
########################################################

########################################################
# formatting data
acled = acled %>% 
	# Convert dates to class date
	# Rename variables to match taxonomy names. 
	mutate(
		date = dmy(EVENT_DATE),
		enddate = dmy(EVENT_DATE),
		event_tax = EVENT_TYPE,
		# need to change this to dyadic after we resolve
		# acled non-gov list
		actor_tax = ACTOR1, 
		prec_tax = GEO_PRECISION
		) %>% 
	rename_all(tolower) %>% 
	data.frame(.,stringsAsFactors = FALSE)

ged = ged %>%    
	# ditto
	mutate(
		date = as.Date(date_start),
		enddate = as.Date(date_end),
		event_tax = type_of_vi,
		actor_tax = side_a,
		prec_tax = where_prec
		) %>% 
	data.frame(.,stringsAsFactors = FALSE)
########################################################	

########################################################
# meltt data
combo = meltt(
	acled,ged,
	taxonomies = taxonomy,
	twindow = 1,spatwindow = 3
	)

# Summary of the integration reported in the paper
summary(combo)
########################################################