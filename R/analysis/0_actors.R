# GET THA FACTS!
# How many countries that have 'battles'  from ALL years of ACLED data
# (maybe should subset on like actual civil wars?)
###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg(c('readr', 'ggmap', 'abind'))
###########################################################

###########################################################
acled = read_csv(
  paste0(pathData, "acled-armedactors-battles.csv"))
###########################################################

###########################################################
# how many unique actors per country?
# first select a few key vars
acled <- acled %>%
  select('event_type','year', 'actor1', 'actor2', 'country','latitude', 'longitude','fatalities')

###########################
# aggregate military and police forces into gov
acled$actor1[grepl('Military Forces of|Police Forces of',acled$actor1)] = paste0(
  'Gov Forces of ',
  acled$country[grepl('Military Forces of|Police Forces of',acled$actor1)]
)

acled$actor2[grepl('Military Forces of|Police Forces of',acled$actor2)] = paste0(
  'Gov Forces of ',
  acled$country[grepl('Military Forces of|Police Forces of',acled$actor2)]
)
###########################

###########################
# grepl to remove unidentified actors
acled = acled[!grepl('Unidentified', acled$actor1), ]
acled = acled[!grepl('Unidentified', acled$actor2), ]
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

# include any event that involed an actor at all from our parsed actor list
# acledParsed = acled[acled$actor1 %in% actorVec | acled$actor2 %in% actorVec,]

# only include events in which both actors come from our parsed actor list
acledParsed = acled[which(acled$actor1 %in% actorVec),]
acledParsed = acledParsed[which(acledParsed$actor2 %in% actorVec),]
###########################

###########################################################

###########################################################
# get out desc stats at the conflict level using our
# parsed actor list

# create conflict id
acledParsed$cyear = with(acledParsed, paste(country, year, sep='_'))

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
  out = c('country'=slice$country[1], 'year'=slice$year[1], 'nActors'=n)
  return(out)
}) %>% do.call('rbind', .) %>% data.frame(.,stringsAsFactors=FALSE)

# fix up object types
actorCntsID$year = num(actorCntsID$year)
actorCntsID$nActors = num(actorCntsID$nActors)

#
summary(actorCntsID$nActors)
###########################################################

###########################################################
# then count up unique actors per group across act1 and act2
groups = acled %>%
  group_by(country) %>%
  summarise(number_groups = n_distinct(actor1, actor2)) %>%
  arrange(desc(number_groups))

# now make a map that is dumb w/o further revisions
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3)
worldplot

## check for disagreements between the two datasets
diff <- setdiff(world$region, groups$country) #ugh lots to clean

#if I do not fix above^ this is wrong:
names(world)[names(world) == "region"] <- "country"
worldSubset <- inner_join(world, groups, by = "country")
head(worldSubset)

worldSubset %>%
  filter(number_groups>100)

## theme
basic <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldGroups <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = number_groups)) +
  scale_fill_distiller(palette ="BrBG", direction = -1) +
  ggtitle("Number of warring parties") +
  basic
###########################################################
