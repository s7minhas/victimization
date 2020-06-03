# GET THAT FACTS!
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

loadPkg(c('readr', 'ggmap', 'abind', 'countrycode'))

smTheme = theme(
  axis.ticks=element_blank(),
  panel.border=element_blank()
)
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

# todos
## make some charts to explore the "average" that we calcd
## make a map
## are multiactor conflicts more fatal
## are multiactor conflicts worse for those poor ole civies
## are multiactor conflicts longer
###########################################################

###########################################################
# use actorCntsID (summary of actors by country-year)
# to make a map

# now make a map that is dumb w/o further revisions
world <- map_data("world")

## check for disagreements between the two datasets
# use countrycode pkg to stdz names across
# acled and map region variable
world$cname = countrycode(
  world$region, 'country.name', 'country.name'
)
world = world[!is.na(world$region),]
cntriesToDrop = c(
  'ANTARCTICA', 'GREENLAND'
)
world = world[!world$cname %in% cntriesToDrop,]

# do the same for acled
actorCntsID$cname = countrycode(
  actorCntsID$country, 'country.name', 'country.name'
)

#
yrs = sort(unique(actorCntsID$year))
worldBig = lapply(yrs, function(t){
  slice = actorCntsID[actorCntsID$year==t,]
  world$year = t
  world$nActors = slice$nActors[match(world$cname, slice$cname)]
  return(world)
}) %>% do.call('rbind', .)

## theme
worldSlice = worldBig[worldBig$year %in% seq(1999,2019,4),]
worldGroups = ggplot() +
  geom_polygon(
    data = worldSlice,
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = nActors
  ) ) +
  coord_fixed(1.3) +
  scale_fill_gradient2(
    low='#f7fbff',
    mid='#6baed6',
    high='#08306b',
    na.value='grey70'
    ) +
  labs(
    fill="# Active \n Armed Groups"
  ) +
  facet_wrap(~year) +
  theme_void() +
  theme(
    legend.position='top',
    legend.key.width=unit(1.5, 'cm')
  )
ggsave(
  worldGroups,
  file=paste0(pathGraphics,'actorCntMap.pdf'),
  height=8, width=12
)

# create animated version

###########################################################

###########################################################
actorCntsID = actorCntsID %>%
  group_by(year) %>%
  mutate(
    nActorYrAvg = mean(nActors, na.rm=TRUE)
  )

ggplot(
  actorCntsID,
  aes(x=nActors) ) +
  geom_histogram() +
  geom_vline(
    aes(xintercept=nActorYrAvg),
    color='red',
    linetype='dashed'
  ) +
  facet_wrap(~year) +
  theme(
    panel.border=element_blank(),
    axis.ticks=element_blank()
  )
###########################################################

###########################################################
# boxplot summary of actor dist over time
ggplot(
  actorCntsID,
  aes(x=factor(year), y=nActors)
  ) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(alpha=.3) +
  geom_hline(
    aes(yintercept=5),
    color='red',
    linetype='dashed'
  ) +
  labs(
    x='', y='# Active Armed Groups'
  ) +
  theme(
    axis.text.x=element_text(angle=45),
    panel.border=element_blank(),
    axis.ticks=element_blank()
  )
###########################################################

###########################################################
# create categorical summary of actors over time
actorCntsID$grFive = 1*(actorCntsID$nActors>5)
actorCntsID$nCat = '1-4'
actorCntsID$nCat[actorCntsID$nActors>5] = '5-9'
actorCntsID$nCat[actorCntsID$nActors>10] = '>10'
actorCntsID$nCat = factor(
  actorCntsID$nCat,
  levels=rev(c('1-4','5-9','>10'))
)

ggplot(actorCntsID,
  aes(
    x=factor(year),
    fill=factor(nCat)
  )) +
  geom_bar() +
  scale_fill_brewer(palette='Dark2') +
  labs(
    x='',
    y='# Active Armed Groups',
    fill=''
  ) +
  smTheme +
  theme(
    axis.text.x=element_text(angle=45)
  )
###########################################################
