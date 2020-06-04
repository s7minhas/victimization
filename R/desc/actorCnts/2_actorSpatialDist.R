###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg(c(
  'ggmap',
  'countrycode',
  'gifski', 'av', 'gganimate'
  ))
###########################################################

###########################################################
load(paste0(pathData, 'actorCntsID.rda'))
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

# # create animated version
# worldGif = ggplot() +
#   geom_polygon(
#     data = worldBig,
#     mapping = aes(
#       x = long,
#       y = lat,
#       group = group,
#       fill = nActors
#   ) ) +
#   coord_fixed(1.3) +
#   scale_fill_gradient2(
#     low='#f7fbff',
#     mid='#6baed6',
#     high='#08306b',
#     na.value='grey70'
#     ) +
#   labs(
#     fill="# Active \n Armed Groups in {frame_time}"
#   ) +
#   facet_wrap(~year) +
#   theme_void() +
#   theme(
#     legend.position='top',
#     legend.key.width=unit(1.5, 'cm')
#   ) +
#   transition_time(year) +
#   ease_aes('linear')
#
# anim_save(
#   worldGif,
#   file=paste0(pathGraphics,'actorCntMapGif.gif'),
#   height=8, width=12)
###########################################################
