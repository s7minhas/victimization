###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg(c( 'readr' ))
###########################################################

###########################################################
load(paste0(pathData, 'actorCntsID.rda'))
###########################################################

###########################################################
# are multiactor conflicts more violent
acledRaw = suppressMessages(read_csv(
  file=paste0(pathData, 'acled_raw_1997-01-01-2020-06-03.csv')
))

# create subsetted versions based on event types
acledRawBattles = acledRaw[
  acledRaw$event_type %in% c('Battles'),]
acledRawCivies = acledRaw[
  acledRaw$event_type %in% c('Violence against civilians'),]

# are they more fatal in general
acledSummCivies = acledRawCivies %>%
  group_by(
    country, year
  ) %>%
  summarize(
    civ_fatals = sum(fatalities, na.rm=TRUE)
  )

acledSummBattles = acledRawBattles %>%
  group_by(
    country, year
  ) %>%
  summarize(
    bat_fatals = sum(fatalities, na.rm=TRUE)
  )

# create id variables
acledSummCivies$id = with(acledSummCivies, paste0(country, year))
acledSummBattles$id = with(acledSummBattles, paste0(country, year))
actorCntsID$id = with(actorCntsID, paste0(country, year))

# merge
actorCntsID$civ_fatals = acledSummCivies$civ_fatals[
  match(actorCntsID$id, acledSummCivies$id)]
actorCntsID$bat_fatals = acledSummBattles$bat_fatals[
  match(actorCntsID$id, acledSummBattles$id)]

# check rel
rhoActorsFatals = cor(
  actorCntsID[,c('nActors','civ_fatals','bat_fatals')],
  use='pairwise.complete.obs'
)

# check by cat
catActorsFatals = actorCntsID %>%
  group_by(nCat) %>%
  summarize(
    civ_fatals=mean(civ_fatals),
    bat_fatals=mean(bat_fatals)
  )

aovCiv = aov(civ_fatals ~ nCat, data=actorCntsID)
aovBat = aov(bat_fatals ~ nCat, data=actorCntsID)
summary(aovCiv) ; summary(aovBat)
###########################################################

###########################################################
# reshape data
ggData = pivot_longer(
    data=actorCntsID,
    cols=c(civ_fatals, bat_fatals),
    names_to=c('fatals')
)

# rename
ggData$fatals[ggData$fatals=='bat_fatals'] = "Battle Fatalities"
ggData$fatals[ggData$fatals=='civ_fatals'] = "Civilian Fatalities"

# log
ggData$value = log(ggData$value + 1)

# calc means by nCat and fatals
ggData = ggData %>%
  group_by(nCat, fatals) %>%
  mutate(
    avg = mean(value, na.rm=TRUE),
    q25 = quantile(value, .25, na.rm=TRUE),
    q75 = quantile(value, .75, na.rm=TRUE),
    qLo = quantile(value, .025, na.rm=TRUE),
    qHi = quantile(value, .975, na.rm=TRUE)
  )

# viz
ggData$nCat = factor(ggData$nCat,
  levels=rev(levels(ggData$nCat)))
actorConfDyn = ggplot(ggData, aes(x=nCat)) +
  geom_linerange(aes(ymin=q25, ymax=q75), size=.75) +
  geom_linerange(aes(ymin=qLo, ymax=qHi), size=.5) +
  geom_point(aes(y=avg), size=2) +
  geom_jitter(aes(y=value), alpha=.15) +
  labs(
    x='Number of Armed Actors',
    y='Logged Fatality Counts'
  ) +
  facet_wrap(~fatals, nrow=2, scales='free_y') +
  smTheme
ggsave(actorConfDyn,
  file=paste0(pathGraphics, 'actorConfDyn.pdf'),
  width=10, height=6
)
#save
actConfDynFats = ggData
save(actConfDynFats, file=paste0(pathData, 'actConfDynFats.rda'))
###########################################################
