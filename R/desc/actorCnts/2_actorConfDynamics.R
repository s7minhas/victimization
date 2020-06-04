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
cor(
  actorCntsID[,c('nActors','civ_fatals','bat_fatals')],
  use='pairwise.complete.obs'
)

# check by cat
actorCntsID %>%
  group_by(nCat) %>%
  summarize(
    civ_fatals=mean(civ_fatals),
    bat_fatals=mean(bat_fatals)
  )
###########################################################
