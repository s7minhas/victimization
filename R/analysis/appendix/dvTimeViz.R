########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))
########################################################

########################################################
# viz over time

# calc yearly averages
yrAgg = dataBase %>%
  group_by(year) %>%
  summarize(
    herf = mean(herf, na.rm=TRUE)
  )

# plot
ggplot(yrAgg, aes(x=year, y=herf, group=1)) +
  geom_line()
########################################################

########################################################
# viz over time by countries
########################################################
