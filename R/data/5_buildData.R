############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }

#
loadPkg('readr')
############################

############################
# reorg to df
load(paste0(pathData, 'netStats.rda'))
netDF = do.call('rbind', netStats)
rownames(netDF) = NULL

# convert to country year
netDF$id = with(netDF, paste0(country, '_', year))

# subset to graph level measures
# years without events according
# to acled get dropped
graphVars = c(
	'nActors', 'nEvents',
	'herf',
	'graph_trans','graph_dens',
	'graph_avgDeg', 'graph_meanDist',
	'graph_localTrans' )
data = netDF[,c('id', graphVars)] %>% unique()

#
data$country = char(unlist(lapply(strsplit(data$id,'_'), function(x){x[1]})))
data$year = num(unlist(lapply(strsplit(data$id,'_'), function(x){x[2]})))

# check which countries get dropped
# 'Botswana''Equatorial Guinea''eSwatini''Gabon'
# setdiff(names(netStats), unique(data$country))
############################

############################
# stdz country names using panel
data$cname = cname(data$country)
data$id = with(data, paste0(cname, '_', year))
data$ccode=panel$ccode[match(data$cname,panel$cname)]
data$cyear=paste(data$ccode, data$year, sep='')
############################

############################
# merge in vars
####
# polity - 2018
load(paste0(pathData, 'polity/polity.rda'))
polVars = c('polity2', 'xconst', 'polcomp')
data = simpleMerge(data, polity, polVars, 'id', 'cnameYear')
rm(polity)
####

####
# worldbank - 2019
load(paste0(pathData, 'worldBank/worldBank.rda'))
wbVars = c('gdp','gdpCap','gdpGr','pop','gdpLog','gdpCapLog','popLog')
data = simpleMerge(data, worldBank, wbVars, 'id', 'cnameYear')
rm(worldBank)
####

####
# cow cinc - 2012
load(paste0(pathData, 'cow_cinc/cinc.rda'))
cincVars = c('milex','milper','cinc')
data = simpleMerge(data, cinc, cincVars, 'id', 'cnameYear')
rm(cinc)
####

####
# icrg - 2014
load(paste0(pathData, 'icrg/icrg.rda'))
icrgVars = c(
	'govtStab', 'socEconCon', 'invProf', 'intConf',
	'extConf', 'corr', 'milPol', 'relPol', 'lawOrd',
	'ethTens', 'demAcct', 'burQual')
data = simpleMerge(data, icrg, icrgVars, 'id', 'cnameYear')
rm(icrg)
####

####
# civ victimization -2020
acledCiv = suppressMessages( read_csv( paste0( pathData,
		"acled_1997-01-01-2020-07-02.csv"))) %>%
		filter(event_type=='Violence against civilians') %>%
		group_by(country, year) %>%
		summarize( fatalities = sum(fatalities, na.rm=TRUE) )
acledCiv$cname = cname(acledCiv$country)
acledCiv$cnameYear = with(acledCiv, paste0(cname, '_', year))
data$civVicCount = acledCiv$fatalities[match(data$id, acledCiv$cnameYear)]
data$civVicCount[is.na(data$civVicCount)] = 0
####

####
# number of conflicts -2020
# civ victimization -2020
acledConf = suppressMessages( read_csv( paste0( pathData,
		"acled_1997-01-01-2020-07-02.csv"))) %>%
		filter(event_type=='Battles') %>%
		group_by(country, year) %>%
		summarize( nConf = n() )
acledConf$cname = cname(acledConf$country)
acledConf$cnameYear = with(acledConf, paste0(cname, '_', year))
data$nConf = acledConf$nConf[match(data$id, acledConf$cnameYear)]
data$nConf[is.na(data$nConf)] = 0
####

####
# epr - 2017
load(paste0(pathData, 'growup/epr.rda'))
epr = data.frame(epr, stringsAsFactors=FALSE)
eprVars = c(
	'exclpop', 'egippop',
	'discrimpop', 'maxexclpop'
	)
data = simpleMerge(data, epr, eprVars, 'id', 'cnameYear')
rm(epr)
####

####
# add kathman cmps peacekeeper
# data at country-year level - originally 2012
# https://kathmanundata.weebly.com/ # updated to 2019
load(paste0(pathData, 'kathman/kath_v2.rda'))
names(kath)[6] = 'totalPeacekeepers'
kathVars = c(
	'troop','police','militaryobservers', 'totalPeacekeepers', 'total2'
	)
data = simpleMerge(data, kath, kathVars, 'id', 'cnameYear')
rm(kath)

# convert missing to zero
for(v in kathVars){ data[is.na(data[,v]),v] = 0 }

# create binary indicator for any intervention
# using total or total2 here returns the same result
data$anyPeaceKeeper = 0
data$anyPeaceKeeper[data$totalPeacekeepers>0] = 1

# no longer need to do this chunk because kathman
# updated the data to 2019
# # set everything after 2013 to NA
# # (data ends at 2012, so as lag
# # last set of info we have is 2013)
# for(v in c(kathVars, 'anyPeaceKeeper')){
# 		data[which(data$year>2013),v] = NA }
####

####
# add nsa data at country-year level - 2011
# we replicate forward nsa data from 2011 to 2014
# following the procedure from kathman and benson
# https://journals.sagepub.com/doi/10.1177/0022002718817104
# see footnote 12
load(paste0(pathData, 'nsa/nsa_v2.rda'))
nsaVars = names(nsa)[3:ncol(nsa)]
data = simpleMerge(data, nsa, nsaVars, 'id', 'cnameYear')
rm(nsa)
############################

############################
# save
save(data, file=paste0(pathData, 'data.rda'))
############################
