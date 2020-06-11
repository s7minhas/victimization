############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
  source('~/documents/victimization/R/setup.R') }
############################

############################
# reorg to df
load(paste0(pathData, 'netStats_acled.rda'))
netDF = do.call('rbind', netStats)
rownames(netDF) = NULL

# convert to country year
netDF$id = with(netDF, paste0(country, '_', year))

#
data = netDF[,c('country','year','id')] %>%
	group_by(id) %>%
	summarize(
		nActors = n()
		) %>%
	data.frame()
data$country = char(unlist(lapply(strsplit(data$id,'_'), function(x){x[1]})))
data$year = num(unlist(lapply(strsplit(data$id,'_'), function(x){x[2]})))
############################

############################
# merge graph level measures
graphVars = c(
	'graph_trans','graph_dens',
	'graph_avgDeg', 'graph_meanDist',
	'graph_localTrans'
)
data = simpleMerge(data, netDF, graphVars, 'id', 'id', lagVars=FALSE)
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
# polity
load(paste0(pathData, 'polity/polity.rda'))
polVars = c('polity2', 'xconst', 'polcomp')
data = simpleMerge(data, polity, polVars, 'id', 'cnameYear')
rm(polity)
####

####
# worldbank
load(paste0(pathData, 'worldBank/worldBank.rda'))
wbVars = c('gdp','gdpCap','gdpGr','pop','gdpLog','gdpCapLog','popLog')
data = simpleMerge(data, worldBank, wbVars, 'id', 'cnameYear')
rm(worldBank)
####

####
# cow cinc
load(paste0(pathData, 'cow_cinc/cinc.rda'))
cincVars = c('milex','milper','cinc')
data = simpleMerge(data, cinc, cincVars, 'id', 'cnameYear')
rm(cinc)
####

####
# icrg
load(paste0(pathData, 'icrg/icrg.rda'))
icrgVars = c(
	'govtStab', 'socEconCon', 'invProf', 'intConf',
	'extConf', 'corr', 'milPol', 'relPol', 'lawOrd',
	'ethTens', 'demAcct', 'burQual')
data = simpleMerge(data, icrg, icrgVars, 'id', 'cnameYear')
rm(icrg)
####

####
# civ victimization
#### need to finish acled civ dv measure
acled = readr::read_csv(paste0(pathData, 'ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv'))
acledCiv = acled[which(acled$EVENT_TYPE=='Violence against civilians'),]
acledCiv = acledCiv[acledCiv$YEAR>=1993,c('YEAR','COUNTRY','FATALITIES')]
acledCiv$cname = cname(acledCiv$COUNTRY)
acledCiv$cnameYear = with(acledCiv, paste0(cname, '_', YEAR))
data$civVicCount = acledCiv$FATALITIES[match(data$id, acledCiv$cnameYear)]
####

####
# epr
load(paste0(pathData, 'epr/epr.rda'))
eprVars = c(
	'ethfrac', 'exclgrps', 'exclpop'
	)
data = simpleMerge(data, epr, eprVars, 'id', 'cnameYear')
rm(epr)
####

####
# add kathman cmps peacekeeper data at country-year level
load(paste0(pathData, 'kathman/kath.rda'))
names(kath)[6] = 'totalPeacekeepers'
kathVars = c(
	'troop','police','militaryobservers', 'totalPeacekeepers'
	)
data = simpleMerge(data, kath, kathVars, 'id', 'cnameYear')
rm(kath)

# convert missing to zero
for(v in kathVars){ data[is.na(data[,v]),v] = 0 }

# create binary indicator for any intervention
data$anyPeaceKeeper = 0
data$anyPeaceKeeper[data$totalPeacekeepers>0] = 1
####

####
# add nsa data at country-year level
load(paste0(pathData, 'nsa/nsa.rda'))
nsaVars = names(nsa)[3:ncol(nsa)]
data = simpleMerge(data, nsa, nsaVars, 'id', 'cnameYear')
rm(nsa)
############################

############################
# save
save(data, file=paste0(pathData, 'data_acled.rda'))
############################
