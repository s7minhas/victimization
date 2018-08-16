####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }
####

############################
# reorg to df
load(paste0(pathData, 'netStatsGED.rda'))
netDF = do.call('rbind', netStatsGED)
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

# here is where we could sort by # of actors (nactors)

############################
# merge graph level measures
graphVars = c('graph_recip','graph_trans','graph_dens')
data = simpleMerge(data, netDF, graphVars, 'id', 'id', lagVars=FALSE)

# think about which net actor level measures to add in...
#.....
rm(netDF)
############################

############################
# fix country name
data$country[data$country=='Serbia (Yugoslavia)'] = 'Serbia'

# stdz country names using panel]
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
load(paste0(pathData, 'ged171.Rdata'))
ged=data.frame(ged171, stringsAsFactors = FALSE) ; rm(ged171)
ged = ged[,c(
	'year','type_of_vi','country',
	'deaths_civ'
	)]

# subset to relev type
gedCiv = ged[ged$type_of_vi %in% c(1:2),]

#names
gedCiv$country[gedCiv$country=='Serbia (Yugoslavia)'] = 'Serbia'
gedCiv$cname = cname(gedCiv$country)
gedCiv$cnameYear = with(gedCiv, paste0(cname, '_', YEAR))
data$gedCivCount = gedCiv$deaths_civ[match(data$id, gedCiv$cnameYear)]
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
####
############################

############################
# save
save(data, file=paste0(pathData, 'GEDdata.rda'))
############################