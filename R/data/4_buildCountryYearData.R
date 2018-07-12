####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

####

############################
# reorg to df
load(paste0(pathData, 'netStats.rda'))
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
graphVars = c('graph_recip','graph_trans','graph_dens')
data = simpleMerge(data, netDF, graphVars, 'id', 'id')

# think about which actor level measures to add in...

rm(netDF)
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
acled = readr::read_csv(paste0(pathData, 'ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv'))
acledCiv = acled[which(acled$EVENT_TYPE=='Violence against civilians'),]
####

####
# ucdp 


####
# other country year variables...cassy help.
### regime involved in interstate conflicts
### peacekeeper data, note: only for subsaharan africa 
### major power interventions ...
### external support to rebels or country ... ucdp has this
### # of foreign troops intervening each year
### conflict Severity is operationalized as the natural log of 
	## the number of total battlefield deaths in the conflict in 
	## a given yea
############################

############################
# save
save(data, file=paste0(pathData, 'data.rda'))
############################