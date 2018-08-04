if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

load(paste0(pathData, 'ged171.Rdata'))
ged=data.frame(ged171, stringsAsFactors = FALSE) ; rm(ged171)

ged = ged[,c(
	'id','year','type_of_vi','conflict_n',
	'dyad_new_i','dyad_name',
	'side_a_new','side_a',
	'side_b_new','side_b',
	'country',
	'best', 'deaths_a', 'deaths_b', 'deaths_civ'
	)]

#
gedCiv = ged[ged$type_of_vi==2,]

# 
cntries = unique(gedCiv$country)
summStatsGED = data.frame(do.call('rbind', lapply(cntries, function(c){
	slice = gedCiv[gedCiv$country==c,]

	# number of conflicts
	cntConf = nrow(slice)

	# length of conflict
	yrCnt = max(slice$year) - min(slice$year)

	# number of actors
	cntActors = length(unique(c(slice$side_a, slice$side_b)))

	# number of dyads
	cntDyads = length(unique(slice$dyad_name))

	# org
	c(cntConf=cntConf, yrCnt=yrCnt, cntActors=cntActors, cntDyads=cntDyads)
	})))
summStatsGED$cntry = cntries

gedOne = ged[ged$type_of_vi==3,]
gedOne$country=char(gedOne$country)
gedOne = gedOne[which(gedOne$country %in% cntries),]
civCnt = gedOne %>% group_by(country) %>% summarize(civDeaths = sum(best, na.rm=TRUE))

summStatsGED$civDeaths = civCnt$civDeaths[match(summStatsGED$cntry, civCnt$country)]

# save(summStatsGED, file=paste0(pathDrop, 'summStatsGED.rda'))

summStatsGED[order(summStatsGED$cntDyads, decreasing=TRUE),]

tmp=gedCiv[gedCiv$country=='Pakistan',]
tmp$dyad_name <- char(tmp$dyad_name)
cbind(table(tmp$dyad_name))

cntriesUCDP=summStatsGED[order(summStatsGED$cntDyads, decreasing = TRUE),][,'cntry']
cData = gedCiv = data.frame(ged[which(ged$COUNTRY %in% cntriesUCDP), ])
save(cData, cntriesUCDP, file=paste0(pathData, 'cntriesACLED_byAll.rda'))