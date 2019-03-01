if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
  source('~/Documents/victimization/R/setup.R') }

load(paste0(pathData, 'ged171.Rdata'))
ged=data.frame(ged171, stringsAsFactors = FALSE) ; rm(ged171)

ged = ged[,c(
	'id','year','date_start','date_end',
	'type_of_vi','conflict_n',
	'dyad_new_i','dyad_name',
	'side_a_new','side_a',
	'side_b_new','side_b',
	'country', 'longitude', 'latitude','where_prec',
	'best', 'deaths_a', 'deaths_b', 'deaths_civ'
	)]
#

# types other than one-sided violence bc of actor names
ged = ged[ged$type_of_vi %in% c(1:2),]

# 
cntries = unique(ged$country)
summStatsGED = data.frame(do.call('rbind', lapply(cntries, function(c){
	slice = ged[ged$country==c,]

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

#
gedCivD = ged
gedCivD$country=char(gedCivD$country)
gedCivD = gedCivD[which(gedCivD$country %in% cntries),]
civCnt = gedCivD %>%
	group_by(country) %>%
	summarize(civDeaths = sum(best, na.rm=TRUE))

summStatsGED$civDeaths = civCnt$civDeaths[
	match(
		summStatsGED$cntry, civCnt$country)]

#save summary data
save(summStatsGED, file=paste0(pathDrop, 'summStatsGED.rda'))

#
cntriesGED = summStatsGED[summStatsGED$cntActors >=5,]
cntriesGED = cntriesGED[order(
	cntriesGED$cntActors, decreasing = TRUE),][,'cntry']
cData = ged = data.frame(ged[which(ged$country %in% cntriesGED), ])
save(cData, cntriesGED, file=paste0(pathData, 'cntriesGED_byAll.rda'))