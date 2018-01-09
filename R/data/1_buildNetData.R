if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/intraConfNetDyn/R/setup.R') }

#
# load(paste0(pathData, 'ged171.Rdata'))
# ged=data.frame(ged171, stringsAsFactors = FALSE) ; rm(ged171)

# ged = ged[,c(
# 	'id','year','type_of_vi','conflict_n',
# 	'dyad_new_i','dyad_name',
# 	'side_a_new','side_a',
# 	'side_b_new','side_b',
# 	'country',
# 	'best', 'deaths_a', 'deaths_b', 'deaths_civ'
# 	)]

# #
# gedCiv = ged[ged$type_of_vi==2,]

# # 
# cntries = unique(gedCiv$country)
# summStats = data.frame(do.call('rbind', lapply(cntries, function(c){
# 	slice = gedCiv[gedCiv$country==c,]

# 	# number of conflicts
# 	cntConf = nrow(slice)

# 	# length of conflict
# 	yrCnt = max(slice$year) - min(slice$year)

# 	# number of actors
# 	cntActors = length(unique(c(slice$side_a, slice$side_b)))

# 	# number of dyads
# 	cntDyads = length(unique(slice$dyad_name))

# 	# org
# 	c(cntConf=cntConf, yrCnt=yrCnt, cntActors=cntActors, cntDyads=cntDyads)
# 	})))
# summStats$cntry = cntries


# gedOne = ged[ged$type_of_vi==3,]
# gedOne$country=char(gedOne$country)
# gedOne = gedOne[which(gedOne$country %in% cntries),]
# civCnt = gedOne %>% group_by(country) %>% summarize(civDeaths = sum(best, na.rm=TRUE))

# summStats$civDeaths = civCnt$civDeaths[match(summStats$cntry, civCnt$country)]

# # save(summStats, file=paste0(pathDrop, 'summStats.rda'))

# summStats[order(summStats$cntDyads, decreasing=TRUE),]

# tmp=gedCiv[gedCiv$country=='Pakistan',]
# tmp$dyad_name <- char(tmp$dyad_name)
# cbind(table(tmp$dyad_name))

loadPkg('readr')
acled = read_csv('~/Dropbox/Research/conflictEvolution/data/ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv')

acledCiv = acled[which(acled$EVENT_TYPE=='Violence against civilians'),]

toKeep = c('Remote violence', 'Battle-No change of territory', 
	'Battle-Government regains territory', 'Headquarters or base established', 
	'Battle-Non-state actor overtakes territory')

acled = acled[which(acled$EVENT_TYPE %in% toKeep),]
acled$dyad_name = paste0(acled$ACTOR1, '_', acled$ACTOR2)

# 
cntries = unique(acled$COUNTRY)
summStatsACLED = data.frame(do.call('rbind', lapply(cntries, function(c){
	slice = acled[acled$COUNTRY==c,]

	# number of conflicts
	cntConf = nrow(slice)

	# length of conflict
	yrCnt = max(slice$YEAR) - min(slice$YEAR)

	# number of actors
	cntActors = length(unique(c(slice$ACTOR1, slice$ACTOR2)))

	# number of dyads
	cntDyads = length(unique(slice$dyad_name))

	# org
	c(cntConf=cntConf, yrCnt=yrCnt, cntActors=cntActors, cntDyads=cntDyads)
	})))
summStatsACLED$cntry = cntries

# getSumm = function(x){c(min=min(x), max=max(x), mean=mean(x), sd=sd(x))}
# t(apply(summStatsACLED[,-ncol(summStatsACLED)], 2, getSumm))

# getSumm(acledCiv$FATALITIES)

# acledNigeria = acled[acled$COUNTRY=='Nigeria' & acled$YEAR>2000 & acled$YEAR <=2015,]
# gedNigeria = gedCiv[gedCiv$country=='Nigeria' & gedCiv$year>2000 & gedCiv$year <= 2015,]

# summary(acledNigeria$YEAR)
# summary(gedNigeria$year)

# dim(acledNigeria)
# dim(gedNigeria)



# loadPkg('tidyr')
# ggAcled = gather(summStatsACLED, key='var', value='value', -cntry)
# ggplot(ggAcled, aes(x=value)) + geom_density() + facet_wrap(~var, scales='free')


# ##
# library(ggmap)


# tmp = get_map('Nigeria a')


toFocus = summStatsACLED$cntry[summStatsACLED$cntActors>100]
years = 2000:2016

summStatsACLED_toFocus = data.frame(do.call('rbind', lapply(toFocus, function(c){
	do.call('rbind', lapply(years, function(t){
		slice = acled[acled$COUNTRY==c & acled$YEAR==t,]
		cntConf = nrow(slice)
		cntActors = length(unique(c(slice$ACTOR1, slice$ACTOR2)))
		cntDyads = length(unique(slice$dyad_name))
		sliceCiv = acledCiv[acledCiv$COUNTRY==c & acledCiv$YEAR==t,]
		cntCivEvents = nrow(sliceCiv)
		cntCivFatals = ifelse(nrow(sliceCiv)>0, sum(sliceCiv$FATALITIES), 0)
		c(cntry=c, year=t, 
			cntConf=cntConf, cntActors=cntActors, cntDyads=cntDyads, 
			cntCivEvents=cntCivEvents, cntCivFatals=cntCivFatals)
	}) )
})), stringsAsFactors=FALSE)

for(v in setdiff(names(summStatsACLED_toFocus), 'cntry')){ summStatsACLED_toFocus[,v]=num(summStatsACLED_toFocus[,v])}

plotSumm = function(data, yVar, yLab, fName){
	data$ggY = data[,yVar]
	g=ggplot(data, aes(x=year, y=ggY, group=1)) +
		geom_line() +
		geom_point() +
		ylab(yLab) + 
		scale_x_continuous('', breaks=seq(2000,2016,2)) +
		facet_wrap(~cntry, scales='free_y', ncol=3) +
		theme_bw() +
		theme(
			axis.text.x=element_text(angle=45, hjust=1, size=6),
			axis.ticks=element_blank(),
			panel.border=element_blank()
			)
	ggsave(g, file=paste0(pathDrop, fName), width=9, height=5)	
}

plotSumm(summStatsACLED_toFocus, 'cntConf', 'Number of conflicts', 'cntConf.pdf')
plotSumm(summStatsACLED_toFocus, 'cntActors', 'Number of actors', 'cntActors.pdf')
plotSumm(summStatsACLED_toFocus, 'cntDyads', 'Number of dyads', 'cntDyads.pdf')
plotSumm(summStatsACLED_toFocus, 'cntCivEvents', 'Number of Violent Civilian Events', 'cntCivEvents.pdf')
plotSumm(summStatsACLED_toFocus, 'cntCivFatals', 'Number of Civilian Fatalities', 'cntCivFatals.pdf')
