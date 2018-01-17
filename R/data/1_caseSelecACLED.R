if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/intraConfNetDyn/R/setup.R') }

loadPkg('readr')
acled = read_csv('~/Dropbox/Research/conflictEvolution/data/ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv')

acledCiv = acled[which(acled$EVENT_TYPE=='Violence against civilians'),]

toKeep = c(
	# 'Remote violence', 'Headquarters or base established', 
	'Battle-No change of territory', 
	'Battle-Government regains territory', 
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

# descriptive stats across acled data
getSumm = function(x){c(min=min(x), max=max(x), mean=mean(x), sd=sd(x))}
t(apply(summStatsACLED[,-ncol(summStatsACLED)], 2, getSumm))
getSumm(acledCiv$FATALITIES)

# peer into horn of africa
toFocus=summStatsACLED[summStatsACLED$cntry %in% c('Sudan', 'South Sudan', 'Kenya', 'Somalia', 'Ethiopia'), 'cntry']
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

# plotSumm(summStatsACLED_toFocus, 'cntConf', 'Number of conflicts', 'cntConf_horn.pdf')
# plotSumm(summStatsACLED_toFocus, 'cntActors', 'Number of actors', 'cntActors_horn.pdf')
# plotSumm(summStatsACLED_toFocus, 'cntDyads', 'Number of dyads', 'cntDyads_horn.pdf')
# plotSumm(summStatsACLED_toFocus, 'cntCivEvents', 'Number of Violent Civilian Events', 'cntCivEvents_horn.pdf')
# plotSumm(summStatsACLED_toFocus, 'cntCivFatals', 'Number of Civilian Fatalities', 'cntCivFatals_horn.pdf')

# determine most conflictual countries
cntriesACLED=summStatsACLED[order(summStatsACLED$cntDyads, decreasing = TRUE),][1:20,'cntry']
aData = data.frame(acled[which(acled$COUNTRY %in% cntriesACLED), ])
save(aData, cntriesACLED, file=paste0(pathData, 'cntriesACLED_byDyads.rda'))

cntriesACLED=summStatsACLED[order(summStatsACLED$cntActors, decreasing = TRUE),][1:20,'cntry']
aData = data.frame(acled[which(acled$COUNTRY %in% cntriesACLED), ])
save(aData, cntriesACLED, file=paste0(pathData, 'cntriesACLED_byActors.rda'))

cntriesACLED=summStatsACLED[order(summStatsACLED$cntConf, decreasing = TRUE),][1:20,'cntry']
aData = data.frame(acled[which(acled$COUNTRY %in% cntriesACLED), ])
save(aData, cntriesACLED, file=paste0(pathData, 'cntriesACLED_byConf.rda'))