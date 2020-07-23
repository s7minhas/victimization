##########################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('dorffc')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('sbgcop', 'reshape2'))
##########################

##########################
load(paste0(pathData, 'modelDataCnt1.rda'))
sbgDataCnt1 = sbgData
load(paste0(pathData, 'modelDataCnt1.rda'))
sbgDataCnt2 = sbgData
##########################

##########################
impTrace = function(impMod, fName){
	impConv = impMod$C.psamp
	impData = melt(impConv)
	toDrop = c(
		'ccode', 'year', 'civVicCount',
		'graph_dens', 'graph_avgDeg',
		'nActors', 'nEvents', 'nConf' )
	impData$Var1 = char(impData$Var1)
	impData$Var2 = char(impData$Var2)
	impData = impData[which(!impData$Var1 %in% toDrop),]
	impData = impData[which(!impData$Var2 %in% toDrop),]

	impData$varPair = with(impData,
		paste(Var1, Var2, sep='_'))

	gg = ggplot(impData, aes(x=Var3, y=value)) +
		geom_line() +
		facet_wrap(~varPair, scales='free_y')
	ggsave(gg, file=paste0(pathGraphics, fName))
}

impTrace(sbgDataCnt1, 'cnt1ImpTrace.pdf')
impTrace(sbgDataCnt2, 'cnt2ImpTrace.pdf')
##########################
