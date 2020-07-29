########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
########################################################

########################################################
load(paste0(pathResults, 'baseMods_end2019.rda'))
########################################################

########################################################
coefMissFE = list(
	base = feModBaseCoef )

coefMissRE = list(
	base = reModBaseCoef )
########################################################

########################################################
mLabs = c(
	'Base ACLED Model',
	'Base + Controls\n(1997-2018)', 'Base + Controls\n(1997-2012)' )

varKey = data.frame(
	dirty=rownames(coefMissFE$'cnt2'), stringsAsFactors = FALSE )
varKey$clean = c(
	'Graph Density', 'Number of\nConflicts', 'Number of\nActors',
	'Polity', 'Log(Population)', 'Log(GDP per Capita)',
	'Excluded\nPopulation', 'Presence of\nPeacekeepers',
	'Rebel(s) Stronger\nthan Govt.',
	'Rebel(s) Supported\nby Foreign Govt.',
	'Govt. Supported\nby Foreign Govt.' )

ggDataMissFE = coefProcess(coefMissFE)
ggDataMissRE = coefProcess(coefMissRE)
########################################################

########################################################
# comparison of base model: fe vs re
baseFE = ggDataMissFE[ggDataMissFE$model=='Base ACLED Model',]
baseRE = ggDataMissRE[ggDataMissRE$model=='Base ACLED Model',]
baseFE$model = 'Base ACLED Model\n(Fixed Country Effects)'
baseRE$model = 'Base ACLED Model\n(Random Country Effects)'
ggDataBaseMod = rbind(baseFE, baseRE)
coefViz(ggDataBaseMod, 'coefPlotBase_FE_RE_end2019.pdf')
########################################################
