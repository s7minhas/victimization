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
varKey = data.frame(
	dirty=rownames(coefMissFE$'base'), stringsAsFactors = FALSE )
varKey$clean = c(
	'Network\nCompetition', 'Number of\nConflicts', 'Number of\nActors')

mLabs = 'Base ACLED Model'
ggDataMissFE = coefProcess(coefMissFE)
ggDataMissRE = coefProcess(coefMissRE)
########################################################

########################################################
# comparison of base model: fe vs re
baseFE = ggDataMissFE
baseRE = ggDataMissRE
baseFE$model = 'Base ACLED Model\n(Fixed Country Effects)'
baseRE$model = 'Base ACLED Model\n(Random Country Effects)'
ggDataBaseMod = rbind(baseFE, baseRE)
coefViz(ggDataBaseMod, 'coefPlotBase_FE_RE_end2019.pdf')
########################################################
