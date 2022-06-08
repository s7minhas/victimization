########################################################
source(paste0(here::here(), '/setup.R'))
########################################################
tmp=ls()
########################################################
load(paste0(pathResults, 'baseMods.rda'))
load(paste0(pathResults, 'cnt1Mods.rda'))
load(paste0(pathResults, 'cnt2Mods.rda'))
########################################################
cbind(setdiff(ls(), tmp))
########################################################
coefMissFE = list(
	base = feModBaseCoef,
 	cnt1 = feModCnt1Coef, cnt2 = feModCnt2Coef )

coefMissRE = list(
	base = reModBaseCoef,
 	cnt1 = reModCnt1Coef, cnt2 = reModBaseCoef )

coefImpFE = list(
	base = feModBaseCoef,
 	cnt1 = feModCnt1Coef_imp, cnt2 = feModCnt2Coef_imp )

coefImpRE = list(
	base = reModBaseCoef,
 	cnt1 = reModCnt1Coef_imp, cnt2 = reModCnt2Coef_imp )
########################################################

########################################################
mLabs = c(
	'Base ACLED Model',
	'Base + Controls\n(1997-2018)', 'Base + Controls\n(1997-2015)' )

varKey = data.frame(
	dirty=rownames(coefMissFE$'cnt2'), stringsAsFactors = FALSE )
varKey$clean = c(
	'Network\nCompetition', 'Number of\nConflicts', 'Number of\nActors',
	'Polity', 'Log(Population)', 'Log(GDP per Capita)',
	'Excluded\nPopulation', 'Presence of\nPeacekeepers',
	'Rebel(s) Stronger\nthan Govt.',
	'Rebel(s) Supported\nby Foreign Govt.',
	'Govt. Supported\nby Foreign Govt.' )

ggDataMissFE = coefProcess(coefMissFE)
ggDataMissRE = coefProcess(coefMissRE)
ggDataImpFE = coefProcess(coefImpFE)
ggDataImpRE = coefProcess(coefImpRE)
########################################################

########################################################
# comparison of base model: fe vs re
baseFE = ggDataMissFE[ggDataMissFE$model=='Base ACLED Model',]
baseRE = ggDataMissRE[ggDataMissRE$model=='Base ACLED Model',]
baseFE$model = 'Base ACLED Model\n(Fixed Country Effects)'
baseRE$model = 'Base ACLED Model\n(Random Country Effects)'
ggDataBaseMod = rbind(baseFE, baseRE)
coefViz(ggDataBaseMod, 'figure6.png')

# re mods with cntrls
cntImpRE = ggDataImpRE[ggDataImpRE$model!=mLabs[1],]
coefViz(cntImpRE, 'figure7.png')
########################################################
