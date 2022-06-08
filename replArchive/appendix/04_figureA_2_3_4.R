########################################################
source(paste0(here::here(), '/setup.R'))
########################################################

########################################################
load(paste0(pathResults, 'baseMods.rda'))
load(paste0(pathResults, 'cnt1Mods.rda'))
load(paste0(pathResults, 'cnt2Mods.rda'))
########################################################

########################################################
coefMissFE = list(
	base = feModBaseCoef,
 	cnt1 = feModCnt1Coef, cnt2 = feModCnt2Coef )

coefMissRE = list(
	base = reModBaseCoef,
 	cnt1 = reModCnt1Coef, cnt2 = reModCnt2Coef )

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
# re mods with cntrls
cntMissRE = ggDataMissRE[ggDataMissRE$model!=mLabs[1],]
coefViz(cntMissRE, 'appendix/figure_A3.png')

# fe mods with cntrls
cntImpFE = ggDataImpFE[ggDataImpFE$model!=mLabs[1],]
cntMissFE = ggDataMissFE[ggDataMissFE$model!=mLabs[1],]
coefViz(cntImpFE, 'appendix/figure_A2.png')
coefViz(cntMissFE, 'appendix/figure_A4.png')
########################################################
