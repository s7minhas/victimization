########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
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

coefProcess = function(coefList, labs=mLabs, vKey=varKey){
	out = lapply(1:length(coefList), function(ii){
		x = coefList[[ii]] ; lab = labs[ii]
		colnames(x)[1:3] = c('mean', 'sd', 'tstat')
		if( !('var' %in% names(x)) ){
			x = data.frame(x[,1:3], stringsAsFactors=FALSE)
			x$var = rownames(x) ; rownames(x) = NULL }
		x = x %>% getCIVecs(.) %>%
			getSigVec(.) %>%
			mutate(model=lab)
		return(x) })

	# combine
	out = do.call('rbind', out)

	# clean variable labels
	out$model = factor(out$model, levels=mLabs)
	out$varName = vKey$clean[match(out$var, vKey$dirty)]
	out = out[!is.na(out$varName),]
	out$varName = factor(out$varName, levels=rev(vKey$clean))
	return(out) }

ggDataMissFE = coefProcess(coefMissFE)
ggDataMissRE = coefProcess(coefMissRE)
ggDataImpFE = coefProcess(coefImpFE)
ggDataImpRE = coefProcess(coefImpRE)
########################################################

########################################################
coefViz = function(coefData, fName, path=pathGraphics){
	ggCoef = ggplot(coefData, aes(x=varName, y=mean, color=sig)) +
		geom_hline(aes(yintercept=0), linetype=2, color = "black") +
		geom_point(size=4) +
		geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
		geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +
		scale_colour_manual(values = coefp_colors, guide=FALSE) +
		ylab('') + xlab('') + facet_wrap(~model) +
		coord_flip() + theme_light(base_family="Source Sans Pro") +
		theme(
			legend.position='top', legend.title=element_blank(),
			panel.border=element_blank(), axis.ticks=element_blank(),
			axis.text.y=element_text(hjust=0),
			strip.text.x = element_text(size = 9, color='white'),
			strip.background = element_rect(
				fill = "#525252", color='#525252'))
	ggsave(ggCoef, width=8, height=6,
		file=paste0(path, fName), device=cairo_pdf) }
########################################################

########################################################
# comparison of base model: fe vs re
baseFE = ggDataMissFE[ggDataMissFE$model=='Base ACLED Model',]
baseRE = ggDataMissRE[ggDataMissRE$model=='Base ACLED Model',]
baseFE$model = 'Base ACLED Model\n(Fixed Country Effects)'
baseRE$model = 'Base ACLED Model\n(Random Country Effects)'
ggDataBaseMod = rbind(baseFE, baseRE)
coefViz(ggDataBaseMod, 'coefPlotBase_FE_RE.pdf')

# re mods with cntrls
cntImpRE = ggDataImpRE[ggDataImpRE$model!=mLabs[1],]
cntMissRE = ggDataMissRE[ggDataMissRE$model!=mLabs[1],]
coefViz(cntImpRE, 'coefPlotCntImpRE.pdf')
coefViz(cntMissRE, 'coefPlotCntMissRE.pdf')

# fe mods with cntrls
cntImpFE = ggDataImpFE[ggDataImpFE$model!=mLabs[1],]
cntMissFE = ggDataMissFE[ggDataMissFE$model!=mLabs[1],]
coefViz(cntImpFE, 'coefPlotCntImpFE.pdf')
coefViz(cntMissFE, 'coefPlotCntMissFE.pdf')
########################################################
