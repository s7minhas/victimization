########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('MASS', 'stringr','glmmTMB'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))
########################################################

########################################################
# fn to get sens of covar param to dropping cntries
getSensData = function(
	modEst, ivs, dat, title, varIndex=1 ){

	# set up spec
	dv = 'civVicCount'
	p = length(ivs)
	feF = formula(paste0(
	  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
	reF = formula(paste0(
	  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))

	# run mods varying cntries included
	cntries = sort(unique(dat$cname))
	gDensSens = lapply(cntries, function(cntry){
		slice = dat[dat$cname!=cntry,]
		if(modEst=='FE'){
			mod = glm.nb(feF, data=slice)
			coef = summary(mod)$'coefficients'[1:p,]
			gDensCoef = coef[varIndex,,drop=FALSE] }
		if(modEst=='RE'){
			mod = glmmTMB(reF, data=slice, family='nbinom2')
			coef = summary(mod)$'coefficients'$cond[1:(p+1),]
			gDensCoef = coef[varIndex+1,,drop=FALSE] }
		return(gDensCoef) })

	# cleanup
	gDensSens = do.call('rbind', gDensSens)
	colnames(gDensSens)[1:3] = c('mean', 'sd', 'tstat')
	gDensSens = data.frame(gDensSens, stringsAsFactors=FALSE, row.names=NULL)
	gDensSens$cntry = str_to_title(cntries)
	gDensSens = getSigVec(getCIVecs(gDensSens))
	gDensSens$title = title
	return(gDensSens) }
########################################################

########################################################
# run on various specs with raw data
titleLabs = c(
	'Base ACLED Model',
	'Base + Controls\n(1997-2018)')
bivs = c( 'herf', 'nConf', 'nActors' )
gDensSensBase = getSensData(
	modEst='FE', # AIC indicates FE is preferred to RE
	ivs = bivs, dat=dataBase,
	title=titleLabs[1] )
c1ivs = c( bivs,
  'polity2', 'popLog', 'gdpCapLog', 'exclpop' )
gDensSensCnt1 = getSensData(
	modEst='RE', # time-invariant controls
	ivs = c1ivs, dat=dataCnt1,
	title=titleLabs[2] )
## too restricted of a sample when we start dropping
## countries and years for model with second set
## of controls
########################################################

########################################################
# combine and cleanup for plotting
gDensSens = rbind(gDensSensBase, gDensSensCnt1)
gDensSens$cntry = factor(gDensSens$cntry,
	levels=rev(sort(unique(gDensSens$cntry))) )
gDensSens$title = factor(gDensSens$title,
	levels=titleLabs)

ggCoef = ggplot(gDensSens, aes(x=cntry, y=mean, color=sig)) +
	geom_hline(aes(yintercept=0), linetype=2, color = "black") +
	geom_point(size=2) +
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	ylab('Effect of Graph Density') +
	xlab('Dropped Country') + facet_wrap(~title) +
	coord_flip() + theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=1),
		strip.text.x = element_text(size = 9, color='white'),
		strip.background = element_rect(
			fill = "#525252", color='#525252'))
ggsave(ggCoef, width=8, height=6,
	file=paste0(pathGraphics, 'gDensSens.pdf'),
	device=cairo_pdf)
########################################################
