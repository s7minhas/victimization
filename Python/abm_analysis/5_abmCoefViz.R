# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }
################################################

################################################
# load and process coef data
cleanVars = c(
	'Number of\nConflicts','Number of\nActors', 'Network \nCompetition')
coefData = lapply(c('fe','re'), function(est){
	load(paste0(pathResults, 'abm_',est,'Coefs.rda'))
	if(est=='fe'){ coefs = coefs$herf_und[1:3,]}
	if(est=='re'){ coefs = coefs$herf_und[-1,]}
	coefs = data.frame(coefs, stringsAsFactors=FALSE)
	coefs$var = rownames(coefs) ; rownames(coefs) = NULL
	coefs$varName = cleanVars
	if(est=='fe'){
		coefs$title='ABM Simulation Model\n(Fixed Country Effects)'}
	if(est=='re'){
		coefs$title='ABM Simulation Model\n(Random Country Effects)'}
	return(coefs) })
coefData = do.call("rbind", coefData)
################################################

################################################
# get ready for plotting
names(coefData)[1:2] = c('mean', 'sd')
coefData = coefData %>%
		getCIVecs(.) %>% getSigVec(.)

# org for plotting
coefData$varName = factor(
	coefData$varName,
	levels=c(
		'Number of\nActors',
		'Number of\nConflicts',
		'Network \nCompetition'))
coefData$title = factor(coefData$title)

# rescale herf for interp purposes
# doing it here is equivalent to making
# the change in the data
coefData[
	coefData$var=='herf_und',c('mean','lo95','hi95','lo90','hi90')
	] = -1*coefData[
		coefData$var=='herf_und',c('mean','lo95','hi95','lo90','hi90')
		]
coefData$sig[coefData$var=='herf_und'] = 'Positive'

# viz
ggCoef = ggplot(
		coefData,
		aes(x=varName, y=mean, color=sig)) +
	geom_hline(
		aes(yintercept=0), linetype=2, color = "black") +
	geom_point(size=4) +
	geom_linerange(
		aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
	geom_linerange(
		aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +
	scale_colour_manual(
		values = coefp_colors, guide=FALSE) +
	ylab('') + xlab('') +
	facet_wrap(~title) +
	coord_flip() +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0),
		strip.text.x = element_text(
			size = 9, color='white'),
		strip.background = element_rect(
			fill = "#525252", color='#525252') )
ggsave(ggCoef,
	width=7, height=4,
	file=paste0(pathGraphics, 'abmCoefPlot.pdf'),
	device=cairo_pdf )
################################################
