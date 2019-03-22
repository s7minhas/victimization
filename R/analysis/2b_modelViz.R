########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }
########################################################

########################################################
load(paste0(pathResults, 'nbMods_acled.rda'))
########################################################

########################################################
# prep coef summ for plotting
prepData = function(raw,mName){
	coefData = raw[!grepl('factor',rownames(raw)),] %>%
		data.frame(.,stringsAsFactors=FALSE) %>%
		setNames(c('mean','sd','tstat')) %>%
		mutate(
			var=rownames(.),
			model=mName
			) %>%
		getCIVecs(.) %>%
		getSigVec(.) 
	return(coefData) }

# combine model results
coefData = rbind(
	prepData(summBase,'Base ACLED Model'),
	prepData(summCntrls,'Base + Controls ACLED Model')
	)
coefData$model = factor(
	coefData$model,
	levels=unique(coefData$model)
	)


# varKey
varKey = data.frame(
	dirty=unique(coefData$var),
	stringsAsFactors = FALSE
	)
varKey$clean = c(
	'Graph Density',
	'Number of\nConflicts',
	'Number of\nActors',
	'Polity', 'Log(Population)',
	'Log(GDP per Capita)',
	'Ethnic\nFractionalization',
	'Presence of\nPeacekeepers',
	'Rebel(s) Stronger\nthan Govt.',
	'Rebel(s) Supported\nby Foreign Govt.',
	'Govt. Supported\nby Foreign Govt.'
	)
coefData$varName = varKey$clean[
	match(coefData$var,varKey$dirty) ]
coefData$varName = factor(
	coefData$varName,
	levels=rev(varKey$clean)
	)
########################################################

########################################################
# viz
ggCoef = ggplot(coefData, aes(x=varName, y=mean, color=sig)) +
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_point(size=4) + 
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +		
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	ylab('') + xlab('') +
	facet_wrap(~model) +
	coord_flip() +	
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0),
		strip.text.x = element_text(size = 9, color='white'),
		strip.background = element_rect(fill = "#525252", color='#525252')		
	)	
ggsave(ggCoef, 
	width=8, height=6,
	file=paste0(pathGraphics, 'acled_coefPlot.pdf'),
	device=cairo_pdf
	)
########################################################