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
loadPkg(c('MASS', 'stringr'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))

# set up spec
dv = 'civVicCount'
ivs = c('graph_dens', 'nConf', 'nActors')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
########################################################

########################################################
# run mods varying cntries included
cntries = sort(unique(dataBase$cname))
gDensSens = lapply(cntries, function(cntry){
	slice = dataBase[dataBase$cname!=cntry,]
	feModBase = glm.nb(feF, data=slice)
	feModBaseCoef = summary(feModBase)$'coefficients'[1:p,]
	gDensCoefFE = feModBaseCoef[1,,drop=FALSE]
	return(gDensCoefFE) })

# cleanup
gDensSens = do.call('rbind', gDensSens)
colnames(gDensSens)[1:3] = c('mean', 'sd', 'tstat')
gDensSens = data.frame(gDensSens, stringsAsFactors=FALSE, row.names=NULL)
gDensSens$cntry = str_to_title(cntries)
gDensSens = getSigVec(getCIVecs(gDensSens))
gDensSens$title = 'Effect of Graph Density\nwhen Dropping a Country'
########################################################

########################################################
ggCoef = ggplot(gDensSens, aes(x=factor(cntry), y=mean, color=sig)) +
	geom_hline(aes(yintercept=0), linetype=2, color = "black") +
	geom_point(size=4) +
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	ylab('') + xlab('') + facet_wrap(~title) +
	coord_flip() + theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0),
		strip.text.x = element_text(size = 9, color='white'),
		strip.background = element_rect(
			fill = "#525252", color='#525252'))
ggsave(ggCoef, width=8, height=6,
	file=paste0(pathGraphics, 'gDensSens.pdf'),
	device=cairo_pdf)
########################################################
