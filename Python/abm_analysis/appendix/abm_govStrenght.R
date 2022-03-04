# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

loadPkg(c('MASS', 'foreach', 'doParallel'))
################################################

# load in data #################################
load(paste0(abmPath, 'netStats.rda'))
################################################

################################################
# mods to iterate through
parInfo = expand.grid(
	gVal = unique(netStats$govStrengthBin),
	mod = c('re', 'fe') )
for(i in 1:ncol(parInfo)){ parInfo[,i] = char(parInfo[,i]) }
################################################

################################################
# run base mod in parallel
cores = nrow(parInfo)
cl = makeCluster(cores)
registerDoParallel(cl)
res = foreach(
	ii = 1:nrow(parInfo),
	.packages=c( 'MASS', 'glmmTMB' ) ) %dopar% {

# get govStrength val and mod type
g = parInfo$gVal[ii]
mType = parInfo$mod[ii]

# slice data
slice = netStats[netStats$govStrengthBin==g,]

# run fe mod
if(mType=='fe'){
	form=formula(paste0('vic~numConf+n_actors+herf_und+ factor(game)-1'))
	mod = glm.nb(form, data=slice)
	out = summary(mod)$'coefficients' }

# run re mods
if(mType=='re'){
	form=formula(paste0('vic~numConf+n_actors+herf_und+ (1|game)'))
	mod = glmmTMB( form, data=slice, family='nbinom2' )
	out = summary(mod)$'coefficients'$cond }

return(out) }
stopCluster(cl)
names(res) = apply(parInfo, 1, paste, collapse='_')
################################################

################################################
# load and process coef data
cleanVars = c(
	'Number of\nConflicts','Number of\nActors', 'Network \nCompetition')
coefData = lapply(1:length(res), function(ii){
	# pull out labels for mod
	lab = names(res)[ii]
	g = substring(lab, 1, 1)
	mType = substring(lab, 3, nchar(lab))

	# pull out relev mod
	coefs = res[[ii]]

	# subset to relev coefs
	if(mType=='fe'){ coefs = coefs[1:3,]}
	if(mType=='re'){ coefs = coefs[-1,]}

	# org
	coefs = data.frame(coefs, stringsAsFactors=FALSE)
	coefs$var = rownames(coefs) ; rownames(coefs) = NULL
	coefs$varName = cleanVars

	# add govStrength labs
	if(g=='1'){coefs$gLab = 'High Gov. Strength'}
	if(g=='0'){coefs$gLab = 'Low Gov. Strength'}

	# add mType labs
	if(mType=='fe'){
		coefs$title='ABM Simulation Model\n(Fixed Country Effects)'}
	if(mType=='re'){
		coefs$title='ABM Simulation Model\n(Random Country Effects)'}

	#
	return(coefs) })
coefData = do.call("rbind", coefData)
save(coefData,
	file=paste0(pathResults, 'abmCoefs_govStrength.rda'))
################################################

################################################
# rescale herf for interp purposes
coefData$Estimate[
	coefData$var=='herf_und'] = -1*coefData$Estimate[coefData$var=='herf_und']

# relabel
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
	facet_grid(gLab~title) +
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
	width=7, height=5,
	file=paste0(pathGraphics, 'abmCoefPlot_govStrength.pdf'))
################################################
