########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))
########################################################

########################################################
# viz over time
# calc yearly averages
yrAgg = dataBase %>%
  group_by(year) %>%
  summarize(
    herfMu = mean(herf),
    herfMed = median(herf),
		herf25 = quantile(herf, .25),
		herf75 = quantile(herf, .75),
		herf10 = quantile(herf, .1),
		herf90 = quantile(herf, .9)
  )
yrBrks = round(seq(1997, 2020, length.out=4))

# plot
ggMed = ggplot(dataBase, aes(x=factor(year), y=herf) ) +
 	geom_violin(
		kernel='rectangular',
		alpha=.5,
		fill='grey60', color='grey60'
		)	+
	geom_jitter(
		data=dataBase,
		alpha=.15, width=.1) +
	# geom_line(
	# 	data=yrAgg, aes(x=factor(year), y=herfMed, group=1),
	# 	size=.4 ) +
	geom_point(data=yrAgg,
		aes(x=factor(year), y=herfMed),
		size=20, shape='-', color='grey40' ) +
	xlab('') + ylab('Network Competition') +
	scale_x_discrete(breaks=yrBrks) +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0)
	)
ggsave(
	ggMed,
	width=8, height=5,
	file=paste0(pathGraphics, 'netCompMed.pdf'),
	device=cairo_pdf )
####################################

########################################################
# viz over time by countries

# add country acronyms for plots
dataBase$cabb = countrycode(dataBase$cname, 'country.name', 'cowc')

# plot
ggCntry = ggplot(
  dataBase, aes(x=year, y=herf, group=1) ) +
  geom_line(size=.4) + geom_point(size=.6) +
  facet_wrap(~cabb, scale='free_y') +
	xlab('') + ylab('Network Competition by Country') +
	scale_x_continuous(breaks=yrBrks) + 
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.x=element_text(angle=45, hjust=1, size=7),
		strip.text.x = element_text(size = 9, color='white'),
		strip.background = element_rect(
			fill = "#525252", color='#525252'))
ggsave(
	ggCntry,
	width=8, height=8,
	file=paste0(pathGraphics, 'netCompCntry.pdf'),
	device=cairo_pdf )
########################################################
