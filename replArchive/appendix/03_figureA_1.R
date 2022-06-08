########################################################
source(paste0(here::here(), '/setup.R'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))
########################################################

########################################################
# viz over time by countries

# add country acronyms for plots
dataBase$cabb = countrycode(dataBase$cname, 'country.name', 'cowc')

# yrs to show on x axis
yrBrks = round(seq(1997, 2020, length.out=4))

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
	file=paste0(pathGraphics, 'appendix/figure_A1.png'),
	dpi=600 )
########################################################
