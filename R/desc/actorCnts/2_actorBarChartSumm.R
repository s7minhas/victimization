###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }
###########################################################

###########################################################
load(paste0(pathData, 'actorCntsID.rda'))
###########################################################

###########################################################
# categorical vars
actorCntsID$nCat3 = addCat(
	actorCntsID$nActors,
	c('4 or\nless','5-9','10 or\nmore'), c(5,10))
###########################################################

###########################################################
# viz diff in # actors over time via cat variable
actorCats = ggplot(actorCntsID,
  aes(
    x=factor(year),
    fill=factor(nCat3)
  )) +
  geom_bar(colour='grey40') +
  scale_fill_brewer(palette='Greys') +
  labs(
    x='',
    y='# Active Armed Groups',
    fill=''
  ) +
	theme_light(base_family="Source Sans Pro") +
  smTheme +
  theme(
    axis.text.x=element_text(angle=45),
	 	legend.position='top' )

ggsave(actorCats,
  file=paste0(pathGraphics, 'actorCntCats.pdf'),
  width=8, height=6, device=cairo_pdf)
###########################################################
