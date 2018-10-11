####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }
####

############################
# load data
wood = foreign::read.dta(paste0(pathData, 'wood2010/wood_jprdata_rebels.dta'))
############################

############################
# subset to location and loot goods
wood = wood[,c(
	'location',
	'secdia_dum','drugs2','gems_all'
	)]

# clean up names
wood$cname = cname(wood$location)

# clean up congo
wood$cname[wood$location=='Congo/Zaire'] = 'CONGO, THE DEMOCRATIC REPUBLIC OF'

# aggregate to max
wood = wood %>%
	group_by(cname) %>%
	summarize_all(max) %>% data.frame()

# add ccodes
wood$ccode = panel$ccode[match(wood$cname,panel$cname)]
############################

############################
# save
save(wood, file=paste0(pathData, 'wood2010/wood.rda'))
############################