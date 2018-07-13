####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }
####

############################
# load data
kath = foreign::read.dta(paste0(pathData, 'kathman/CMPS Mission Totals 1990-2011.dta'))
############################

# Process kathman data

# include only post 1993 observations
kath = kath[kath$year >= 1993, ]
sum(is.na(kath$missioncountry))

# pair country names with panel
kath$missioncountry = char(kath$missioncountry) %>% trim()

# Convert to country code format	
kath$cname=cname(kath$missioncountry)
	#check 
	tmp<-cbind(kath$missioncountry, kath$cname)
	write.csv(tmp, file = paste0(pathData, 'kathman/tmp.csv'))

# add in ccode
kath$ccode=panel$ccode[match(kath$cname,panel$cname)]
	#check 
	tmp<-cbind(kath$cname, kath$ccode)
	write.csv(tmp, file = paste0(pathData, 'kathman/tmp.csv'))

write.csv(tmp, file = paste0(pathData, 'kathman/tmp.csv'))


