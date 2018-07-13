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

############################
# agg to cyear
kath = kath %>% group_by(missioncountry, year) %>%
	summarize(
		troop = sum(troop, na.rm=TRUE),
		police = sum(police, na.rm=TRUE),
		militaryobservers = sum(militaryobservers, na.rm=TRUE),
		total = sum(total, na.rm=TRUE)
		) %>% data.frame()
############################

############################
# Process kathman data

# include only post 1993 observations
kath = kath[kath$year >= 1993, ]

# pair country names with panel
kath$missioncountry = char(kath$missioncountry) %>% trim()

# remove missing obs 
kath = kath[which(kath$missioncountry!=''),]

# Convert to country code format	
kath$cname=cname(kath$missioncountry)

# clean countrynames
kath$cname[kath$cname=='Yugoslavia'] = 'SERBIA'

# add in ccode
kath$ccode=panel$ccode[match(kath$cname,panel$cname)]

# creat c-year ids
kath$cnameYear = char(with(kath, paste0(cname, '_', year)))
kath$ccodeYear = char(with(kath, paste0(ccode, '_', year)))

# Check for Dupes
names(table(kath$cnameYear)[table(kath$cnameYear)>1]) # Dupe check
table(kath$ccodeYear)[table(kath$ccodeYear)>1] # Dupe check

## dump timor-leste because we dont care about se asia
kath = kath[which(kath$cname!='TIMOR-LESTE'),]
############################

############################
# save
save(kath, file=paste0(pathData, 'kathman/kath.rda'))
############################