####
require(here) ; pth = here::here()
source(paste0(pth, '/R/setup.R'))
####

############################
# load data
# updated data from kathman
# https://kathmanundata.weebly.com/uploads/1/2/9/6/129650971/kathman_cmps_2013.pdf
kath = read.csv(paste0(pathData, 'kathman/mission-month_12-2019.csv'))[,-1]
############################

############################
# preprocessing
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

# remove missioncountry: Unknown and Various
kath = kath[!is.na(kath$cname),]
############################

############################
# agg to cyear
kath = kath %>% group_by(cname, year) %>%
	summarize(
		troop = sum(troop, na.rm=TRUE),
		police = sum(police, na.rm=TRUE),
		militaryobservers = sum(militaryobservers, na.rm=TRUE),
		total = sum(total, na.rm=TRUE),
		total2 = sum(total2, na.rm=TRUE)
		) %>% data.frame()
############################

############################
# finish up processing
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
save(kath, file=paste0(pathData, 'kathman/kath_v2.rda'))
############################
