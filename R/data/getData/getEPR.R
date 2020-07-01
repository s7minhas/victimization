####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
####

############################
# load data
epr = foreign::read.dta(paste0(pathData, 'epr/EPR3CountryNewReduced.dta'))
############################

############################
# Process epr data

# include only post 1993 observations
epr = epr[epr$year >= 1993, ]

# Match country names with panel dataset
epr$country = char(epr$country) %>% trim()

# Convert to matching countrycodes
epr$cname=cname(epr$country)

# Other country name fixes
epr$cname[epr$cname=='Yugoslavia'] = 'SERBIA'

# Construct id from year + name
epr$cnameYear=paste0(epr$cname, '_', epr$year)

# Check for Dupes
names(table(epr$cnameYear)[table(epr$cnameYear)>1]) # Dupe check

# Adding in codes from panel
epr$ccode=panel$ccode[match(epr$cname,panel$cname)]
slice = unique(epr[is.na(epr$ccode),c('country','cname')]) # check for NAs
epr$cyear=paste(epr$ccode, epr$year, sep='_')
table(epr$cyear)[table(epr$cyear)>1] # Dupe check
############################

############################
# Save cleaned epr data
save(epr, file=paste0(pathData, 'epr/epr.rda'))
############################
