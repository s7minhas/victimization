####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
####

############################
# Download WB data using WDI package
loadPkg('WDI')
# File name to store data
fName = paste0(pathData, 'worldBank/worldBankVars.csv')
wbVars = c(
	'NY.GDP.MKTP.KD', # GDP, constant US
	'NY.GDP.PCAP.KD', # GDP per capita, constant US
	'NY.GDP.MKTP.KD.ZG', # GDP growth
	'SP.POP.TOTL' # Population
	)

# Call WDI website
wbData = WDI(country='all', 
	indicator=wbVars, 
	start=1993, end=2018, extra=TRUE )
write.csv(wbData, file=fName)

# Change names
wbVarsClean = c('gdp', 'gdpCap', 'gdpGr', 'pop')
names(wbData)[4:(length(wbVars)+3)] = wbVarsClean
############################

############################
# Process WB data

# Create matching countrynames
wbData$cname = countrycode(wbData$iso2c, 'iso2c', 'country.name')

# Drop aggregated WB units
wbData = wbData[!is.na(wbData$cname),]

# Create country + year id
wbData$cnameYear = paste0(wbData$cname, '_', wbData$year)

# Check duplicates
table(wbData$cnameYear)[table(wbData$cnameYear)>1]

# Add countrycodes
wbData$ccode = panel$ccode[match(wbData$cname, panel$cname)]

# Drop small islands mostly
wbData = wbData[!is.na(wbData$ccode),]

# Create ccode + year id
wbData$cyear = paste0(wbData$ccode, '_', wbData$year)

# Check duplicates
table(wbData$cyear)[table(wbData$cyear)>1]
############################

############################
# Create logged version of vars
wbData$gdpLog = log(wbData$gdp)
wbData$gdpCapLog = log(wbData$gdpCap)
wbData$popLog = log(wbData$pop)
############################

############################
# Save
worldBank = wbData
save(worldBank, file=paste0(pathData, 'worldBank/worldBank.rda'))
############################