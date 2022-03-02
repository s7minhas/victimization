############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }

#
loadPkg('geosphere')
############################

###########################
# load data
acled = suppressMessages( read_csv(
	paste0(
		pathData,
		"acled_1997-01-01-2020-07-02.csv")))

# subset to just africa since
# some of our IVs are only available for africa
cntryKey = data.frame(
	cntry = unique(acled$country), stringsAsFactors=F )
cntryKey$cont = countrycode(
	cntryKey$cntry, 'country.name', 'continent')
acled = acled[
	acled$country %in% cntryKey$cntry[cntryKey$cont=='Africa'] ,]

# subset to relev vars
acled = acled[, c(
    'actor1', 'actor2', 'year', 'country',
    'latitude', 'longitude', 'geo_precision')]
############################

############################
# melt into nodal df that identifies
# where actors interacted, goal
# here is to identify centroid position
tmp1 = acled[,-1] ; tmp2 = acled[,-2]
dat = abind(tmp1, tmp2, along=1)
dat = data.frame(dat, stringsAsFactors=F)
nVars = c('year', 'latitude', 'longitude', 'geo_precision')
for(v in nVars){ dat[,v] = num(dat[,v]) }
dat = dat[!is.na(dat$actor1),]

# subset to events with town/region spatial precision
# results are similar if we use geo_precision: 1:3 or just 1
# for more details on acled spatial precision codes see:
# https://www.acleddata.com/wp-content/uploads/dlm_uploads/2017/10/ACLED_Codebook_2019FINAL_pbl.pdf
dat = dat[dat$geo_precision %in% 1:2,]

# centroid positions by actor-country-year
cent = dat %>%
  group_by(actor1, country, year) %>%
  summarize(
    latMed=median(latitude),
    longMed=median(longitude),
    latMu=mean(latitude),
    longMu=mean(longitude) ) %>%
  ungroup() %>% data.frame(.,stringsAsFactors=F)
# calc'd both median and mu, correlated at
# ~.999 so doesnt make much diff which we go with
# cor(cent[,c('latMed', 'latMu')])
# cor(cent[,c('longMed', 'longMu')])

# calc distances between actor pairs in every country-year
# and use that info to generate a measure of how spread
# out actors are
cntries = unique(cent$country)
geoSpread = lapply(cntries, function(cntry){

  # subset to country
  cntryCent = cent[cent$country==cntry,]

  # get vector of years
  yrs = sort(unique(cntryCent$year))

  # iterate over years so we have a time
  # varying measure of spread
  cntrySpread = lapply(yrs, function(yr){

    # subset to year
    yrCent = cntryCent[cntryCent$year==yr,]

    # est geo dist in miles using geosphere
    # and then organize into df
    dmat = distm(
      data.matrix(yrCent[,c('longMed','latMed')]) )/1609.34
    rownames(dmat) = colnames(dmat) = yrCent$actor1
    ddf = melt(dmat)
    ddf$Var1=char(ddf$Var1) ; ddf$Var2=char(ddf$Var2)
    ddf = ddf[ddf$Var1 != ddf$Var2,]

    # gen summary stats of group spread
    spreadMu = mean(ddf$value)
    spreadMed = median(ddf$value)

    # org and return
    out = data.frame(
      country=cntry, year=yr,
      spreadMu=spreadMu, spreadMed=spreadMed,
      stringsAsFactors=FALSE )

    #
    return(out) })

  # roll up indiv cntry-year calcs into a df
  cntrySpread = do.call('rbind', cntrySpread)

  #
  return(cntrySpread) })

# org into single df
geoSpread = do.call('rbind', geoSpread)

# remove NA cases that result from lack
# of variation in actors
geoSpread = na.omit(geoSpread)

#
save(geoSpread, file=paste0(pathData, 'geoSpread_acled.rda'))
############################
