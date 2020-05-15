###

if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
  source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('dorffc')){ 
  source('~/ProjectsGit/victimization/R/setup.R') }


loadPkg('readr')
acled = read_csv(paste0(pathData,
                "acled-armedactors-battles.csv"))

# GET THA FACTS!
# How many countries that have 'battles'  from ALL years of ACLED data
# (maybe should subset on like actual civil wars?) 

# how many countries?
length(unique(acled$country))# 113

# how many unique actors per country?
# first select a few key vars
acled <- acled %>% 
  as_tibble() %>% 
  select('event_type','year', 'actor1', 'actor2', 'country','latitude', 'longitude')

# then count up unique actors per group across act1 and act2
groups = acled %>%
  group_by(country) %>%
  summarise(number_groups = n_distinct(actor1, actor2)) %>%
  arrange(desc(number_groups))

# now make a map that is dumb w/o further revisions
library(readr)
library(ggmap)
library(dplyr)

world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

## check for disagreements between the two datasets
diff <- setdiff(world$region, groups$country) #ugh lots to clean

#if I do not fix above^ this is wrong:
names(world)[names(world) == "region"] <- "country"
worldSubset <- inner_join(world, groups, by = "country")
head(worldSubset)

worldSubset %>% 
  filter(number_groups>100)

## theme
basic <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldGroups <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = number_groups)) +
  scale_fill_distiller(palette ="BrBG", direction = -1) +
  ggtitle("Number of warring parties") +
  basic