# 7/17/20

empirical sample gets cut to only country-years that had at least one event based on our actor inclusion rules. this cut takes place in the 5_buildData.R file. 

# 7/3/20

- notable change, drop fatality threshold for events because it is too stringent according to qualitative case checks, specifically sierra leone

## actor inclusion rules for actor counts analysis

### actor rules:

- aggregate military/police into gov
- remove peacekeepers, election observers, militaries from other countries, and unidentified groups based on grepl searches

### country inclusion rules:

- global

## actor inclusion rules for empirical data (and for web app)

### actor rules:

- aggregate military/police into gov
- include actors in the range of their event min and max year (this is the only difference in the actor rules from the counts analysis)
- remove peacekeepers, election observers, militaries from other countries, and unidentified groups based on grepl searches

### country inclusion rules:

- country must be in africa ... many of our IVs only exist for africa
- country must have at least three years of conflict
- and testing out a few variants of median number of actors during conflict period criteria
  - no restriction
  - five actors or more
  - ten actors or more

# 7/2/20

## actor inclusion rules for actor counts analysis

### actor rules:

- aggregate military/police into gov
- violence of any kind that led to at least 10 fatalities
  - so just do a group by actor and then sum of fatalities
- remove peacekeepers, election observers, militaries from other countries, and unidentified groups based on grepl searches

### country inclusion rules:

- global

## actor inclusion rules for empirical data (and for web app)

### actor rules:

- aggregate military/police into gov
- violence of any kind that led to at least 10 fatalities
  - so just do a group by actor and then sum of fatalities
- include actors in the range of their event min and max year (this is the only difference in the actor rules from the counts analysis)
- remove peacekeepers, election observers, militaries from other countries, and unidentified groups based on grepl searches

### country inclusion rules:

- country must be in africa ... many of our IVs only exist for africa
- country must have at least three years of conflict
- and testing out a few variants of median number of actors during conflict period criteria
  - no restriction
  - five actors or more
  - ten actors or more

# 6/25/20

## sample of countries to choose:

we limit the countries we pull from ACLED based on the number of actors and dyadic pairs

sample of countries is chosen in:

- 1_caseSelecACLED.R


# 6/22/20

## actor inclusion rules:

- aggregate military/police into gov
- violence of any kind that led to at least 10 fatalities
  - so just do a group by actor and then sum of fatalities
- include actors in the range of their event min and max year
- not a peacekeeper or election observer
- remove unidentified groups based on grepl searches looking for the words unidentified, opposition
- create adjacency matrices such that any actor in the list and active (within their min-max) gets included ... calc net stats

these inclusion rules are used in the following file(s):

- 1_caseSelecACLED.R
