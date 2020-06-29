# 6/29/20

## actor inclusion rules for actor counts analysis

### actor rules:

- aggregate military/police into gov
- violence of any kind that led to at least 10 fatalities
  - so just do a group by actor and then sum of fatalities
- remove peacekeepers, election observers, militaries from other countries, and unidentified groups based on grepl searches

### country inclusion rules:

- country must be in africa

## actor inclusion rules for empirical data (and for web app)

### actor rules:

- aggregate military/police into gov
- violence of any kind that led to at least 10 fatalities
  - so just do a group by actor and then sum of fatalities
- include actors in the range of their event min and max year
- remove peacekeepers, election observers, militaries from other countries, and unidentified groups based on grepl searches

### country inclusion rules:

- country must be in africa
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
