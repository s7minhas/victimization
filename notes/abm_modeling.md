# 7/17/2020

abm results hold with fixed and random effects for game. just note that the fixed effect model takes about 18 hours to run.

# 7/1/2020

- final model for abm will be a negative binomial with random effects, mean of dv is about 2.4 and var is about 6.8 so we're justified in our choice of negative binomial over poisson
- in the earlier model without random effects we were not finding support for the dens and avgDeg hyps after making the change to epsilon in the game
- we also tried coming up with an aggregation strategy by game s.t. we imagine every iteration of the game as a microturn and only look at the cumulative effects in the network, but because of the changing actor composition and general difficulties of longit nets we couldnt come up with an appropriate aggregation strategy for measures like dens and avgDeg
