from VicABM import *
import csv

with open('abmresults1.csv', 'w') as csvfile:
  abmwriter = csv.writer(csvfile, delimiter=',')
  for i in range(10000):
    territory = np.random.poisson(20)
    actors = min(np.random.poisson(10), territory - 2)
    conn = np.random.uniform(low = .2, high = .75)
    civ = np.random.poisson(45)
    VicPenalty = np.random.uniform(low = .05, high = .3)
    CoerceMob = np.random.uniform(low = .1, high = .5)
    DisloyalPenalty = np.random.uniform(low = .25, high = .75)
    delta = np.random.uniform(low = .1, high = .5)
    battledeaths = np.random.poisson(1) + 1
    growthrate = .1
    victimerror = np.random.uniform(low = 0, high = .15)
    turnlimit = np.random.poisson(10) + 1
    abm = Country("abm", territory, conn, actors, civ)
    abm.Game()
    if i%1000 == 0:
      print(i)
    abmwriter.writerow([actors, territory, conn, civ, VicPenalty, CoerceMob, delta, battledeaths, growthrate, victimerror, turnlimit, abm.victimhist, abm.attackhist, abm.actorlists])