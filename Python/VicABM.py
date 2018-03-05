from random import *
import numpy as np
from string import *
import operator
import networkx as nx
import copy
###Game Parameters, we Can Change These
#How much less people like you when you kill your supporters
VicPenalty = .05
#How many resources you get for non-supporters
CoerceMob = .3
#How much worse fighting is for you for each supporter of your enemy in your territory
DisloyalPenalty = .5
##Discount of resources over space
delta = .2
#Number of civilians in each battlefield territory
battledeaths = 2
#Rate that the population grows in a turn
growthrate = .1
#How likely you are, with 1 supporter, to incorrectly kill a supporter
victimerror = .1



class Territory(object):
  def __init__(self, name):
    self.name = name
    self.neighbors = []
    self.control = 0
    self.civilians = []
    self.country = 0
    self.resources = 0
    self.attack = []
    self.exsupp = 0
    self.exstr = {}
###Check whether each civilian in a territory supports the dominant group, then also get an expected level of support for future period calculations
  def SupportCheck(self):
    self.exsupp = 0
    if self.attack == 0:
      for i in self.civilians:
        i.InteriorSupport()
        self.exsupp += (i.support == i.territory.control)
###Get resources from a territory
  def Mobilize(self):
    resources = 0
    for i in self.civilians:
      if i.support == self.control:
        resources += 1
##Apply penalty to the territory if its part of the battlefield
      elif len(self.attack) != 0 & i.support != self.control:
        if i.support in self.attack.control:
          resources -= DisloyalPenalty
      else:
        resources += CoerceMob
    self.resources = resources
##Calculate expected resources you could apply in this territory
  def ExpectedStrength(self):
    for i in self.country.armedactors:
      exst = 0
      for j in i.territory:
        dist = max(self.country.dists[j][self] - 1, 0)
        exst += delta**dist*(CoerceMob*len(j.civilians) + (1 - CoerceMob)*j.exsupp)
      self.exstr[i] = exst
  def Attack(self, target):
    if target not in self.neighbors:
      raise ValueError('This attack cant happen, water is too wet.')
    self.attack.append(target)
    target.attack.append(self)
##Resolve a battle involving this territory
  def Battle(self):
    rsrs = {}
    if len(self.attack) > 0:
      ###Make a list of the actors fighting
      combatants = [self.control]
      for i in self.attack:
        combatants.append(i.control)
      ##Calculate their resources
      for i in combatants:
        res = 0
        trr = i.control.territory
        for j in trr:
          j.Mobilize()
          dist = max(self.country.dists[j][self] - 1, 0)
          res += delta**dist*j.resources
        rsrs[i] = res
    #Transform to probability
    probs = rsrs
    denom = sum(rsrs.values())
    for i in probs.keys():
      probs[i] /= denom
    #Who won?
    outcome = np.random.multinomial(1, list(probs.values()))
    winner = 0
    #Winner takes the territory
    for i in range(len(probs.keys())):
      if outcome[i] == 1:
        winner = list(probs.keys())[i]
        old = copy.deepcopy(self.control)
        self.control = list(probs.keys())[i]
        newsupp = 0
        if self.control != old:
          for j in self.civilians:
            newsupp += j.support == self.control
    #People died, it sucks
    battlefield = self.attack.append(self)
    for j in battlefield:
      for i in range(battledeaths):
        shuffle(j.civilians)
        j.civilians.pop()
    #All these people dying, who are we gonna mobilize next time?
  def Growth(self,rate):
      add = round(len(self.civilians)*rate)
      for i in range(add):
        newbie = Civilian(ascii_lowercase[len(self.civilians) + i + 1] + self.name, self)
        self.civilians.append(newbie)
        self.country.civilians.append(newbie)
  ##Some people arent supporting you, that also sucks, maybe we can kill them?
  def Victimize(self):
    supps = []
    nosupp = []
    number = 0 
##List of supporters and opponents in the territory
    for i in self.civilians:
      if i.support == self.control:
        supps.append(i)
        number += 1
      else:
        nosupp.append(i)
#Based on how many supporters, you get a probability of selective vs indiscriminate violence
    selectprob = 1 - e*((len(self.civilians) - len(supp) + 1))/len(self.civilians)
    selective = np.random.binomial(1, selectprob, 1)
##If it works, you kill an opponent, everyone loves you!
    if selective == 1:
      nosupp.pop()
      self.control.VioHist += 1
      self.exsupp -= (1 + VicPenalty*len(nosupp))
##If it doesnt, you kill a supporter, man, supporting you seems like a bad idea
    if selective == 0:
      supp.pop()
      self.control.VioHist -= 1
      self.exsupp += VicPenalty*len(nosupp)
    self.civilians = supps + nosupp
  def __repr__(self):
    return self.name


class ArmedActor(object):
  def __init__(self, name, gov):
    self.gov = gov
    ##Government doesnt share (ideo = 0) and has extreme preferences. Alternatively we could have them have moderate prefs.
    if self.gov == 1:
      self.preference = 0.0
      self.ideo = 0.0
    else:
      self.preference = uniform(0,1)
      self.ideo = uniform(0,1)
    self.name = name
    self.territory = []
    self.VioHist = 0
    self.country = 0
  def __repr__(self):
    return self.name



class Civilian(object):
  def __init__(self, name, Territory):
    self.preference = uniform(0, 1)
    self.name = name
    self.support = 0
    self.territory = Territory
    self.country = 0
  #Calculate whether civilians will support the armed actor in a non-battle territory
  def InteriorSupport(self):
    IncSupp = 0
    IncPref = self.territory.control.preference
    #Get a probability the other civilians are supporting the actor?
    for i in self.territory.civilians:
      if i != self:
        IncSupp += min(max(1 - abs(IncPref - i.preference) + self.territory.control.VioHist*VicPenalty,1),0)
    IncSupp /= len(self.territory.civilians)
    #Compare that to your ideological distance
    if IncSupp/2 > abs(IncPref - self.preference) - self.territory.control.VioHist*VicPenalty:
      self.support = self.territory.control
    else: 
      self.support = 0
  def BattleSupport(self):
    combatants = [self.territory.control]
    for i in self.territory.attack:
      combatants.append(i.control)
    rsr = {}
    for i in combatants:
      res = 0
      for j in i.territory:
        if j not in self.territory.attack:
          for k in j.civilians:
            res += min(max(1 - abs(i.preference - k.preference) + i.VioHist*VicPenalty,1),0)*(1 - CoerceMob) + CoerceMob
        if j in self.territory.attack:
          for k in j.civilians:
            diffs = 0
            for l in combatants:
              diffs += abs(k.preference - l.preference)
            es = min(max(1 - abs(i.preference - k.preference)/diffs + i.VioHist*VicPenalty,1),0)
            res += es - (CoerceMob -DisloayPenalty)*(1-es)
      rsr[i] = res
      utils = rsr
    for c in combatants:
      rsr[c] /= sum(rsr.values())
      utils[c]  = rsr[c]*(1 - abs(c.preference - self.preference) + c.VioHist*VicPenalty)
    max_key = max(utils, key=lambda k: utils[k])
    max_value = max(utils.values()); 
    choices = [key for key, value in stats.items() if value == max_value]
    if self.territory.control in choices:
      self.support = self.territory.control
    else:
      self.support = shuffle(choices)[0]
  #Civilians can leave... Might want to have a risk of it going really badly and them getting killed tho
  def Flee(self, target):
    self.territory.civilians.remove(self)
    self.territory = target
    target.civilians.append(self)
  #Calculate if civilians want to flee
  def CheckFlee(self):
  #Need to not have supported the incumbent
    if self.support != self.territory.control:
    #Make a list of options, neighboring territories
      opt = {}
      current = abs(self.preference - self.territory.control.preference) - self.territory.control.VioHist*VicPenalty
      for i in self.territory.neighbors:
        opt[i] = abs(self.preference - i.control.preference) - i.control.VioHist*VicPenalty
      #Is the best alternative better than the SQ?
      if min(opt.items(), key=operator.itemgetter(1))[1] < current:
        self.Flee(min(opt.items(), key = operator.itemgetter(1))[0])
  def __repr__(self):
    return self.name



##Object containing the other objects, used for populating everything
#Size = number of provinces, connectedness is how dense the adjacency matrix is
#Nactors is number of armed actors
#Population is average population of a territory
class Country(object):
  def __init__(self, name, size, connectedness, nactors, population):
    self.name = name
    adj = np.zeros(shape = (size, size))
    terr = []
    ##Generate a random adjacency matrix of appropiate size and create territories
    for i in range(size):
      adj[i] = np.append([0]*(i + 1),np.array(np.random.binomial(1, connectedness, size - i - 1)))
      terr.append(Territory(str(i)))
      terr[-1].country = self
    tadj = adj.transpose()
    adj = adj + tadj
    #Its symmetrical
    ##Oh no, isolates are no good, lets give them one random link.
    check = adj.sum(axis = 0)
    for i in range(len(check)):
      if check[i] == 0:
        fix = np.random.randint(size - 1)
        if fix >= i:
          fix += 1
        adj[i][fix] = adj[fix][i] = 1
    ##Make sure each territory has a record of their neighbors
    for i in range(size):
      nbrs = np.where(adj[i] > 0)[0].tolist()
      for j in nbrs:
        terr[i].neighbors.append(terr[j])
    self.adj = adj
    ##Calculate distances using networkx
    self.dists = nx.shortest_path_length(nx.from_numpy_matrix(self.adj))
    self.provinces = terr
    ##Now lets generate armed actors, the last one will be the government, each actor controls one territory
    actrs = []
    for i in range(nactors):
      actrs.append(ArmedActor(ascii_uppercase[i], i == (nactors - 1)))
      ##Make sure can reference country level stuff using an armed actor
      actrs[-1].country = self
      actrs[i].territory.append(terr[i])
      terr[i].control = actrs[i]
    ##Generate civilians in each territory
    civs = []
    for i in terr:
      pop = np.random.poisson(population)
      for j in range(pop):
        newbie = Civilian(ascii_lowercase[j] + i.name, i)
        i.civilians.append(newbie)
        civs.append(newbie)
        ##Make sure civilians also have a country
        civs[-1].country = self
      if i.control == 0:
      ##Except the government who controls all the other territories
        i.control = actrs[nactors - 1]
        actrs[nactors - 1].territory.append(i)
    self.civilians = civs
    self.armedactors = actrs
  def __repr__(self):
      return self.name


##Playing around to see if stuff works

US = Country("USA", 10, .2, 5, 15)
z = US.provinces[0].civilians[1]
P0 = US.provinces[0]

P0.SupportCheck()
P0.Mobilize()