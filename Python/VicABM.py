from random import *
import numpy as np
from string import *

class Territory(object):
  def __init__(self, name):
    self.name = name
    self.neighbors = []
    self.control = 0
    self.civilians = []
  def __repr__(self):
    return self.name


class ArmedActor(object):
  def __init__(self, name, gov):
    self.gov = gov
    if self.gov == 1:
      self.preference = 0.0
      self.ideo = 0.0
    else:
      self.preference = uniform(0,1)
      self.ideo = uniform(0,1)
    self.name = name
    self.territory = []
  def __repr__(self):
    return self.name

class Civilian(object):
  def __init__(self, name, Territory):
    self.preference = uniform(0, 1)
    self.VioHist = {}
    self.name = name
    self.support = {}
    self.territory = Territory
  def __repr__(self):
    return self.name


class Country(object):
  def __init__(self, name, size, connectedness, nactors, population):
    self.name = name
    adj = np.zeros(shape = (size, size))
    terr = []
    for i in range(size):
      adj[i] = np.append([0]*(i + 1),array(np.random.binomial(1, connectedness, size - i - 1)))
      terr.append(Territory(str(i)))
    tadj = adj.transpose()
    adj = adj + tadj
    check = adj.sum(axis = 0)
    for i in range(len(check)):
      if check[i] == 0:
        fix = np.random.randint(size - 1)
        if fix >= i:
          fix += 1
        adj[i][fix] = adj[fix][i] = 1
    for i in range(size):
      nbrs = np.where(adj[i] > 0)[0].tolist()
      for j in nbrs:
        terr[i].neighbors.append(terr[j])
    self.adj = adj
    self.provinces = terr
    actrs = []
    for i in range(nactors):
      actrs.append(ArmedActor(ascii_uppercase[i], i == (nactors - 1)))
      actrs[i].territory.append(terr[i])
      terr[i].control = actrs[i]
    civs = []
    for i in terr:
      pop = np.random.poisson(population)
      for j in range(pop):
        newbie = Civilian(ascii_lowercase[j] + i.name, i)
        i.civilians.append(newbie)
        civs.append(newbie)
      if i.control == []:
        i.control = actrs[nactors - 1]
        actrs[nactors - 1].territory.append(i)
    self.civilians = civs
    self.armedactors = actrs
    def __repr__(self):
      return self.name