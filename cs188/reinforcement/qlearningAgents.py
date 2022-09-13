# qlearningAgents.py
# ------------------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
#
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


from game import *
from learningAgents import ReinforcementAgent
from featureExtractors import *

import random
import util
import math


class QLearningAgent(ReinforcementAgent):
    """
      Q-Learning Agent

      Functions you should fill in:
        - computeValueFromQValues
        - computeActionFromQValues
        - getQValue
        - getAction
        - update

      Instance variables you have access to
        - self.epsilon (exploration prob)
        - self.alpha (learning rate)
        - self.discount (discount rate)

      Functions you should use
        - self.getLegalActions(state)
          which returns legal actions for a state
    """

    def __init__(self, **args):
        self.qvalues = dict()
        ReinforcementAgent.__init__(self, **args)

        "*** YOUR CODE HERE ***"

    def getQValue(self, state, action):
        if state not in self.qvalues:
            self.qvalues[state] = dict()
        if action not in self.qvalues[state]:
            return 0.0
        return self.qvalues[state][action]

    def computeValueFromQValues(self, state):
        legalActions = self.getLegalActions(state)
        if(len(legalActions) == 0):
            return 0.0
        return max([self.getQValue(state, action) for action in legalActions])

    def computeActionFromQValues(self, state):
        legalActions = self.getLegalActions(state)
        if(len(legalActions) == 0):
            return None
        _, action = max([(self.getQValue(state, action), action)
                        for action in legalActions], key=lambda a: a[0])
        return action

    def getAction(self, state):
        legalActions = self.getLegalActions(state)
        if len(legalActions) == 0:
            return None
        import random
        if util.flipCoin(self.epsilon):
            return random.choice(legalActions)
        return self.computeActionFromQValues(state)

    def update(self, state, action, nextState, reward):
        oldvalue = self.getQValue(state, action)
        nextvalue = self.computeValueFromQValues(nextState)
        newcalc = oldvalue * (1 - self.alpha) + \
            ((self.discount * nextvalue + reward) * self.alpha)

        self.qvalues[state][action] = newcalc

    def getPolicy(self, state):
        return self.computeActionFromQValues(state)

    def getValue(self, state):
        return self.computeValueFromQValues(state)


class PacmanQAgent(QLearningAgent):
    "Exactly the same as QLearningAgent, but with different default parameters"

    def __init__(self, epsilon=0.05, gamma=0.8, alpha=0.2, numTraining=0, **args):
        """
        These default parameters can be changed from the pacman.py command line.
        For example, to change the exploration rate, try:
            python pacman.py -p PacmanQLearningAgent -a epsilon=0.1

        alpha    - learning rate
        epsilon  - exploration rate
        gamma    - discount factor
        numTraining - number of training episodes, i.e. no learning after these many episodes
        """
        args['epsilon'] = epsilon
        args['gamma'] = gamma
        args['alpha'] = alpha
        args['numTraining'] = numTraining
        self.index = 0  # This is always Pacman
        QLearningAgent.__init__(self, **args)

    def getAction(self, state):
        """
        Simply calls the getAction method of QLearningAgent and then
        informs parent of action for Pacman.  Do not change or remove this
        method.
        """
        action = QLearningAgent.getAction(self, state)
        self.doAction(state, action)
        return action


class ApproximateQAgent(PacmanQAgent):
    def __init__(self, extractor='IdentityExtractor', **args):
        self.featExtractor = util.lookup(extractor, globals())()
        PacmanQAgent.__init__(self, **args)
        self.weights = util.Counter()

    def getWeights(self):
        return self.weights

    def getQValue(self, state, action):
        features = self.featExtractor.getFeatures(state, action)
        sum = 0
        for key in features:
            sum += features[key] * self.weights[key]
        return sum

        """
          Should return Q(state,action) = w * featureVector
          where * is the dotProduct operator
        """
        "*** YOUR CODE HERE ***"

    def update(self, state, action, nextState, reward):
        features = self.featExtractor.getFeatures(state, action)

        oldvalue = self.getQValue(state, action)
        nextvalue = self.computeValueFromQValues(nextState)
        difference = ((self.discount * nextvalue + reward)) - oldvalue

        for key in features:
            self.weights[key] = self.weights[key] + \
                self.alpha * difference * features[key]

    def final(self, state):
        "Called at the end of each game."
        # call the super-class final method
        PacmanQAgent.final(self, state)

        # did we finish training?
        if self.episodesSoFar == self.numTraining:
            # you might want to print your weights here for debugging
            "*** YOUR CODE HERE ***"
            pass
