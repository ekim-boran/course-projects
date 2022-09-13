# valueIterationAgents.py
# -----------------------
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


# valueIterationAgents.py
# -----------------------
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


import mdp
import util

from learningAgents import ValueEstimationAgent
import collections


class ValueIterationAgent(ValueEstimationAgent):
    """
        * Please read learningAgents.py before reading this.*

        A ValueIterationAgent takes a Markov decision process
        (see mdp.py) on initialization and runs value iteration
        for a given number of iterations using the supplied
        discount factor.
    """

    def __init__(self, mdp, discount=0.9, iterations=100):
        """
          Your value iteration agent should take an mdp on
          construction, run the indicated number of iterations
          and then act according to the resulting policy.

          Some useful mdp methods you will use:
              mdp.getStates()
              mdp.getPossibleActions(state)
              mdp.getTransitionStatesAndProbs(state, action)
              mdp.getReward(state, action, nextState)
              mdp.isTerminal(state)
        """
        self.mdp = mdp
        self.discount = discount
        self.iterations = iterations
        self.values = util.Counter()  # A Counter is a dict with default 0
        self.runValueIteration()

    def runValueIteration(self):
        self.temp = util.Counter()
        for _ in range(0, self.iterations):
            for state in self.mdp.getStates():
                if self.mdp.isTerminal(state):
                    self.values[state] = 0
                    continue
                max = -1000
                for action in self.mdp.getPossibleActions(state):
                    x = 0
                    for (nextState, prob) in self.mdp.getTransitionStatesAndProbs(state, action):
                        reward = self.mdp.getReward(state, action, nextState)
                        x += (reward +
                              (self.values[nextState] * self.discount)) * prob
                    if x >= max:
                        max = x
                self.temp[state] = max
            self.values, self.temp = self.temp, self.values

    def getValue(self, state):
        return self.values[state]

    def computeQValueFromValues(self, state, action):
        x = 0
        for (nextState, prob) in self.mdp.getTransitionStatesAndProbs(state, action):
            reward = self.mdp.getReward(state, action, nextState)
            x += (reward + (self.values[nextState] * self.discount)) * prob
        return x

    def computeActionFromValues(self, state):
        max = -100000
        selectedAction = None
        for action in self.mdp.getPossibleActions(state):
            x = 0
            for (nextState, prob) in self.mdp.getTransitionStatesAndProbs(state, action):
                reward = self.mdp.getReward(state, action, nextState)
                x += prob * \
                    (reward + (self.values[nextState] * self.discount))
            if x >= max:
                max = x
                selectedAction = action
        return selectedAction

    def getPolicy(self, state):
        return self.computeActionFromValues(state)

    def getAction(self, state):
        "Returns the policy at the state (no exploration)."
        return self.computeActionFromValues(state)

    def getQValue(self, state, action):
        return self.computeQValueFromValues(state, action)


class AsynchronousValueIterationAgent(ValueIterationAgent):
    """
        * Please read learningAgents.py before reading this.*

        An AsynchronousValueIterationAgent takes a Markov decision process
        (see mdp.py) on initialization and runs cyclic value iteration
        for a given number of iterations using the supplied
        discount factor.
    """

    def __init__(self, mdp, discount=0.9, iterations=1000):
        """
          Your cyclic value iteration agent should take an mdp on
          construction, run the indicated number of iterations,
          and then act according to the resulting policy. Each iteration
          updates the value of only one state, which cycles through
          the states list. If the chosen state is terminal, nothing
          happens in that iteration.

          Some useful mdp methods you will use:
              mdp.getStates()
              mdp.getPossibleActions(state)
              mdp.getTransitionStatesAndProbs(state, action)
              mdp.getReward(state)
              mdp.isTerminal(state)
        """
        ValueIterationAgent.__init__(self, mdp, discount, iterations)

    def runValueIteration(self):

        statelist = self.mdp.getStates()
        l = len(statelist)
        for i in range(0, self.iterations):
            state = statelist[i % l]
            if self.mdp.isTerminal(state):
                self.values[state] = 0
                continue
            max = -1000
            for action in self.mdp.getPossibleActions(state):
                x = 0
                for (nextState, prob) in self.mdp.getTransitionStatesAndProbs(state, action):
                    reward = self.mdp.getReward(state, action, nextState)
                    x += (reward +
                          (self.values[nextState] * self.discount)) * prob
                if x >= max:
                    max = x
            if len(self.mdp.getPossibleActions(state)) != 0:
                self.values[state] = max


class PrioritizedSweepingValueIterationAgent(AsynchronousValueIterationAgent):

    def __init__(self, mdp, discount=0.9, iterations=100, theta=1e-5):
        """
          Your prioritized sweeping value iteration agent should take an mdp on
          construction, run the indicated number of iterations,
          and then act according to the resulting policy.
        """
        self.theta = theta
        ValueIterationAgent.__init__(self, mdp, discount, iterations)

    def init_predeccessors(self):
        pred = dict()
        for state in self.mdp.getStates():
            pred[state] = set()

        for state in self.mdp.getStates():
            for action in self.mdp.getPossibleActions(state):
                for (newstate, prob) in self.mdp.getTransitionStatesAndProbs(state, action):
                    if(prob > 0  ):
                        pred[newstate].add(state)
        return pred

    def calculate_state(self, state):

        max = -1000
        for action in self.mdp.getPossibleActions(state):
            x = 0
            for (nextState, prob) in self.mdp.getTransitionStatesAndProbs(state, action):
                reward = self.mdp.getReward(state, action, nextState)
                x += (reward +
                      (self.values[nextState] * self.discount)) * prob
            if x >= max:
                max = x
        return max

    def update_pq(self, pq, states, first):
        for state in states:
            if(self.mdp.isTerminal(state)):
                continue
            current_value = self.values[state]
            maxqvalue = max([self.computeQValueFromValues(state, action)
                            for action in self.mdp.getPossibleActions(state)])
            diff = abs(current_value - maxqvalue)
            #print("update pg:", state, diff, current_value, maxqvalue, "x")
            if(first):
                pq.update(state, -diff)
            else:
                if diff > self.theta:
                    pq.update(state, -diff)

    def runValueIteration(self):
        pred = self.init_predeccessors()
        pq = util.PriorityQueue()
        self.update_pq(pq, self.mdp.getStates(), True)

        for i in range(0, self.iterations):
            if(pq.isEmpty()):
                break
            elem = pq.pop()
            newvalue = self.calculate_state(elem)
            #print("iteration :", i, "---", elem, self.values[elem], newvalue)
            self.values[elem] = newvalue
            #print("update called", elem)
            self.update_pq(pq, pred[elem], False)
            #print("after update heap:", pq.heap)
