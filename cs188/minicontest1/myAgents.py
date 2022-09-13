# myAgents.py
# ---------------
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

from game import Agent, Directions
from searchProblems import PositionSearchProblem

import util
import time
import search

"""
IMPORTANT
`agent` defines which agent you will use. By default, it is set to ClosestDotAgent,
but when you're ready to test your own agent, replace it with MyAgent
"""


def createAgents(num_pacmen, agent='MyAgent'):
    return [eval(agent)(index=i) for i in range(num_pacmen)]


def breadthFirstSearch(problem):
    start_state = problem.getStartState()
    data = util.Queue()
    data.push((start_state, []))
    visited = set()
    while not data.isEmpty():
        (state, actions) = data.pop()
        if state in visited:
            continue
        visited.add(state)
        if(problem.isGoalState(state)):
            return (actions, state)
        for (child, action, stepCost) in problem.getSuccessors(state):

            newactions = list(actions)
            newactions.append(action)
            data.push((child, newactions))
    return ([], (1, 1))


class MyFoodSearchProblem(PositionSearchProblem):
    def __init__(self, list, gameState, agentIndex):
        self.list = list
        self.food = gameState.getFood()
        self.walls = gameState.getWalls()
        self.startState = gameState.getPacmanPosition(agentIndex)
        self.costFn = lambda x: 1
        self._visited, self._visitedlist, self._expanded = {}, [], 0  # DO NOT CHANGE

    def isGoalState(self, state):
        x, y = state

        return state not in self.list and self.food[x][y]


class MyAgent(Agent):
    target = []

    def getAction(self, state):
        if self.index == 0:
            MyAgent.target.clear()
        problem = MyFoodSearchProblem(self.target, state, self.index)
        (actions, state) = breadthFirstSearch(problem)
        self.target.append(state)
        if(len(actions) == 0):
            return Directions.STOP
        return actions[0]

    def initialize(self):
        MyAgent.target = []
        return


"""
Put any other SearchProblems or search methods below. You may also import classes/methods in
search.py and searchProblems.py. (ClosestDotAgent as an example below)
"""


class ClosestDotAgent(Agent):

    def findPathToClosestDot(self, gameState):
        """
        Returns a path (a list of actions) to the closest dot, starting from
        gameState.
        """
        # Here are some useful elements of the startState
        startPosition = gameState.getPacmanPosition(self.index)
        food = gameState.getFood()
        walls = gameState.getWalls()
        problem = AnyFoodSearchProblem(gameState, self.index)
        return search.bfs(problem)

    def getAction(self, state):
        return self.findPathToClosestDot(state)[0]


class AnyFoodSearchProblem(PositionSearchProblem):
    def __init__(self, gameState, agentIndex):
        self.food = gameState.getFood()

        # Store info for the PositionSearchProblem (no need to change this)
        self.walls = gameState.getWalls()
        self.startState = gameState.getPacmanPosition(agentIndex)
        self.costFn = lambda x: 1
        self._visited, self._visitedlist, self._expanded = {}, [], 0  # DO NOT CHANGE

    def isGoalState(self, state):
        x, y = state
        return self.food[x][y]
