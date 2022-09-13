# multiAgents.py
# --------------
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


from util import manhattanDistance
from game import Actions, Directions
import random
import util

from game import Agent


class ReflexAgent(Agent):
    def getAction(self, gameState):
        """
        You do not need to change this method, but you're welcome to.

        getAction chooses among the best options according to the evaluation function.

        Just like in the previous project, getAction takes a GameState and returns
        some Directions.X for some X in the set {NORTH, SOUTH, WEST, EAST, STOP}
        """
        # Collect legal moves and successor states
        legalMoves = gameState.getLegalActions()

        # Choose one of the best actions
        scores = [self.evaluationFunction(
            gameState, action) for action in legalMoves]
        bestScore = max(scores)
        bestIndices = [index for index in range(
            len(scores)) if scores[index] == bestScore]
        # Pick randomly among the best
        chosenIndex = random.choice(bestIndices)

        return legalMoves[chosenIndex]

    def evaluationFunction(self, currentGameState, action):
        """
        Design a better evaluation function here.

        The evaluation function takes in the current and proposed successor
        GameStates (pacman.py) and returns a number, where higher numbers are better.

        The code below extracts some useful information from the state, like the
        remaining food (newFood) and Pacman position after moving (newPos).
        newScaredTimes holds the number of moves that each ghost will remain
        scared because of Pacman having eaten a power pellet.

        Print out these variables to see what you're getting, then combine them
        to create a masterful evaluation function.
        """
        # Useful information you can extract from a GameState (pacman.py)
        successorGameState = currentGameState.generatePacmanSuccessor(action)
        newPos = successorGameState.getPacmanPosition()
        newFood = successorGameState.getFood()
        newGhostStates = successorGameState.getGhostStates()
        newScaredTimes = [
            ghostState.scaredTimer for ghostState in newGhostStates]

        eats = currentGameState.getFood()[newPos[0]][newPos[1]] * 50
        fooddistances = [manhattanDistance(
            f, newPos) for f in newFood.asList()]

        if(len(fooddistances) == 0):
            mindistance = 0
        else:
            mindistance = min(fooddistances)

        return successorGameState.getScore() - 3 * mindistance + eats


def scoreEvaluationFunction(currentGameState):
    """
    This default evaluation function just returns the score of the state.
    The score is the same one displayed in the Pacman GUI.

    This evaluation function is meant for use with adversarial search agents
    (not reflex agents).
    """
    return currentGameState.getScore()


class MultiAgentSearchAgent(Agent):
    """
    This class provides some common elements to all of your
    multi-agent searchers.  Any methods defined here will be available
    to the MinimaxPacmanAgent, AlphaBetaPacmanAgent & ExpectimaxPacmanAgent.

    You *do not* need to make any changes here, but you can if you want to
    add functionality to all your adversarial search agents.  Please do not
    remove anything, however.

    Note: this is an abstract class: one that should not be instantiated.  It's
    only partially specified, and designed to be extended.  Agent (game.py)
    is another abstract class.
    """

    def __init__(self, evalFn='scoreEvaluationFunction', depth='2'):
        self.index = 0  # Pacman is always agent index 0
        self.evaluationFunction = util.lookup(evalFn, globals())
        self.depth = int(depth)


class MinimaxAgent(MultiAgentSearchAgent):
    """
    Your minimax agent (question 2)
    """
    # return (score, action)

    def iterate(self, gameState, agents):
        #print(agents, is_max)
        (agent, is_max) = agents[0]
        if(len(agents) == 1):
            return (self.evaluationFunction(gameState), None)
        if(gameState.isWin() or gameState.isLose()):
            return (gameState.getScore(), None)

        scores = []
        for action in gameState.getLegalActions(agent):
            newstate = gameState.generateSuccessor(agent, action)
            (score, _) = self.iterate(newstate, agents[1:])
            scores.append((score, action))

        # if(len(scores) == 0):
        #    (0, None)

        if is_max:
            x = max(scores, key=(lambda a: a[0]))
        else:
            x = min(scores, key=(lambda a: a[0]))

        return x

    def getAction(self, gameState):
        # print(gameState.getNumAgents())
        agents = []
        for x in range(0, (self.depth)):
            agents.append((0, True))

            for agent in range(1, gameState.getNumAgents()):
                agents.append((agent, False))
        agents.append((0, True))
        (score, action) = self.iterate(gameState, agents)
        return action


class AlphaBetaAgent(MultiAgentSearchAgent):
    def iterate(self, gameState, agents, min_val, max_val):
        #print(agents, is_max)
        (agent, is_max) = agents[0]
        if(len(agents) == 1):
            return (self.evaluationFunction(gameState), None)
        if(gameState.isWin() or gameState.isLose()):
            return (gameState.getScore(), None)

        scores = []
        for action in gameState.getLegalActions(agent):
            newstate = gameState.generateSuccessor(agent, action)
            (score, _) = self.iterate(newstate, agents[1:], min_val, max_val)
            if not is_max and max_val > score:
                return (score, action)
            elif is_max and min_val < score:
                return (score, action)
            elif not is_max and score < min_val:
                min_val = score
            elif is_max and score > max_val:
                max_val = score
            scores.append((score, action))

        if is_max:
            x = max(scores, key=(lambda a: a[0]))
        else:
            x = min(scores, key=(lambda a: a[0]))

        return x

    def getAction(self, gameState):
        # print(gameState.getNumAgents())
        agents = []
        for x in range(0, (self.depth)):
            agents.append((0, True))
            for agent in range(1, gameState.getNumAgents()):
                agents.append((agent, False))

        agents.append((0, True))
        (score, action) = self.iterate(gameState, agents, 100000, -100000)
        return action


class ExpectimaxAgent(MultiAgentSearchAgent):
    def iterate(self, gameState, agents):
        #print(agents, is_max)
        (agent, is_max) = agents[0]
        if(len(agents) == 1):
            return (self.evaluationFunction(gameState), None)
        if(gameState.isWin() or gameState.isLose()):
            return (gameState.getScore(), None)

        scores = []
        for action in gameState.getLegalActions(agent):
            newstate = gameState.generateSuccessor(agent, action)
            (score, _) = self.iterate(newstate, agents[1:])
            scores.append((score, action))

        if is_max:
            x = max(scores, key=(lambda a: a[0]))
        else:
            x = (sum([s[0] for s in scores]) / len(scores), None)
        return x

    def getAction(self, gameState):
        agents = []
        for x in range(0, (self.depth)):
            agents.append((0, True))
            for agent in range(1, gameState.getNumAgents()):
                agents.append((agent, False))

        agents.append((0, True))
        (score, action) = self.iterate(gameState, agents)
        return action

# it passes the test so i won't improve it further
def betterEvaluationFunction(currentGameState):
    pos = currentGameState.getPacmanPosition()
    foods = currentGameState.getFood()
    ghosts = currentGameState.getGhostStates()
    fooddistances = [manhattanDistance(
        f, pos) for f in foods.asList()]
    ghostdistances = [manhattanDistance(
        g.getPosition(), pos) for g in ghosts]
    minghostdistance = min(ghostdistances)

    return currentGameState.getScore() - len(fooddistances) * 5 - minghostdistance * 6

    # return scoreEvaluationFunction(currentGameState)


# Abbreviation
better = betterEvaluationFunction
