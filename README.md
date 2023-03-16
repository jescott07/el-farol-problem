# El Farol Problem
This is a repository for the implementation of an modified version of the El Farol problem. The El Farol problem is a classic problem in game theory that deals with the challenges of predicting the behavior of a group of individuals when they have to make decisions based on incomplete information.

## The classical El Farol Problem
The classical problem is based on the following scenario:

There is a bar called El Farol that is popular among a group of individuals. Every Thursday night, these individuals decide whether to go to El Farol or stay home. The decision to go to El Farol is based on their subjective evaluation of how crowded the bar will be. If they think the bar will be too crowded, they stay home, and if they think it will be too empty, they go to the bar. The problem arises when everyone has different opinions about how crowded the bar will be, and there is no way to predict the exact number of people who will go to the bar.

The El Farol problem can be modeled mathematically as a game where each individual i chooses a strategy si from a set of strategies S, where S is the set of all possible subjective evaluations of the bar's crowdedness. Let pi(si) be the probability that individual i goes to the bar given that their subjective evaluation is si. The probability pi(si) can be modeled as a decreasing function of the number of individuals who choose the same strategy:

pi(si) = 1 / (1 + exp(k * (n(si) - c)))

where k is a parameter that determines the steepness of the function, n(si) is the number of individuals who choose the strategy si, and c is a threshold value that represents the maximum number of individuals the bar can accommodate. The function pi(si) represents the probability that individual i goes to the bar when their subjective evaluation is si, given the number of individuals who have chosen the same strategy.

How to run the code?
The code is written in Python and can be run from the command line. Before running the code, make sure you have Python installed on your machine.

To run the code, follow these steps:

Clone the repository to your local machine.
Navigate to the repository directory.
Run the command python el_farol.py in the terminal.
The code will generate a graph showing the number of individuals who go to the bar over time, given different scenarios of subjective evaluations.

Contributing
Contributions to the code are always welcome. If you find a bug or have an idea for an improvement, please feel free to open an issue or submit a pull request.

License
This project is licensed under the MIT License - see the LICENSE file for details.
