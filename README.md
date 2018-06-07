# Simple Tabular Q-Learning in Gridworld

This repository includes several `R6` classes that implement a simple gridworld to demonstrate tabular Q-Learning. Done as an exercise for Chapter 5 and 6 of Sutton and Barto (2018).

![Agent Navigating the Gridworld](https://raw.githubusercontent.com/demirev/gridworld/master/demo.gif)

## Gridworld

The first environment is located in `gridworld.R`. This is the vanilla gridworld. It has a start line and a finish line, giving the agent a reward of $-1$ for each step it takes (until crossing the finish line).

The are a couple of controllers for this environment in `grid_walkers.R`. Each controller takes as input its position on the grid and chooses a direction of movement.

## Racetrack

Racetrack (`racetrack.R`) is similar to gridworld, but crashing into a wall resets the agent to the start line. 

Also agents build for this environment (found in `race_cars.R`) have their own persistent state (velocity). They choose the change in velocity along any of the four directions (acceleration) rather than just the end point of their move.

## Q-Learning

The agents shown in this repo employ tabular Q-Learning. That means that they store a table with each possible state (square on the grid), and the estimated Q-values (expected discounted sum of future rewards) for all possible actions at that state (move or accelerate/decelerate in the up/down/left/right direction.)

The general formula for updating the Q-value of taking an action $a$ at state $s$ is

\[
Q(a,s) = Q(s,a) + \alpha * (r(s,a) + \gamma \max_{a'}Q(s',a') - Q(s,a))
\]

Where $s'$ and $a'$ are the next state and the next action respectively.
