# rich-master-chef

## Introduction

We find that cooking is quite fun. So, we get our inspiration from food booths (such as taco booths, sushi shops) for our project.

This is a restaurant simulation game based on the brick library to provide users with an immerse and interesting cooking experience. Players are provided an initial budget for a day, which they can use to purchase different ingredients (for dishes). After that, they will use ingredients to cook dishes based on their recipes. Different recipes (a.k.a., combinations of ingredients) consume different time and could sell at different prices. Player who earns the most at the end of several days wins the game. Players must maximize the utilization of their budget and make the best profit by using the least amount of time. At the same time, players have to make sure they are taking care of what’s happening in the kitchen, because mistakes happen at times. In this case, players need to handle these events in a timely manner.

## Overview

- The cook has three day limits to run the restaurant
- Budget per day, grows by the profit made in the previous day if wins the game
- Different ingredients & their prices
- Recipe processing costs & their sale prices
- Earnings for the day

### Key Components

- Kitchen: game board
- Date: indicates earnings and game board status for the current date
- Budget: budget allocated each day, and operations on the budget
- Earning: earnings obtained each day so far
- Player: status of the player
  
## Instructions

- The master chef has 3 days to run this restuarant
- There are 2 recipes for now: Soup (Dish A) and Sashimi (Dish B)
- How to cook soup: You should always prepare the food before cooking. Then, cook it 2 times and season it. Tadaaa, the soup is ready to be served!
- How to cook Sashimi: For Sashimi, you don't really need to *cook* it. So, you need to prepare it first. Then, season it and serve it :)
- Other than the recipes, you, as the master chef, can do following things:
- Prepare gradients (-20 budget)
- Season (-5 budget)
- Cook (-10 budget)
- Serve (-5 budget, +100 earning)
- Clean up mistakes (-10 budget)
- The game ends when *remaining days* turns 0. Then your earning will be recorded (if it is max)
- The remaining budget of a day will be saved for the next day

### Challenges

- Design of the architecture of the project

  - Group meetings frequently
  - Doing research for inspiration

- Implementation: In progress

### We expect to meet our goals until the deadline
