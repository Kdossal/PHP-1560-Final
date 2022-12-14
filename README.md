# PHP-1560-Final

# Purpose: 
Our project aims to simulate hosptial bed occupancy for a given population over the course of 100 days. The parameters that our model takes in includes the population size, the female to male ratio, the number of beds initially available, and the community-level income. Each of these parameters contribute have a specific distribution for their length of stay, which we modelled as a Poisson distribution with means that were taken from Health Care Utilization Project data sources. Four dynamic visualizations are produced: the first figure illustrates the individuals currently using a bed compared to those who are waiting, the second figure illustrates average financial losses for being under/over capacity, the third figure graphs the distribution of an admitted patient's length of stay, and the fourth figure graphs the distribution of wait times for the patients in the simulation.

# Code Structure:
The code has is generally divided into shiny app and simulation code. The simulation code deals with the underlying logic and runs the simulation, while the shiny app takes in data and displays it visually. Simulation code is written in seperate files but imported using the 'source' command so that it is accessible in the shiny app and can produce dynamic simulations based on how the user inputs specific parameter values.

# References:
1) https://www.cdc.gov/nchs/data/hus/2020-2021/BedComSt.pdf
(2) https://www.hcup-us.ahrq.gov/reports/statbriefs/sb246-Geographic-Variation-Hospital-Stays.pdf
(3) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8625660/
(4) https://www.codeproject.com/Articles/1111093/Discrete-Event-Simulation-using-R-Hospital-Capacit