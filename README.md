# PHP 1560/2560 Final Project README

# Link to ShinyApp: https://xpp2zj-jenny-wei.shinyapps.io/project-files/


# Overview: 
Our project aims to simulate hospital bed occupancy for a given population over the course of 100 days. Our model takes in parameters including population size, female to male ratio, number of beds available (1), and community-level income. These values are used in 'proj_simulation.R' to calculate the daily hospital admission rate and patients' length of stay based on demographic-specific means drawn from data reported in the Health Care Utilization Project (2). Taking the average values for admission rate and length of stay, we generate several Poisson distributions to simulate hospital bed occupancy, as well as calculate corresponding patient wait times and hospital losses to illustrate scenarios when the hospital is under or over capacity. 


In our 'app.R' file, four dynamic visualizations are produced: the first figure illustrates the individuals currently using a bed compared to those who are waiting, the second figure illustrates average financial losses for being under/over capacity, the third figure graphs the distribution of an admitted patient's length of stay, and the fourth figure graphs the distribution of wait times for the patients in the simulation.


Inspiration for this project came from 'discrete simulation' models that are common in Health Care resource allocation (3)(4). This project simplifies the complexity of the simulation by only accounting for beds as the resource.

# Code Structure:
The code has is generally divided into shiny app and simulation code. The simulation code deals with the underlying logic and runs the simulation, while the shiny app takes in data and displays it visually. Simulation code is written in separate files but imported using the 'source' command so that it is accessible in the shiny app and can produce dynamic simulations based on how the user inputs specific parameter values.

# References:
(1) https://www.cdc.gov/nchs/data/hus/2020-2021/BedComSt.pdf

(2) https://www.hcup-us.ahrq.gov/reports/statbriefs/sb246-Geographic-Variation-Hospital-Stays.pdf

(3) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8625660/

(4) https://www.codeproject.com/Articles/1111093/Discrete-Event-Simulation-using-R-Hospital-Capacit
