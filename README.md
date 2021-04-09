# Mone-Carlo-Simulation-Models

This repo has a few examples of MC simulation in R, R embedded in Shiny web app, and Python.

Newsvendor_app.R is a Shiny app that allows user to change Newsvendor model parameters and view simulation results under those changes.

Newsvendor.R is a is a "de-Shiny'd" version of Newsvendor_app.R.  It is intended to provide a simple intro (e.g., a "first model") to Monte Carlo simulation in R, and does not try to be interactive. I just run this script in RStudio; one could create a Jupyter notebook version of it if they want.

Newsvendor.ipynb is a Python/Jupter Notebook analog to Newsvendor.R; i.e., it shows how to do a basic MC simulation in Python instead of R if one prefers Python over R.

MC_Simulation_Examples_app.R is a Shiny app that includes the Newvendor on one tab, and has a Project Management simulation example on another tab.  Project Management_Example.csv is an example file that can be uploaded in the "file upload" mode of the Project Management Simulation.
Future examples can be added as tabs to this model.  
