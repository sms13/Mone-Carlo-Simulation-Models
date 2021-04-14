# Mone-Carlo-Simulation-Models

This repo contains some introductory examples of MC simulation in both R and Python. It introduces two application examples: 1) the classic Newsvendor problem, and 2) a project planning example.  For the R examples, there are examples of these models as a "regular" R scripts (.R file), an R Markdown script (.rmd file), and R embedded in a Shiny web app (.R).  The Python examples are given in the form of Jupyter notebooks (.ipynb). 

Newsvendor_app.R is an R/Shiny app that allows user to change Newsvendor model parameters and view simulation results under those changes.

Newsvendor.R is a "de-Shiny'd" version of Newsvendor_app.R.  It is intended to be an example of a traditional coding approach to run and analyze an MC simulation in R, where comments are embedded in the code and one might use copy results/figures/etc. from the code output to a separate document, presentation, etc.  As opposed to the storytelling alongside the analyis/code kind of approach typically taken with notebooks, which brings us to...

Newsvendor_RMD.rmd is an R Markdown (essentially an R notebook) approach to the newsvendor problem.  This is more representative of how one might develop a story, intersperse some code chunks and code outputs for displaying in the report, hiding others for viewing in the .rmd file only, etc.  One can also create a Jupyter notebook version of this R code if they prefer that environment for the notebook-style approach (I have not created a Jupyter version of the R code).  Newsvendor_RMD.html is the file that is created afer "Knitting" the .rmd file.

For those who prefer Python to R, Newsvendor.ipynb is a Python/Jupter Notebook version of the Newsvendor problem.

MC_Simulation_Examples_app.R is a Shiny app that includes both the Newvendor example on one tab, and the Project Management simulation example on another tab.  Project Management_Example.csv is an example file that can be uploaded in the "file upload" mode of the Project Management Simulation.  A published version of this Shiny app is here: https://sms13.shinyapps.io/MC_Examples/

 
