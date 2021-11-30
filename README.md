# Bayesian Hyperparameter Optimization in the Prediction of NBA Game Outcomes

#### Author: John Oliver
#### Point of Contact: John Oliver (oliver.john@uwlax.edu)
#### Year of Origin: 2021

*** 

This repository contains code that builds a full-scale project designed to predict the outcome of NBA games. Scripts are written to scrape and clean NBA box score data from the 2017, 2018, and 2019 seasons from basketball-reference.com. Classification models are constructed within the sci-kit learn package in Python, and further subject to hyperparameter optimization.

# Repository Organization 

The repository contains the following folders and files:

- data: folder containing all data used in analysis. All subfolders have data pertaining to the 2017, 2018, and 2019 season
  * avg_data: last 15 game average data
  * cleaned_data: data that is cleaned initially after scraping
  * elo_data: data containing elo information
  * mod_data: data sets that are ready to be used for classification models
  * scraped_data: raw data scraped from online 

- data_scraper: folder containing the function to scrape data
 * data_scraper.ipynb: script that scrapes seasons' worth of box score data

- functions: folder containing functions used in cleaning and data construction process
  * data_cleaners: functions used in the initial cleaning of raw data
  * last_n_avg_build: function used to calculate last n averages. In this analysis n = 15
  * model_data_formater: function designed to format data to be used in classification models

- model_fits: folder containing all model fits
  * each ipynb file in this folder contain 1 year of data.
  * within each year, Logistic Regression, Random Forest, Support Vector Machine, and Naive Bayes are fit
  * within each year, Logistic Regression, Random Forest, Support Vector Machine are subject to hyperparameter optimization via Random Search and Bayesian Methods

- workflow.R: a script designed to show the data construction process, with the year 2019 being shown as an example 
