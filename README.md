# Seoul Bike Rental Prediction Project

---

## Project Objective
This machine learning project aims to predict hourly bike rentals in Seoul. The 
primary objective is to develop a model with close or better accuracy 
(measured by Mean Absolute Error - MAE and Root Mean Squared Error - RMSE) than the 
models presented in [this](https://www.tandfonline.com/doi/full/10.1080/22797254.2020.1725789) research paper that accompanies the dataset. Full article can be found [here](https://lokwudishu.com/blog/bike_rental_prediction/).

## Project Outcome
After conducting 2 rounds of hyper-parameter tuning, the XGBOOST model outperformed 
all other models, including the best-performing model in the research paper, in terms of RMSE.
The developed model achieved a lower RMSE, indicating improved prediction accuracy. Read 
full analysis write up on Medium. 


## Data Source 
The dataset used in this project is sourced from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand). It contains hourly data on bike sharing demand in Seoul, including various features such as weather conditions, date-time information, and holiday indicators.

## Models Explored
Models Explored
Throughout the project, several machine learning models were explored to predict bike rentals. The three primary models investigated are:

* Random Forest
* XGBOOST
* Cubist

## Reproducibility
The project was implemented using the R programming language and the tidymodels framework, which provides a consistent and streamlined workflow for machine learning in R. The following packages were utilized. For a list of packages and package versions, see [this]() file. 
