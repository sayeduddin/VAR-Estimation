# VAR Estimation
Estimating VAR of a portfolio of two stock indices using copula theory and Monte Carlo simulation.

## VAR Estimation Report
This is the final report written for the project, outlining key challenges and decomposing how the overall project was navigated. 

## FTSE and FCHI data
Excel file containing data regarding the performance of all the different iterations of models that were checked. The data is sorted by BIC value. Columns that are green signify that they have passed the corresponding test according to the significance levels that were set. A row of 4 greens signifies that the model has passed all tests and so is a suitable candidate for use in the final VAR estimation. The tests that were used are as follows: 
-Ljung–Box test (autocorrelation)
-Kolmogorov–Smirnov test (normal)
-Anderson–Darling test (normal)

## VAR Estimation Markdown
Contains all code used to write the final report. 

## FTSE/FCHI Model Tests
Contains all the tests use to find the optimal model for FTSE/FCHI. The code was generated using Python since the process of manually changing the models was not time efficient and I was unable to find an appropriate method of iteration in R in the time that was available. 

## Code Gen
Python code used to generate R code for thousands of iterations of models that needed to be checked. 


