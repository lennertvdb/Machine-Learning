# ml21-group08

## Project Description
Prediction on loan default is essential for banks because it has a strong impact on their profitability. Gaining insight in the factors that play a role in these defaults could enable decision makers to change their loan offers. For example, if an applicant is likely to default the bank could simply deny the loan, reduce the amount or charge a higher interest. 

So we basically want to build a model that can predict default based on information about the potential borrower. 

## Project Structure 
### Modelling framework
We created an internal and an external way of testing. The internal way was created by splitting the training csv in 80% train set and 20% validation set. This was done in the “BasetableCreation_Splitting.R” file. When we were happy with our internal AUC, we used the model to perform external testing. For the external testing, we had to run our data preprocessing again but on the whole training csv, to optimize the training ability of our model. To switch efficiently between the two ways of testing, we implemented both code possibilities and used commenting.

### CRISP-DM
We let us guide by the Cross Industry Standard Process for Data Mining (CRISP-DM) process model. So we went through the following steps: 
1. Business Understanding
2. Data Understanding
3. Data Preparation
4. Modelling
5. Evaluation and deployment

### Folder structure 
|-- README 
|-- data 
|  |--1_bronze
|  |--2_silver
|  |--3_gold
|  |--results
|-- doc
|  |--technical_report.pdf
|-- src
|  |--BasetableCreation_Splitting
|  |--DataPreprocessing
|  |--Deployment_and_metrics
|  |--Feature_selection_ridge_classification
|  |--LGB
|  |--Logistic_Regression
|  |--RandomForest_Tuning
|  |--Treebased_Methods

Our data folder contains 3 sublevels. 
    1_bronze: raw data
    2_silver: selected features
    3_gold: fully cleaned data ready to deploy 

Our technical report can be found in the doc folder.

Our code can be found in the src folder. Please notice our scripts can be divided in the steps from the CRISP-DM model. 

## Programming Language 
We made use of the programming language 'R'.

## Futher Contributions
We experimented with the follwing modelling methods: Logistic Regression, Random Forest & Light Gradient Boosting

Additional methods such as neural networks could be used too for this classification problem. 







