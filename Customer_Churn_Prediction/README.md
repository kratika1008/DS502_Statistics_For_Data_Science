# Telecommunication Company Customer Churn Analysis

## Goal:
- Identify factors leading to churn
- Recommendations to companies to retain customers

## Dataset:
Kaggle dataset: https://www.kaggle.com/blastchar/telco-customer-churn

## Data Pre-processing:
- Removed null & duplicate records
- Converted Categorical features to Numerical
- Corrected dataTypes of some columns

## Data Exploration:
- Identified ratio of Churn : Not Churn customer records (found that the data is skewed)
- Analyze the effect of various features on the churn and removed features that have no effect on customer churn
- Built correlation among various features and removed redundant information

## Data Oversampling:
- Since the data is highly skewed, over-sampled data using SMOTE

## Train/Test:
- Divided data into 10 folds using Stratified k-folds
- Trained various Machine Learning models and compared their results
	- Ordinary Least Squares
	- LASSO
	- Random Forest
	- kNN
	- Support Vector Classifier
	- XGBoost
	- Logistic Regression
	- Linear Discriminant Analysis (LDA)
	- Quadratic Discriminant Analysis (QDA)
	- Ensemble Model
- Identified most important features impacting the Customer churn

## Result:
- Linear models performed really well as the data showed a more simpler behaviour
- Many models showed similar results with XGBoost and Ensemble model providing the highest result
- Month-to-month Contracts, Longer Tenure, Higher Total Charges, No Online Security, No Technical Support are the main reason behind customer churn (as repetedly pointed by various algorithms)