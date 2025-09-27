Step 1. Explore Data
•	What is the goal of the study?
•	Data structure: 
1.	sample size? 
2.	Number of predictors? 
3.	Number of response variables?
4.	Type of data: categorical vs. continuous vs others
5.	Balanced or unbalanced?
6.	Missing values? 

Step 2. Handle Missingness
•	Imputation using KNN 

Step 3. Encode Predictors
•	Converting from categorical to numerical predictors using one hot encoding

Step 4. Normalize & Transform
•	Apply Box-Cox transformation to make distributions more normal (helps linear models, PCA, etc.)
•	Center and scale predictors (essential before PCA or distance-based models)

Step 5. Outlier Handling
•	Apply spatial sign transformation if outliers are an issue




