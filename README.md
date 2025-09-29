

# Predicting Early Readmission of Diabetic Patients

This project focuses on **predicting whether diabetic patients are readmitted to the hospital within 30 days of discharge**. The dataset includes demographic, clinical, and treatment-related variables collected during hospital encounters. The goal is to prepare and preprocess the data for building accurate machine learning models that can support healthcare providers in reducing avoidable readmissions.



## Project Objectives

* Explore the dataset to understand its structure, relationships, and challenges.
* Preprocess the data by handling missing values, categorical encoding, skewness, and outliers.
* Address class imbalance in the response variable.
* Split the data into training and testing sets for reliable model evaluation.
* Build the foundation for predictive modeling of early diabetic patient readmission.


## Dataset Overview

* **Sample size:** ~25,000 hospital encounters
* **Predictors:**

  * Demographics (age, gender, race)
  * Clinical measures (lab results, number of inpatient/outpatient visits, medications)
  * Hospitalization details (discharge disposition, length of stay, number of procedures)
* **Response variable:**

  * 3 Classes as outcomes: *Readmitted within 30 days*, class 2
                           *Readmitted more than 30 days*, class 1
                           *No readmission*, class 0

## Data Exploration

* Checked correlation among predictors to avoid redundancy.
* Identified missing values across several variables.
* Found **class imbalance** between readmitted vs non-readmitted patients.
* Detected **skewness** in continuous predictors and presence of outliers.

## Data Preprocessing

1. **Categorical Encoding**: Converted categorical features into numeric format using *one-hot encoding*.
2. **Missing Values**: Applied **k-Nearest Neighbor (k=5) imputation**, producing more reliable estimates than mean/median replacement.
3. **Skewness Reduction**: Used **Box-Cox transformation** to normalize skewed predictors.
4. **Scaling**: Standardized all predictors to ensure fair contribution across features.
5. **Outlier Handling**: Applied **Spatial Sign transformation** to reduce the influence of extreme values.


## Data Splitting

* Performed an **80/20 train-test split**.
* Used **stratified random sampling** to preserve class balance across sets.
* Created **upsampled and downsampled training sets** to address imbalance in readmission outcomes.



## Next Steps

* Implement machine learning models (Logistic Regression, Random Forest, XGBoost, Neural Networks).
* Compare performance using metrics such as **AUC, Precision, Recall, and F1-score**.
* Perform hyperparameter tuning for best results.
* Interpret feature importance to provide actionable insights for healthcare providers.


##  Repository Structure


├── data/                 # Raw and processed datasets
├── figures/            # All the figures after data processing
├── Codes/              # Python/R scripts for reproducible workflows
├── Explorted Results/              # Outputs such as plots and metrics
└── README.md             # Project overview


## Importance

Early prediction of readmission for diabetic patients can:

* Improve patient care through timely intervention.
* Reduce hospital costs by preventing avoidable readmissions.
* Assist healthcare providers in allocating resources efficiently.



## License

This project is released under the [MIT License](LICENSE).


## Contributor

**Yaman Aljnadi**
MS.C. Student | Data Science  | Michigan Technological University

**Ananna Biswas**
Ph.D. Student | Computational Science & Engineering | Michigan Technological University


