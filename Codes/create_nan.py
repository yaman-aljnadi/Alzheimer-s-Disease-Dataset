import pandas as pd
import numpy as np

df = pd.read_csv("Datasets/alzheimers_disease_data.csv")

protected_cols = ["PatientID", "Age", "Gender", "EducationLevel", "Diagnosis"]

eligible_cols = [col for col in df.columns if col not in protected_cols]

n_total = df[eligible_cols].size
n_missing = int(n_total * 0.10)

rows = np.random.randint(0, len(df), n_missing)
cols = np.random.choice(eligible_cols, n_missing)

for r, c in zip(rows, cols):
    df.at[r, c] = np.nan

df.to_csv("Datasets/alzheimers_disease_data_with_missing.csv", index=False)

print(f"Inserted {n_missing} missing values into the dataset.")