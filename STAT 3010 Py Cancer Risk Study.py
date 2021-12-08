import pandas as pd
import numpy as np

data = pd.read_csv(r'C:\Users\Slaye\Documents\R STAT 3010 Projects\STAT 3010 Cancer Risk Dataset.csv')

# Question 1
## Table 1: Descriptive Statistics for Age, Calories, and Fat
print(round(data['Age'].describe(),2))
print(round(data['Calories'].describe(),2))
print(round(data['Fat'].describe(),2))

### Function to receive interquartile range for each variable
def IQR(variable):
    q3, q1 = np.percentile(data[variable], [75 ,25])
    iqr = q3 - q1
    iqr = round(iqr,2)
    return (iqr)

### Attached interquartile range to descriptive statistics
IQR('Age')
IQR('Calories')
IQR('Fat')

