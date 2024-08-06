#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)

import statsmodels.formula.api as smf

# load data
df = pd.read_csv('data_clean_update.csv')

#%% EDA
# general infor
def print_basic_info(df):
    df.info()
    print(f"\nNumber of student: {df['Student_ID'].nunique()}")
    print(f"\nNumber of problem: {df['Problem'].nunique()}")
    print(f"\nNumber of skills: {df['Skill'].nunique()}")

# skill list
def get_skill_list(df):
    return df['Skill'].value_counts()

# convert to categorical data
def convert_categorical(df):
    df['Student_ID'] = df['Student_ID'].astype('category')
    df['Skill'] = df['Skill'].astype('category')
    return df

df = convert_categorical(df)

#%% ESTIMATE COEF
# check crosstab
def fit_logistic_regression(df):
    """
    The -1 in the formula doesn't really cancel the intercept, but force it to 0
    Changing the order of variable in the equation leads to change in reference value
    For the below model, ref value is Student ID No.1 = 0
    """
    formula = 'Success ~ C(Student_ID) + C(Skill) + Opportunity:C(Skill) -1'
    
    # more method and attribute for fit(), check
    # https://www.statsmodels.org/dev/generated/statsmodels.discrete.discrete_model.Logit.fit.html
    log_model = smf.logit(formula, data=df).fit(model='ncg', maxiter=100)
    
    return log_model

log_model = fit_logistic_regression(df)
print(log_model.summary())
print("AIC: ", log_model.aic)
print("BIC: ", log_model.bic)

#%% EXPORT COEFFICIENTS
summary = log_model.summary()
summary_as_text = summary.as_text()

with open('model_summary_python_raw.txt', 'w') as file:
    file.write(summary_as_text)


