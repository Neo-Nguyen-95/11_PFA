#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)

import statsmodels.formula.api as smf

df = pd.read_csv('data_clean_update.csv')

#%% EDA
# general infor
df.info()

print(f"\nNumber of student: {df['Student_ID'].nunique()}")
print(f"\nNumber of problem: {df['Problem'].nunique()}")
print(f"\nNumber of skills: {df['Skill'].nunique()}")

# problem & step list
problem_list = df.groupby(['Problem'])['Step'].nunique().reset_index()

step_list = (df.groupby(['Problem', 'Step'])['Success'].mean()
             .reset_index()
             .drop(columns='Success')
             )

# skill list
skill_list = df['Skill'].value_counts()

# convert to categorical data
df['Student_ID'] = df['Student_ID'].astype('category')
df['Skill'] = df['Skill'].astype('category')

# student info
student_perf = df['Success'].groupby(df['Student_ID']).mean()
student_frequency = df['Student_ID'].value_counts()
student_info = pd.concat([student_perf, student_frequency], axis='columns')

student_info.plot(kind='scatter', y='Success', x='count')

# student practice info
student_skil_count = df['Skill'].groupby(df['Student_ID']).value_counts()


#%% ESTIMATE COEF
# check crosstab
crosstab_student_skill = pd.crosstab(df['Student_ID'], df['Skill'])
crosstab_student_skill.head()

formula = 'Success ~ C(Student_ID) + C(Skill) + Opportunity:C(Skill) -1'
"""
The -1 in the formula doesn't really cancel the intercept, but force it to 0
Changing the order of variable in the equation leads to change in reference value
For the above model, ref value is Student ID No.1 = 0
"""

# more method and attribute for fit(), check
# https://www.statsmodels.org/dev/generated/statsmodels.discrete.discrete_model.Logit.fit.html
log_model = smf.logit(formula, data=df).fit(model='ncg', maxiter=100)
log_model.summary()

log_model.aic
log_model.bic

#%% EXPORT COEFFICIENTS
summary = log_model.summary()
summary_as_text = summary.as_text()

with open('model_summary_python_raw.txt', 'w') as file:
    file.write(summary_as_text)


