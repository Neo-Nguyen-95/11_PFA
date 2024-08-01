#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)
import matplotlib.pyplot as plt

import statsmodels.formula.api as smf

df = pd.read_csv('/Users/dungnguyen/Library/CloudStorage/GoogleDrive-dzung.usth@gmail.com/My Drive/[Current work] AEGlobal/17. Team PD/Thực tập - Nguyên/Task 3/dataset.csv')

#%% EDA
# general infor
df.info()

print(f"\nNumber of student: {df['StudentID'].nunique()}")
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

#%% CLEAN

# shorten StudentID
student_list = set(df['StudentID'].values.tolist())

student_shorten_id = {}
for index, student_id in enumerate(student_list):
    student_shorten_id[student_id] = index + 1
    
df['Student_ID'] = df['StudentID'].map(student_shorten_id)
df.drop(columns='StudentID', inplace=True)

# convert to categorical data
df['Student_ID'] = df['Student_ID'].astype('category')
df['Skill'] = df['Skill'].astype('category')

# student info
student_perf = df['Success'].groupby(df['Student_ID']).mean()
student_frequency = df['Student_ID'].value_counts()
student_info = pd.concat([student_perf, student_frequency], axis='columns')

plt.figure(dpi=200)
student_info.plot(kind='scatter', y='Success', x='count')

# save clean dataset
# df.to_csv("clean_data.csv")

#%% ESTIMATE COEF
# check crosstab
crosstab_student_skill = pd.crosstab(df['Student_ID'], df['Skill'])
crosstab_student_skill.head()

formula = 'Success ~ C(Student_ID) + C(Skill) + Opportunity : C(Skill) - 1'
log_model = smf.logit(formula, data=df).fit(method = 'ncg',
                                            maxiter=100)
log_model.summary()
log_model.aic
log_model.bic


