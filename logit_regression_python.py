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

# more method and attribute for fit(), check
# https://www.statsmodels.org/dev/generated/statsmodels.discrete.discrete_model.Logit.fit.html
log_model = smf.logit(formula, data=df).fit(method = 'ncg',
                                            maxiter=100)
log_model.summary()

log_model.aic
log_model.bic

#%% EXPORT COEFFICIENTS
summary = log_model.summary()
summary_as_text = summary.as_text()

# Split the summary into lines
summary_lines = summary_as_text.split('\n')

# Find the start and end of the coefficient table
start_index = summary_lines.index('-------------------------------------------------------------------------------------------------------------------') + 1
end_index = summary_lines.index('===================================================================================================================', start_index)
coef_table_lines = summary_lines[start_index:end_index]

# Prepare data for CSV
data_for_csv = []
header = ['Variable', 'Coefficient', 'Std. Error', 'z-Value', 'P-Value']
data_for_csv.append(header)

for line in coef_table_lines:
    if line.strip():  # Avoid empty lines
        parts = line.split()
        variable = parts[0]
        coefficient = parts[1]
        std_error = parts[2]
        z_value = parts[3]
        p_value = parts[4]
        data_for_csv.append([variable, coefficient, std_error, z_value, p_value])

# Create DataFrame and save to CSV
df_summary = pd.DataFrame(data_for_csv[1:], columns=data_for_csv[0])
df_summary.to_csv('model_summary_python.csv', index=False)

print("Summary saved to 'model_summary.csv'")




