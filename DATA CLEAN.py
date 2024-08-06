#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)

df = pd.read_csv('data_original.csv')
df = df[['Anon Student Id', 'Problem Name', 'Step Name', 'First Attempt', 
        'KC (Original)', 'Opportunity (Original)', 'Predicted Error Rate (Original)']]

#%% CLEANING
# shorten StudentID
student_list = set(df['Anon Student Id'].values.tolist())

student_shorten_id = {}
for index, student_id in enumerate(student_list):
    student_shorten_id[student_id] = index + 1
    
df['Student_ID'] = df['Anon Student Id'].map(student_shorten_id)
df.drop(columns='Anon Student Id', inplace=True)

df['Success'] = df['First Attempt'].map({'correct': 1, 'incorrect': 0})
df['Skill'] = df['KC (Original)']  # rename
df['Opportunity'] = df['Opportunity (Original)']  # rename
df['Problem'] = df['Problem Name']  # rename
df['Step'] = df['Step Name']

df.drop(columns = ['First Attempt', 'KC (Original)', 'Opportunity (Original)',
                   'Problem Name', 'Step Name'],
        inplace=True)

#%% ADD SUCCESS_ & FAILURE_OPPORTUNITY

# STEP 1: Verify opportunity calculation
df_count = df[['Student_ID', 'Skill']]

my_opportunity = []

for i in range(len(df_count)):
    skill_name = df_count.iloc[0:i+1]['Skill'][i]
    student_id = df_count.iloc[0:i+1]['Student_ID'][i]
    oppor = df_count.iloc[0:i+1].value_counts().loc[(student_id, skill_name)]
    my_opportunity.append(oppor)
    
my_opportunity = pd.Series(my_opportunity)

# STEP 2: Calculate Success and Failure opportunity
df_count = df[['Student_ID', 'Skill', 'Success']]

i = 4
skill_name = df_count.iloc[0:i+1]['Skill'][i]
student_id = df_count.iloc[0:i+1]['Student_ID'][i]
df_temp = df_count.iloc[0:i+1]
df_temp[df_temp['Skill'] == skill_name]['Success'].sum()


#%% EXPORT
# df.to_csv("data_clean_update.csv")
