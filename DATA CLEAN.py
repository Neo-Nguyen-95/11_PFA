#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)

df = pd.read_csv('data_original.csv')
df = df[['Anon Student Id', 'Problem Name', 'Step Name', 'First Attempt', 
        'KC (Original)', 'Opportunity (Original)', 'Predicted Error Rate (Original)']]

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

# save clean dataset
df.to_csv("data_clean_update.csv")
