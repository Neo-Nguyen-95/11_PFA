#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)

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
