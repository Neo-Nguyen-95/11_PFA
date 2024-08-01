#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)

df = pd.read_csv('/Users/dungnguyen/Library/CloudStorage/GoogleDrive-dzung.usth@gmail.com/My Drive/[Current work] AEGlobal/17. Team PD/Thực tập - Nguyên/Task 3/dataset.csv')

# shorten StudentID
student_list = set(df['StudentID'].values.tolist())

student_shorten_id = {}
for index, student_id in enumerate(student_list):
    student_shorten_id[student_id] = index + 1
    
df['Student_ID'] = df['StudentID'].map(student_shorten_id)
df.drop(columns='StudentID', inplace=True)

# save clean dataset
df.to_csv("clean_data.csv")