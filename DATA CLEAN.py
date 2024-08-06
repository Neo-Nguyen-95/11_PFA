#%% LIB & IMPORT
import pandas as pd
pd.set_option('display.max_columns', None)

df = pd.read_csv('data_original.csv')
df = df[['Anon Student Id', 'Problem Name', 'Step Name', 'First Attempt', 
        'KC (Original)', 'Opportunity (Original)', 'Predicted Error Rate (Original)']]

#%% CLEANING
def clean_data(df):
    """Clean and preprocess the dataframe"""
    # shorten student ID
    df['Student_ID'] = df['Anon Student Id'].astype('category').cat.codes +1
    
    # map success values
    df['Success'] = df['First Attempt'].map({'correct': 1, 'incorrect': 0})
    
    # rename columns
    df.rename(columns={
        'KC (Original)' : 'Skill',
        'Opportunity (Original)': 'Opportunity',
        'Problem Name': 'Problem',
        'Step Name': 'Step'
        }, inplace=True)

    df.drop(columns = ['Anon Student Id', 'First Attempt'], inplace=True)
    
    return df

df = clean_data(df)
    
    
#%% ADD SUCCESS_ & FAILURE_OPPORTUNITY

def calculate_opportunities(df):
    df['Calculated Opportunity'] = df.groupby(['Student_ID', 'Skill']).cumcount() + 1
    df['Success_opportunity'] = df.groupby(['Student_ID', 'Skill'])['Success'].cumsum()
    df['Fail_opportunity'] = df['Opportunity'] - df['Success_opportunity']
    
    return df

df = calculate_opportunities(df)

# Verify
verification = (df['Opportunity'] - df['Calculated Opportunity']).sum()
print("The difference between author's Opportunity and mine is: ", verification)

#%% EXPORT
df.to_csv("data_clean_update.csv")
