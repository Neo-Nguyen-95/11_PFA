#%% LOAD DATA
import pandas as pd
import numpy as np

pd.set_option('display.max_column', None)

bkt_coef = pd.read_csv('/Users/dungnguyen/Desktop/Data Science off/Python Programming/1. Work Project/11_PFA/1. Parameter Estimation/BKT_summary_kc.csv')

pfa_coef = pd.read_csv('/Users/dungnguyen/Desktop/Data Science off/Python Programming/1. Work Project/11_PFA/1. Parameter Estimation/PFA_summary_python.csv')

#%% TRANSFORM
def bkt_transform(df):
    result = pd.DataFrame(
        {'skill': df['skill'].unique(),
         'prior': df[df['param']=='prior']['value'].values,
         'learns': df[df['param']=='learns']['value'].values,
         'guesses': df[df['param']=='guesses']['value'].values,
         'slips': df[df['param']=='slips']['value'].values
         }
        )
    return result

bkt_coef_update = bkt_transform(bkt_coef)
    
def pfa_transform(df):
    result = ...
    
    return result

df_skill = pfa_coef.iloc[0:15]
df_success = pfa_coef.iloc[15:30]
df_fail = pfa_coef.iloc[30:45]
        
