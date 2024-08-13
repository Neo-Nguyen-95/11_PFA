#%% LOAD DATA
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

pd.set_option('display.max_columns', None)

# pfa_cluster_params = pd.read_csv('PFA_cluster_parameters.csv',
#                                  index_col='cluster')

pfa_cluster_params = pd.read_csv('PFA_cluster_parameters.csv',
                                 index_col='cluster')

# GENERATE SCENARIO
table_scenario = pd.DataFrame()

def generate_scenario(df):
    for cluster in pfa_cluster_params.index:
        for fail_number in range(4):
            table = pd.DataFrame({
                'success_opp': list(range(6)),
                'fail_opp': [fail_number] * len(range(6)),
                'cluster': [cluster] * len(range(6))
                })
            df = pd.concat([df, table], axis='rows')
    
    df['success_fail_distance'] = df['success_opp'] - df['fail_opp']

    df['total_opp'] = df['success_opp'] + df['fail_opp']
    
    df = df.reset_index()
    df.drop(columns='index', inplace=True)
    
    return df

table_scenario = generate_scenario(table_scenario)

#%% PFA PREDICT PROB
def pfa_success_probability(df, df_coef):
    beta = (df['cluster']
            .map(lambda x: df_coef.at[x, 'skill_diff'])
            )
    
    gamma = (df['cluster']
             .map(lambda x: df_coef.at[x, 'success_coef'])
             )
    
    rho = (df['cluster']
           .map(lambda x: df_coef.at[x, 'fail_coef'])
           )
    
    logit = beta + gamma * df['success_opp'] + rho * df['fail_opp']
    
    df['success_probability'] = 1 / (1 + np.exp(-logit))
    
    return df

table_scenario = pfa_success_probability(table_scenario, pfa_cluster_params)

# plot
plt.figure(dpi=200)
sns.scatterplot(data=table_scenario,
                x='success_fail_distance', y='success_probability')

#%% DECISION MAKING
# Success prob if having correct answer at 1st time
s1 = table_scenario[table_scenario['total_opp'] == 0]
s1
"""1st time correct => prob > 0.7 => choose 0.8
"""

# Number of practice should be at least 3, fail at least 1, prob at least 0.8
s2 = table_scenario[(table_scenario['total_opp'] >= 3) &
               (table_scenario['success_probability'] >= 0.8)
               ]

s2.groupby(['cluster', 'fail_opp'])['success_opp'].min()
# success - fail distance for each cluster

# Number of practice should be at least 3, fail at least 1, prob at least 0.8
s3 = table_scenario[(table_scenario['fail_opp'] == 3) &
               (table_scenario['total_opp'] == 3)
               ]
s3



