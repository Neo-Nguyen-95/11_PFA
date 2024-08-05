#%% LIBRARY & IMPORT
import pandas as pd
pd.set_option("display.max_columns", None)
import numpy as np

df_practice = pd.read_csv('clean_data.csv')
df_coef = pd.read_csv('model_summary_python.csv')

#%% ANALYSIS
#%% count numbers of skill practice 
student_skil_count = (df_practice['Skill']
                      .groupby(df_practice['Student_ID'])
                      .value_counts()
                      .reset_index())

skill_practice_max = (student_skil_count['count']
                      .groupby(student_skil_count['Skill']).max())
skill_practice_mean = (student_skil_count['count']
                       .groupby(student_skil_count['Skill']).mean())

#%% calculate prob of correct
# C(Student_ID)[1]
# C(Skill)[T.circle-circumference]
# Opportunity:C(Skill)[circle-area]

def p_student_skill(student_id, skill_name, opportunity):
    df = df_coef.set_index('Variable')
    
    theta = df.loc['C(Student_ID)[' + str(student_id) + ']']['Coefficient']
    
    if skill_name == 'circle-area':
        beta = 0
    else: 
        beta = df.loc['C(Skill)[T.' + skill_name + ']']['Coefficient']
    gamma = df.loc['Opportunity:C(Skill)[' + skill_name + ']']['Coefficient']
    
    logit = theta + beta + gamma * opportunity
    prob = 1 / (1 + np.exp(-logit))
    
    return prob

p_student_skill(12, 'circle-area', 4)

#%% Error rate

opportunity_count = (df_practice.groupby(['Skill', 'Opportunity'])['Success']
                     .count()
                     .rename('Number of Observation')
                     )

opportunity_prob = (df_practice.groupby(['Skill', 'Opportunity'])['Success']
                    .mean()
                    .rename('Probability of Correctness')
                    )
error_rate = pd.concat([opportunity_count, opportunity_prob], axis=1)
error_rate = error_rate.reset_index()
error_rate['Empirical error rate'] = 1 - error_rate['Probability of Correctness']

# error rate by skill
(error_rate[error_rate['Skill'] == 'circle-area']
 .sort_values('Opportunity')
 .plot(kind='line', x='Opportunity', y='Empirical error rate')
 )

# number of observation by skill
(error_rate[error_rate['Skill'] == 'circle-area']
 .sort_values('Opportunity')
 .plot(kind='line', x='Opportunity', y='Number of Observation')
 )

# Overall error rate
error_rate.groupby('Opportunity')['Empirical error rate'].mean().plot()
