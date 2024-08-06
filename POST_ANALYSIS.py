#%% I. LIBRARY & IMPORT
import pandas as pd
pd.set_option("display.max_columns", None)
import numpy as np

import matplotlib.pyplot as plt

df_practice = pd.read_csv('data_clean_update.csv')
df_coef = pd.read_csv('model_summary_python.csv')

#%% II. ANALYSIS
#%% 1. SKILL COUNT
student_skil_count = (df_practice['Skill']
                      .groupby(df_practice['Student_ID'])
                      .value_counts()
                      .reset_index())

skill_practice_max = (student_skil_count['count']
                      .groupby(student_skil_count['Skill']).max())
skill_practice_mean = (student_skil_count['count']
                       .groupby(student_skil_count['Skill']).mean())

#%% 2. PREDICTED PROB(CORRECT)
# Data format:
# - C(Student_ID)[1]
# - C(Skill)[T.ALT:CIRCLE-CIRCUMFERENCE]
# - Opportunity:C(Skill)[ALT:CIRCLE-AREA]

# prob for each student - skill - opportunity
def p_student_skill(student_id, skill_name, opportunity):
    df = df_coef.set_index('Variable')
    
    theta = df.loc['C(Student_ID)[' + str(student_id) + ']']['Coefficient']
    
    if skill_name == 'ALT:CIRCLE-AREA':
        beta = 0
    else: 
        beta = df.loc['C(Skill)[T.' + skill_name + ']']['Coefficient']
    
    gamma = df.loc['Opportunity:C(Skill)[' + skill_name + ']']['Coefficient']
    
    logit = theta + beta + gamma * opportunity
    prob = 1 / (1 + np.exp(-logit))
    
    return prob

# prob for the whole dataframe
def success_probability(df):
    df['success_probability'] = p_student_skill(
        student_id = df['Student_ID'],
        skill_name = df['Skill'],
        opportunity = df['Opportunity']
        )
    return df

df_practice = df_practice.apply(success_probability, axis='columns')

#%% 3. ERROR RATE DATAFRAME
opportunity_count = (df_practice.groupby(['Skill', 'Opportunity'])['Success']
                     .count()
                     .rename('Number of Observation')
                     )

empirical_prob = (df_practice.groupby(['Skill', 'Opportunity'])['Success']
                    .mean()
                    .rename('P_Empirical Correctness')
                    )

predicted_prob = (df_practice.groupby(['Skill', 'Opportunity'])['success_probability']
                    .mean()
                    .rename('P_Predicted Correctness')
                    )

error_rate = pd.concat([opportunity_count, empirical_prob, predicted_prob], axis=1)
error_rate = error_rate.reset_index()
error_rate['Empirical error rate'] = 1 - error_rate['P_Empirical Correctness']
error_rate['Predicted error rate'] = 1 - error_rate['P_Predicted Correctness']

# basic visualization: error rate by skill
(error_rate[error_rate['Skill'] == 'ALT:CIRCLE-CIRCUMFERENCE']
 .sort_values('Opportunity')
 .plot(kind='line', 
       x='Opportunity', 
       y=['Empirical error rate', 'Predicted error rate'])
 )

# basic visualization: number of observation by skill
(error_rate[error_rate['Skill'] == 'ALT:CIRCLE-CIRCUMFERENCE']
 .sort_values('Opportunity')
 .plot(kind='line', x='Opportunity', y='Number of Observation')
 )

#%% 4. ERROR RATE VISUAL
#%% 4.a Overall Error Rate
fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(9, 6), dpi=200,
                         gridspec_kw={'height_ratios': [2, 1]},
                         constrained_layout=True,
                         sharex=True)
data1 = (error_rate.groupby('Opportunity')[['Empirical error rate', 
                                           'Predicted error rate']]
        .mean())

data2 = (error_rate.groupby('Opportunity')['Number of Observation']
        .sum())

data = data1.join(data2)
data = round(data, 2)
data = data.reset_index()

axes[0].plot('Opportunity','Empirical error rate', data=data,
         color='red', marker='s', markersize=4, linewidth=2)
axes[0].plot('Opportunity','Predicted error rate', data=data,
         color='green', marker='o', markersize=4,
         linestyle='dashed', linewidth=2)
axes[0].set_ylim(0, 1)
axes[0].set_ylabel('Error rate')
axes[0].set_title('Overall error rate of all skills')

axes[1].bar(data['Opportunity'], data['Number of Observation'])
axes[1].set_ylabel('Observation count')

plt.xlabel('Opportunity [th]')
plt.xticks(data['Opportunity'])

plt.show()

#%% 4.b Error rate for each skill
skill_list = error_rate['Skill'].unique().tolist()

def skill_plot(skil_name):
    fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(9, 6), dpi=200,
                             gridspec_kw={'height_ratios': [2, 1]},
                             sharex=True,
                             constrained_layout=True)
    
    data = (error_rate[error_rate['Skill'] == skil_name]
            .sort_values('Opportunity'))
    
    data = round(data, 2)
    data = data.reset_index()
    axes[0].plot('Opportunity','Empirical error rate', data=data,
             color='red', marker='s', markersize=4, linewidth=2)
    axes[0].plot('Opportunity','Predicted error rate', data=data,
             color='green', marker='o', markersize=4,
             linestyle='dashed', linewidth=2)
    axes[0].set_ylim(0, 1)
    axes[0].set_ylabel('Error rate')
    axes[0].set_title('Overall error rate of skill ' + skil_name)
    
    axes[1].bar(data['Opportunity'], data['Number of Observation'])
    axes[1].set_ylabel('Observation count')
    
    plt.xlabel('Opportunity [th]')
    plt.xticks(data['Opportunity'])
    
    plt.show()
    
for skill in skill_list:
    skill_plot(skill)
    
#%% SILLY CROSS CHECK WITH WEBSITE

for skill in skill_list:
    print(skill)
    print(df_practice[df_practice['Skill'] == skill]['Success'].value_counts())
