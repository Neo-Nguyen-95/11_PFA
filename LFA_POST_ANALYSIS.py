#%% I. LIBRARY & IMPORT
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

pd.set_option("display.max_columns", None)

# load data
df_practice = pd.read_csv('data_clean_update.csv')
df_coef = pd.read_csv('model_summary_python.csv')

#%% II. ANALYSIS
#%% 1. SKILL COUNT
def skill_counts(df):
    student_skill_count = (df['Skill']
                          .groupby(df_practice['Student_ID'])
                          .value_counts()
                          .reset_index())
    skill_practice_max = (student_skill_count['count']
                          .groupby(student_skill_count['Skill'])
                          .max())
    skill_practice_mean = (student_skill_count['count']
                           .groupby(student_skill_count['Skill'])
                           .mean())
    return skill_practice_max, skill_practice_mean

skill_practice_max, skill_practice_mean = skill_counts(df_practice)

#%% 2. PREDICTED PROB(CORRECT)
# Data format:
# - C(Student_ID)[1]
# - C(Skill)[T.ALT:CIRCLE-CIRCUMFERENCE]
# - Opportunity:C(Skill)[ALT:CIRCLE-AREA]

# extract coefficient
def success_probability(df_practice, df_coef):
    df_coef = df_coef.set_index('Variable')
    
    # Extract coefficients for students and skill
    theta = (df_practice['Student_ID']
             .map(lambda x: df_coef.at[f'C(Student_ID)[{x}]', 'Coefficient'])
             )
    beta = (df_practice['Skill']
            .map(lambda x: df_coef.at[f'C(Skill)[T.{x}]', 'Coefficient'] if x != 'ALT:CIRCLE-AREA' else 0)
            )
    gamma = (df_practice['Skill']
             .map(lambda x: df_coef.at[f'Opportunity:C(Skill)[{x}]', 'Coefficient'])
             )
    
    # Calculate logit and probability
    logit = theta + beta + gamma * df_practice['Opportunity']
    df_practice['success_probability'] = 1 / (1 + np.exp(-logit))
    
    return df_practice

df_practice = success_probability(df_practice, df_coef)

# AIC
def calculate_AIC(df):
    p = df['success_probability']
    y = df['Success']
    log_likelihood = (y * np.log(p) + (1 - y) * np.log(1 - p)).sum()
    
    # num of param = number of student + number of skill + number of interaction
    k = df_practice['Student_ID'].nunique() + df_practice['Skill'].nunique()*2
    
    return -2 * log_likelihood + 2 * k
    
calculate_AIC(df_practice)   

# MAD
def calculate_MAD(df):
    p = df['success_probability']
    y = df['Success']
    return (abs(p - y)).mean()

calculate_MAD(df_practice)

#%% 3. ERROR RATE DATAFRAME
def calculate_error_rate(df):
    opportunity_count = (df.groupby(['Skill', 'Opportunity'])['Success']
                         .count()
                         .rename('Number of Observation')
                         )
    
    empirical_prob = (df.groupby(['Skill', 'Opportunity'])['Success']
                        .mean()
                        .rename('P_Empirical Correctness')
                        )
    
    predicted_prob = (df.groupby(['Skill', 'Opportunity'])['success_probability']
                        .mean()
                        .rename('P_Predicted Correctness')
                        )
    
    error_rate = pd.concat([opportunity_count, empirical_prob, predicted_prob], axis=1)
    error_rate = error_rate.reset_index()
    error_rate['Empirical error rate'] = 1 - error_rate['P_Empirical Correctness']
    error_rate['Predicted error rate'] = 1 - error_rate['P_Predicted Correctness']
    
    return error_rate

error_rate = calculate_error_rate(df_practice)

#%% 4. ERROR RATE VISUAL
#%% 4.a Overall Error Rate
def plot_overall_error_rate(error_rate):
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
    
plot_overall_error_rate(error_rate)

#%% 4.b Error rate for each skill
skill_list = error_rate['Skill'].unique().tolist()

def skill_plot(skill_name):
    fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(9, 6), dpi=200,
                             gridspec_kw={'height_ratios': [2, 1]},
                             sharex=True,
                             constrained_layout=True)
    
    data = (error_rate[error_rate['Skill'] == skill_name]
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
    axes[0].set_title('Overall error rate of skill ' + skill_name)
    
    axes[1].bar(data['Opportunity'], data['Number of Observation'])
    axes[1].set_ylabel('Observation count')
    
    plt.xlabel('Opportunity [th]')
    plt.xticks(data['Opportunity'])
    
    plt.show()
    
for skill in skill_list:
    skill_plot(skill)
    
#%% SILLY CROSS CHECK WITH WEBSITE

def cross_check_success(df_practice, skill_list):
    for skill in skill_list:
        print(skill)
        print(df_practice[df_practice['Skill'] == skill]['Success'].value_counts())

cross_check_success(df_practice, skill_list)