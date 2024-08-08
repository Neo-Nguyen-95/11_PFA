#%% I. LIBRARY & IMPORT
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

pd.set_option("display.max_columns", None)

# load data
df_practice = pd.read_csv('data_clean_update.csv')
lfa_coef = pd.read_csv('LFA_summary_python.csv')
pfa_coef = pd.read_csv('PFA_summary_python.csv')
bkt_coef = pd.read_csv('BKT_summary.csv')

#%% II. ANALYSIS
#%% 1. PREDICTED PROB FOR LFA
# Data format:
# - C(Student_ID)[1]
# - C(Skill)[T.ALT:CIRCLE-CIRCUMFERENCE]
# - Opportunity:C(Skill)[ALT:CIRCLE-AREA]

# extract coefficient
def lfa_success_probability(df, df_coef):
    df_coef = df_coef.set_index('Variable')
    
    # Extract coefficients for students and skill
    theta = (df['Student_ID']
             .map(lambda x: df_coef.at[f'C(Student_ID)[{x}]', 'Coefficient'])
             )
    beta = (df['Skill']
            .map(lambda x: df_coef.at[f'C(Skill)[T.{x}]', 'Coefficient'] if x != 'ALT:CIRCLE-AREA' else 0)
            )
    gamma = (df['Skill']
             .map(lambda x: df_coef.at[f'Opportunity:C(Skill)[{x}]', 'Coefficient'])
             )
    
    # Calculate logit and probability
    logit = theta + beta + gamma * df['Opportunity']
    df['lfa_success_probability'] = 1 / (1 + np.exp(-logit))
    
    return df

df_practice = lfa_success_probability(df_practice, lfa_coef)

#%% 2. PREDICTED PROB FOR PFA
# Format
# C(Skill)[ALT:CIRCLE-AREA]
# Success_opportunity:C(Skill)[ALT:CIRCLE-AREA]
# Fail_opportunity:C(Skill)[ALT:CIRCLE-AREA]

def pfa_success_probability(df, df_coef):
    df_coef = df_coef.set_index('Variable')
    
    beta = (df['Skill']
            .map(lambda x: df_coef.at[f'C(Skill)[{x}]', 'Coefficient'])
            )
    
    gamma = (df['Skill']
             .map(lambda x: df_coef.at[f'Success_opportunity:C(Skill)[{x}]', 'Coefficient'])
             )
    
    rho = (df['Skill']
           .map(lambda x: df_coef.at[f'Fail_opportunity:C(Skill)[{x}]', 'Coefficient'])
           )
    
    logit = beta + gamma * df['Success_opportunity'] + rho * df['Fail_opportunity']
    
    df['pfa_success_probability'] = 1 / (1 + np.exp(-logit))
    
    return df

df_practice = pfa_success_probability(df_practice, pfa_coef)

#%% 3. PREDICTED PROB FOR BKT
def transform_bkt_file(filepath):
    bkt_pred = pd.read_csv(filepath)
    bkt_pred.drop(columns=['Unnamed: 0', 'state_predictions'], inplace=True)
    bkt_pred.rename(columns={
        'user_id': 'Student_ID',
        'skill_name': 'Skill',
        'correct_predictions': 'bkt_success_probability'
        }, inplace=True)
    
    return bkt_pred

bkt_pred = transform_bkt_file('BKT_prediction.csv')

df_practice = df_practice.join(bkt_pred['bkt_success_probability'])

#%% 4. MODEL EVALUATE 
# AIC
def model_evaluation(real_value, predicted_value, coef, model='normal'):
    p = predicted_value + 1e-5
    y = real_value
    log_likelihood = (y * np.log(p) + (1 - y) * np.log(abs(1 - p))).sum()
    
    # num of param = number of student + number of skill + number of interaction
    if model == 'bkt':
        k = len(coef) * 4 / 5
    else:
        k = len(coef)
    
    AIC = -2 * log_likelihood + 2 * k
    MAD = (abs(p - y)).mean()
    return AIC, MAD

LFA_AIC, LFA_MAD = model_evaluation(df_practice['Success'],
                                    df_practice['lfa_success_probability'],
                                    lfa_coef)
print('LFA:')
print('- AIC: ', LFA_AIC)
print('- MAD: ', LFA_MAD)

PFA_AIC, PFA_MAD = model_evaluation(df_practice['Success'],
                                    df_practice['pfa_success_probability'],
                                    pfa_coef)
print('PFA:')
print('- AIC: ', PFA_AIC)
print('- MAD: ', PFA_MAD)

BKT_AIC, BKT_MAD = model_evaluation(df_practice['Success'],
                                    df_practice['bkt_success_probability'],
                                    bkt_coef, model='bkt')
print('BKT:')
print('- AIC: ', BKT_AIC)
print('- MAD: ', BKT_MAD)

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
    lfa_predicted_prob = (df.groupby(['Skill', 'Opportunity'])['lfa_success_probability']
                        .mean()
                        .rename('LFA_Predicted Correctness')
                        )
    pfa_predicted_prob = (df.groupby(['Skill', 'Opportunity'])['pfa_success_probability']
                        .mean()
                        .rename('PFA_Predicted Correctness')
                        )
    bkt_predicted_prob = (df.groupby(['Skill', 'Opportunity'])['bkt_success_probability']
                        .mean()
                        .rename('BKT_Predicted Correctness')
                        )
    
    error_rate = pd.concat([opportunity_count, empirical_prob, 
                            lfa_predicted_prob, pfa_predicted_prob,
                            bkt_predicted_prob], axis=1)
    error_rate = error_rate.reset_index()
    error_rate['Empirical error rate'] = 1 - error_rate['P_Empirical Correctness']
    error_rate['LFA error rate'] = 1 - error_rate['LFA_Predicted Correctness']
    error_rate['PFA error rate'] = 1 - error_rate['PFA_Predicted Correctness']
    error_rate['BKT error rate'] = 1 - error_rate['BKT_Predicted Correctness']
    
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
                                               'LFA error rate',
                                               'PFA error rate',
                                               'BKT error rate']]
            .mean())
    
    data2 = (error_rate.groupby('Opportunity')['Number of Observation']
            .sum())
    
    data = data1.join(data2)
    data = round(data, 2)
    data = data.reset_index()
    
    # Actual result
    axes[0].plot('Opportunity','Empirical error rate', data=data,
             color='grey', marker='s', markersize=3, linewidth=1)
    # LFA
    axes[0].plot('Opportunity','LFA error rate', data=data,
             color='green', marker='o', markersize=3,
             linestyle='dashed', linewidth=1)
    # PFA
    axes[0].plot('Opportunity','PFA error rate', data=data,
             color='blue', marker='o', markersize=3,
             linestyle='dashed', linewidth=1)
    
    # BKT
    axes[0].plot('Opportunity','BKT error rate', data=data,
             color='red', marker='o', markersize=3,
             linestyle='dashed', linewidth=1)
    
    axes[0].set_ylim(0, 1)
    axes[0].set_ylabel('Error rate')
    axes[0].set_title('Overall error rate of all skills')
    axes[0].legend(loc="upper right")
    
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
    # Actual
    axes[0].plot('Opportunity','Empirical error rate', data=data,
             color='grey', marker='s', markersize=3, linewidth=1)
    # LFA
    axes[0].plot('Opportunity','LFA error rate', data=data,
             color='green', marker='o', markersize=3,
             linestyle='dashed', linewidth=1)
    
    # PFA
    axes[0].plot('Opportunity','PFA error rate', data=data,
             color='blue', marker='o', markersize=3,
             linestyle='dashed', linewidth=1)
    
    # BKT
    axes[0].plot('Opportunity','BKT error rate', data=data,
             color='red', marker='o', markersize=3,
             linestyle='dashed', linewidth=1)
    
    axes[0].set_ylim(0, 1)
    axes[0].set_ylabel('Error rate')
    axes[0].set_title('Overall error rate of skill ' + skill_name)
    axes[0].legend(loc="upper right")
    
    axes[1].bar(data['Opportunity'], data['Number of Observation'])
    axes[1].set_ylabel('Observation count')
    
    plt.xlabel('Opportunity [th]')
    plt.xticks(data['Opportunity'])
    
    plt.show()
    
for skill in skill_list:
    skill_plot(skill)
  
    
#%% EXPORT
df_practice.to_excel('final_result.xlsx')  
#%% SILLY CROSS CHECK WITH WEBSITE

def cross_check_success(df_practice, skill_list):
    for skill in skill_list:
        print(skill)
        print(df_practice[df_practice['Skill'] == skill]['Success'].value_counts())

cross_check_success(df_practice, skill_list)