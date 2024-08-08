#%% LOAD DATA
import pandas as pd
from pyBKT.models import Model

pd.set_option('display.max_columns', None)

df = pd.read_csv('data_clean_update.csv')

#%% FITTING MODEL
data = df[['Student_ID', 'Skill', 'Success']]
data.columns = ['user_id', 'skill_name', 'correct']

def get_predicted_BKT(data):
    df_predict = pd.DataFrame()
    all_user_params = pd.DataFrame()
    
    user_list = data['user_id'].unique()
    
    for user in user_list:
        # predicted values
        user_data = data[data['user_id'] == user]
        model = Model(seed=42)
        model.fit(data=user_data)
        df_predict = pd.concat([df_predict, model.predict(data=user_data)],
                               axis='rows')
        # extract user paramters
        user_params = model.params()
        user_params['user_id'] = user
        all_user_params = pd.concat([all_user_params, user_params],
                                    axis='rows')
    
    return df_predict, all_user_params

df_predict, all_user_params = get_predicted_BKT(data)
    
#%% EXPORT
all_user_params.to_csv('BKT_summary.csv')
df_predict.to_csv('BKT_prediction.csv')