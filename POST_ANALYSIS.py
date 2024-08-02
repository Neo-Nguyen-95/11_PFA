import pandas as pd
pd.set_option("display.max_columns", None)

df_practice = pd.read_csv('clean_data.csv')
df_coef = pd.read_csv('model_summary_python.csv')



df_coef['Variable']
