#%% LOAD DATA
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.metrics import silhouette_score
import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px
import plotly.io as pio
pio.renderers.default="browser"

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
    result.set_index('skill', inplace=True)
    return result

bkt_coef_update = bkt_transform(bkt_coef)
    
def pfa_transform(pfa_coef):
    pfa_coef[['type', 'skill']] = (pfa_coef['Variable']
                         .str.replace(']','')
                         .str.split('[', expand=True)
                         )

    df_skill = pfa_coef.iloc[0:15].set_index('skill')['Coefficient'].rename('skill_diff')
    df_success = pfa_coef.iloc[15:30].set_index('skill')['Coefficient'].rename('success_coef')
    df_fail = pfa_coef.iloc[30:45].set_index('skill')['Coefficient'].rename('fail_coef')
    result = pd.concat([df_skill, df_success, df_fail], axis='columns')
    
    return result

pfa_coef_update = pfa_transform(pfa_coef)

#%% CLUSTERING
def kmean(df):
    model = make_pipeline(
        StandardScaler(),
        KMeans(n_clusters=3, random_state=42)
        )
    
    model.fit(df)
    
    df['cluster'] = model.named_steps['kmeans'].labels_
    
    cluster_mean = df.groupby('cluster').mean()
    inertia = model.named_steps['kmeans'].inertia_
    
    ss = silhouette_score(df, model.named_steps['kmeans'].labels_)
    
    return df, cluster_mean, inertia, ss

#%% BKT coef clustering
bkt_coef_update, bkt_cluster_mean, bkt_inertia, bkt_ss = kmean(bkt_coef_update)
plt.figure(dpi=200)
sns.scatterplot(data=bkt_coef_update, x='prior', y='learns', hue='cluster',
                palette='pastel')

plt.figure(dpi=200)
sns.scatterplot(data=bkt_coef_update, x='guesses', y='slips', hue='cluster')

#%% PFA coef clustering
pfa_coef_update, pfa_cluster_mean, pfa_inertia, pfa_ss = kmean(pfa_coef_update)

fig = px.scatter_3d(pfa_coef_update, z='skill_diff', y='success_coef', x='fail_coef',
                    color='cluster')
fig.show()

#%% PFA coef export
pfa_coef_update.to_csv('PFA_coefficient_clustering.csv')
pfa_cluster_mean.to_csv('PFA_cluster_parameters.csv')

pfa_coef_update['cluster'] = list(range(len(pfa_coef_update)))
pfa_coef_update.to_csv('PFA_no_cluster_parameters.csv')

