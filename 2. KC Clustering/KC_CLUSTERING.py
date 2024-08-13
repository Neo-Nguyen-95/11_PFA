#%% --- 0. LOAD DATA ---
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

# get KC data from raw dataset
class kc_raw_process:
    """Get KC data from raw dataset"""
    def __init__(self, datapath):
        self.df = pd.read_csv(datapath)
    
    def get_ctt_diff(self):
        return self.df.groupby('Skill')['Success'].mean().rename('ctt_diff')
    
    def get_max_off(self):
        return (self.df.groupby('Skill')['Opportunity'].max()
                .rename('max_opportunity')
                )
    
    def get_averate_opp(self):
        return (self.df.groupby(['Skill', 'Student_ID'])['Opportunity'].max()
                .reset_index()
                .groupby('Skill')['Opportunity'].mean()
                .rename('average_opportunity')
                )

datapath = '/Users/dungnguyen/Desktop/Data Science off/Python Programming/1. Work Project/11_PFA/1. Parameter Estimation/data_clean_update.csv'
raw_data = kc_raw_process(datapath=datapath)
kc_ctt_diff = raw_data.get_ctt_diff()
kc_max_opp = raw_data.get_max_off()
kc_average_opp = raw_data.get_averate_opp()


#%% --- I. TRANSFORM ---
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

#%% --- II. COEF DATA MINING ---
bkt_temp = bkt_coef_update.copy()
bkt_temp.columns = 'bkt_' + bkt_temp.columns

pfa_temp = pfa_coef_update.copy()
pfa_temp.columns = 'pfa_' + pfa_temp.columns

coef_mining = (bkt_temp.join(pfa_temp)
               .join(kc_ctt_diff)
               .join(kc_max_opp)
               .join(kc_average_opp)
               )

# check correlation between parameters
correlation_matrix = coef_mining.corr()
plt.figure(dpi=200)
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', fmt='.2f',
            vmin=-1, vmax=1)
sns.pairplot(coef_mining, diag_kind='kde')


#%% --- III. CLUSTERING ---
def kmean(df, cluster=3):
    model = make_pipeline(
        StandardScaler(),
        KMeans(n_clusters=cluster, random_state=42)
        )
    
    model.fit(df)
    
    df['cluster'] = model.named_steps['kmeans'].labels_
    
    cluster_mean = df.groupby('cluster').mean()
    inertia = model.named_steps['kmeans'].inertia_
    
    ss = silhouette_score(df, model.named_steps['kmeans'].labels_)
    
    return df, cluster_mean, inertia, ss

#%% 3.1 BKT coef k-mean clustering
bkt_coef_update, bkt_cluster_mean, bkt_inertia, bkt_ss = kmean(bkt_coef_update)
plt.figure(dpi=200)
sns.scatterplot(data=bkt_coef_update, x='prior', y='learns', hue='cluster',
                palette='pastel')

plt.figure(dpi=200)
sns.scatterplot(data=bkt_coef_update, x='guesses', y='slips', hue='cluster')

#%% 3.2.A PFA k-mean grid search
inertia = []
ss = []
cluster_range = list(range(2, 10))
for cluster in cluster_range:
    pfa_coef_update, pfa_cluster_mean, pfa_inertia, pfa_ss = kmean(
        pfa_coef_update, cluster=cluster)
    inertia.append(pfa_inertia)
    ss.append(pfa_ss)
    
sns.scatterplot(x=cluster_range, y=inertia)
sns.scatterplot(x=cluster_range, y=ss)

#%% 3.2.B PFA k-mean coef clustering
pfa_coef_update, pfa_cluster_mean, pfa_inertia, pfa_ss = kmean(
    pfa_coef_update, cluster=4)

# fig = px.scatter_3d(pfa_coef_update, z='skill_diff', y='success_coef', x='fail_coef',
#                     color='cluster')
# fig.show()

#%% PFA coef export
# pfa_coef_update.to_csv('PFA_coefficient_clustering.csv')
# pfa_cluster_mean.to_csv('PFA_cluster_parameters.csv')

# pfa_coef_update['cluster'] = list(range(len(pfa_coef_update)))
# pfa_coef_update.to_csv('PFA_no_cluster_parameters.csv')

