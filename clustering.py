# -*- coding: utf-8 -*-
"""
@author: rmuh
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
from sklearn.neighbors import LocalOutlierFactor

def plot_clusters(points,labels,centers) :
    num_clusters = len(centers)
    points_std = StandardScaler().fit_transform(points)
    pca = PCA(n_components=2)
    pca_2d = pca.fit_transform(points_std)
    pca_2d_df = pd.DataFrame(data = pca_2d, columns = ['pc1', 'pc2'])
    COL = ['red', 'blue', 'green', 'yellow', 'gray', 'pink', 'violet', 
           'brown','cyan', 'magenta']
    clusters = list(set(labels))
    colors = COL[:num_clusters]
    fig = plt.figure(figsize = (12,8))
    ax = fig.add_subplot(1, 1, 1)
    targets = pd.DataFrame(data = labels, columns=['target'])
    for color, cluster in zip(colors,clusters):
        indicesToKeep = targets['target'] == cluster
        ax.scatter(pca_2d_df.loc[indicesToKeep,'pc1'], 
                   pca_2d_df.loc[indicesToKeep,'pc2'], 
                   c=color, s=7)
    ax.set_xlabel('Principal Component 1', fontsize = 15)
    ax.set_ylabel('Principal Component 2', fontsize = 15)
    ax.set_title('Clustering', fontsize = 20)
    ax.legend(clusters)
    plt.show()

#Read the file with the indicators
df = pd.read_csv('indicadores.csv')
x = df.iloc[:, 4:]
#Correlation Analysis
corrmat = x.corr() 
#Correlation Matrix
f, ax = plt.subplots(figsize =(9, 8)) 
sns.heatmap(corrmat, ax = ax, cmap ="Reds", linewidths = 0.08)

#Scale the features
x_std = StandardScaler().fit_transform(x)

#Principal Component Analysis

pca = PCA(n_components=2)
pca_2d = pca.fit_transform(x_std)

#Scatter plot with the first couple of principal components
fig = plt.figure(figsize = (12,8))
ax = fig.add_subplot(1,1,1) 
ax.set_xlabel('Principal Component 1', fontsize = 15)
ax.set_ylabel('Principal Component 2', fontsize = 15)
ax.set_title('2 component PCA', fontsize = 20)
ax.scatter(pca_2d[:,0], pca_2d[:,1], c='black', s=7)
ax.grid()

#Outlier detection
#LOF
lof = LocalOutlierFactor(n_neighbors=100, contamination=.02)
y_pred = lof.fit_predict(x_std)
LOF_Scores = lof.negative_outlier_factor_
LOF_pred = pd.Series(y_pred).replace([-1,1],[1,0])
LOF_anomalies = pca_2d[LOF_pred==1]

cmap=np.array(['white','red'])
plt.figure(figsize = (12,8))
plt.scatter(pca_2d[:,0],pca_2d[:,1],c='white',s=7,edgecolor='k')
plt.scatter(LOF_anomalies[:,0],LOF_anomalies[:,1],c='red',s=7)
plt.title('Local Outlier Factor — Anomalies')
plt.xlabel('pc1')
plt.ylabel('pc2')

#Isolation Forest
from sklearn.ensemble import IsolationForest
rs=np.random.RandomState(0)
clf = IsolationForest(max_samples=500,random_state=rs,behaviour="new", contamination=.02) 
clf.fit(x_std)
if_scores = clf.decision_function(x_std)
if_anomalies=clf.predict(x_std)
if_anomalies=pd.Series(if_anomalies).replace([-1,1],[1,0])
if_anomalies=pca_2d[if_anomalies==1]

cmap=np.array(['white','red'])
plt.figure(figsize = (12,8))
plt.scatter(pca_2d[:,0],pca_2d[:,1],c='white',s=7,edgecolor='k')
plt.scatter(if_anomalies[:,0],if_anomalies[:,1],c='red',s=7)
plt.xlabel('pc1')
plt.ylabel('pc2')
plt.title('Isolation Forests - Anomalies')

#Data without outliers
if_anomalies=clf.predict(x_std)
x_new = x[if_anomalies == 1]

#x_new = x[LOF_pred == 0]  #LOF

#Elbow Method

from sklearn import metrics
from scipy.spatial.distance import cdist

distortions = []
K = range(1,10)
for k in K:
    kmeanModel = KMeans(n_clusters=k).fit(x_new)
    kmeanModel.fit(x_new)
    distortions.append(sum(np.min(cdist(x_new, kmeanModel.cluster_centers_, 'euclidean'), axis=1)) /x_new.shape[0])

# Plot the elbow
plt.plot(K, distortions, 'bx-')
plt.xlabel('k')
plt.ylabel('Distortion')
plt.title('The Elbow Method showing the optimal k')
plt.show()

#K-means

kmeans = KMeans(n_clusters=3).fit(x_new)
centroids = kmeans.cluster_centers_
labels = kmeans.labels_.tolist()
print(centroids)

plot_clusters(x_new,labels,centroids)

#Order the groups
labels = pd.Series(labels).replace([0,1,2],[1,3,2])

#Frecuency bar chart

labels = pd.DataFrame(data=labels,columns=['cluster'])
count = labels['cluster'].value_counts(sort=True).to_list()
#print(count)

index = np.arange(len([2,3,1]))
plt.figure(figsize = (12,8))
plt.bar(index, count)
plt.xlabel('Socioeconomic Status', fontsize = 15)
plt.ylabel('No of ageb',fontsize = 15)
plt.xticks(index, [2,1,3], fontsize=15, rotation=30)
plt.title('Ageb distribution')
plt.show()

    


