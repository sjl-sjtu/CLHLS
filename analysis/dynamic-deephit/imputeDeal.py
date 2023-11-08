# -*- coding: utf-8 -*-
"""
Created on Sun Nov 14 12:15:32 2021

@author: 10237
"""
import os
os.chdir("D:\\OneDrive\\newclhls")
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

import random
random.seed(10) 
np.random.seed(10)

df = pd.read_csv("dflongi2.csv")
df = df.sort_values(["id","year"],ascending=True)

IDs = df.loc[:,"id"]
IDs = np.unique(IDs)
IDs = np.sort(IDs)

#入组即为1
IDin = df.loc[ (df["totalTime"]==0) & (df["outcome"]==1),"id"]
IDin = np.unique(IDin)
#IDs = [i for i in IDs if i not in IDin.tolist()]
IDs = np.setdiff1d(IDs,IDin)
df = df.loc[df["id"].isin(IDs),:]

#df = df.dropna(axis = 0, subset = ['times'] )
df = df.dropna(axis = 0, how = 'any', subset = ['times','ADL','bmi','emo'])

grouped = df.groupby("id")
idnew = df["id"].unique()
freq = []
for i in range(len(idnew)):
    dfid = grouped.get_group(idnew[i])
    freq.append(dfid.shape[0])
df_idrate = pd.DataFrame({"id":idnew,"freq":freq})
unmiss_id = df_idrate.loc[df_idrate["freq"]>=2,"id"]
df = df.loc[df["id"].isin(unmiss_id),:]

df['ADL'] = df['ADL']/12
df.to_csv("dflongi_use2.csv",index=False)


####################



vlist = []
for i in list(range(1,22)):
    v = "V"+str(i)
    vlist.append(v)

IDs = df.loc[:,"ID"]
IDs = np.unique(IDs)
IDs = np.sort(IDs)

df1 = df.loc[df["CAS"]==1,:]
df0 = df.loc[df["CAS"]==0,:]

#去极端值：箱线图
def barboxTest(df):
    xID= []
    for i in vlist:
        lower_q=np.quantile(df[i],0.25,interpolation='lower')#下四分位数
        higher_q=np.quantile(df[i],0.75,interpolation='higher')#上四分位数
        int_r=higher_q-lower_q#四分位
        U = higher_q + 1.5*int_r
        L = lower_q - 1.5*int_r
        xID.append(df.loc[df[ df[i]<L].index,"ID"].tolist())
        xID.append(df.loc[df[ df[i]>U].index,"ID"].tolist())
        df.loc[df[df[i]<L].index,i] = np.nan
        df.loc[df[df[i]>U].index,i] = np.nan
    return df,xID
df0,xID0 = barboxTest(df0)
df1,xID1 = barboxTest(df1)
df = pd.concat([df1,df0])
df = df.sort_values(["ID","year"],ascending=True)
df.index = np.arange(0,df.shape[0],1)

#入组即为1
# =============================================================================
# IDin = df.loc[ (df["totaltime"]==0) & (df["CHD"]==1),"ID"]
# IDs = [i for i in IDs if i not in IDin.tolist()]
# df = df.loc[df["ID"].isin(IDs),:]
# 
# =============================================================================
#删除发病后的记录
# =============================================================================
# df = df.drop(df[(df["timenow"]>df["totaltime"]) & (df["CHD"]==1)].index)
# =============================================================================

#控制变量缺失率
X = df.loc[:,vlist]
miss_rate_X = X.isnull().sum().sort_values(ascending=False) / X.shape[0]
#plt.plot(miss_rate_X.sort_values())
#plt.hist(miss_rate_X,bins=[i*0.05 for i in list(range(0,22,1))])
unmiss_feature = miss_rate_X[miss_rate_X<=1]
unmiss_feature = unmiss_feature._stat_axis.values.tolist() 

#控制个体
# =============================================================================
# miss_rate_ID = df.loc[:,unmiss_feature].isnull().sum(axis=1)/len(unmiss_feature)
# plt.hist(miss_rate_ID,bins=[i*0.05 for i in list(range(0,22,1))])
# extreID = df.loc[df.loc[:,unmiss_feature].isnull().sum(axis=1)/len(unmiss_feature)>1,"ID"]
# unmiss_id = df.loc[df["frequency"]>=1,"ID"]
# choose_id = [i for i in unmiss_id if i not in extreID.tolist()]
# df = df.loc[df["ID"].isin(choose_id),:]
# =============================================================================

# =============================================================================
# miss_rate_ID = df.loc[:,unmiss_feature].isnull().sum(axis=1)/len(unmiss_feature)
# plt.hist(miss_rate_ID,bins=[i*0.05 for i in list(range(0,22,1))])
# df = df.drop(df.loc[df.loc[:,unmiss_feature].isnull().sum(axis=1)/len(unmiss_feature)>0.95,:].index)
# df.index = np.arange(0,df.shape[0],1)
# =============================================================================

grouped = df.groupby("ID")
idnew = df["ID"].unique()
id_unmiss_times = []
for i in range(len(idnew)):
    dfid = grouped.get_group(idnew[i])
    id_unmiss_times.append(dfid.shape[0])
df_idrate = pd.DataFrame({"ID":idnew,"unmiss_times":id_unmiss_times})
unmiss_id = df_idrate.loc[df_idrate["unmiss_times"]>=1,"ID"]
df = df.loc[df["ID"].isin(unmiss_id),:]
#df = df.loc[df["frequency"]>=2]

#impute
df["normtime"] = df["timenow"]
normfeat = unmiss_feature+["age","normtime"]
impfeat = unmiss_feature+["sex","age","CAS","normtime"]
df.loc[:,normfeat] = (df.loc[:,normfeat] - df.loc[:,normfeat].min())/(df.loc[:,normfeat].max() - df.loc[:,normfeat].min())
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer,KNNImputer
imp = KNNImputer(n_neighbors=10)
imp = imp.fit(df.loc[:,impfeat])
Ximp = imp.transform(df.loc[:,impfeat])
Ximp = pd.DataFrame(Ximp)
Ximp.columns = impfeat
df.update(Ximp,overwrite=False)
df = df.sort_values(["ID","year"],ascending=True)
df.index = np.arange(0,df.shape[0],1)
df["totaltime"] = df["time"]

df.to_csv("dflongi_use2.csv",index=False)

