# FA data cleaning

import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt


# Clean the data 

compustat = pd.read_csv("compustat_1950_2018_annual_merged.csv")
compustat_1 = compustat[["gvkey","datadate","fyear","revt","rect","ppegt","epspi","ni","at","oancf","sic","rdq"]]
compustat_2 = compustat_1.loc[compustat_1["fyear"] > 1987]
compustat_2 = compustat_2.loc[compustat_2['fyear'] < 2018]
compustat_3 = compustat_2[compustat_2["revt"] >=0]
com_3 = compustat_3[compustat_3['rect'] >= 0]
com_4 = com_3.dropna(subset = ['at','revt','ni','epspi','rect','oancf'])
com_5 = com_4.drop_duplicates(com_4.columns.difference(['rdq']))
com_6 = com_5.fillna(0)

com_6.to_csv('cleaned_compu.csv', float_format = '%.6f', index = 0)

### Descriptive Analysis ###

# 1. The average revenue, net income, total assests for a firm in fiscal year 2017.
df = pd.read_csv('cleaned_compu.csv')

y2017 = df[df['fyear'] == 2017]
y17_rev = round(y2017['revt'].mean(),2)
y17_ni = round(y2017['ni'].mean(),2)
y17_at = round(y2017['at'].mean(),2)

data_mean = [y17_rev, y17_ni, y17_at]
index = ['Revenue', 'Net Income', 'Total Asset' ]
y17_average = pd.DataFrame(data_mean, index = index, columns = ['2017_Average'])

print(y17_average)

# 2. Plot the average revenue, net income, total asset from 1988-2017

df2 = df.groupby('fyear').agg({'revt':'mean', 'ni':'mean', 'at':'mean'})

plt.figure(figsize=(10,8))
plt.plot(df2['revt'], linestyle = 'solid')
plt.plot(df2['ni'], linestyle = 'dashed')
plt.plot(df2['at'], linestyle = 'dashdot')
plt.legend(df2.columns.values.tolist())
plt.title('Trend for Revenue, Net Income & Total Asset 1988-2017')
plt.show()

### Indirect Evidence of Earning Management

# EPS change from year t to t-1
df3 = df
df3['previous_eps'] = df3.sort_values(['fyear']).groupby('gvkey')['epspi'].shift()
df3['eps_change'] = df3.epspi - df3.previous_eps

df4 = df3[df3['eps_change'] <= 0.1]
df4 = df4[df4['eps_change'] >= -0.1]
df4_eps = df4['eps_change']

plt.style.use('seaborn-white')
plt.figure(figsize = [15,6])
plt.hist(df4_eps, bins=20, align = 'left')
plt.plot((0,0),(0,8000),'r--')
plt.title('EPS Change Distirbution')

# ROA change from t to t-1
df3['previous_at'] = df3.sort_values(['fyear']).groupby('gvkey')['at'].shift()
df3['ROA'] = df['ni']/df['previous_at']
df3['previous_roa'] = df3.sort_values(['fyear']).groupby('gvkey')['ROA'].shift()
df3['roa_change'] = df3.ROA - df3.previous_roa

df5 = df3[df3['roa_change'] <= 0.1]
df5 = df5[df3['roa_change'] >= -0.1]
df5_roa = df5['roa_change']

plt.style.use('seaborn-white')
plt.figure(figsize = [15,6])
plt.hist(df5_roa, bins=20, align = 'left')
plt.plot((0,0),(0,26000),'r--')
plt.title('ROA Change Distirbution')