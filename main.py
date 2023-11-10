
import bar_chart_race as bcr
import pandas as pd
import numpy as np


df = pd.read_csv('C:/Users/TRM/Desktop/Git/Analyse_sjov/data/bcr_retuneret.csv',
                 delimiter=";",
                 index_col = "DATO",
                 na_values=['nan']).fillna(0)
df.dtypes

df.drop("Unnamed: 0", inplace = True,  axis=1)
df = df.apply(np.int64)

df.columns = df.columns.str.replace(' ', '')
df.columns = df.columns.str.replace('.', '')
df.columns = df.columns.str.replace('ø', 'oe')
df[1:5]

bcr.bar_chart_race(df=df[1:40],
                   filename="video.mp4"
                   )