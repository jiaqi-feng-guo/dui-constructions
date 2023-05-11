import pandas as pd
import os

# Define a path name that will be used for storing statistical results
PATH = 'Statistics_Results'

# Check if the path exists, if not, create the directory
if not os.path.exists(PATH):
    os.mkdir(PATH)

# Define source filename, data will be read from this file
Source_File = 'RQ1_GLM.xlsx'

# Load data
df = pd.read_excel(Source_File, sheet_name="Complete")
df['BCC_Raw'] = df['BCC_Raw'].fillna(0)

# Count function word frequency in BCC
gnDic = {}
for k, gn in enumerate(df['Thematics_V5']):
    freq = df['BCC_Raw'][k]
    gnDic[gn] = gnDic.get(gn, 0) + freq

dataList = [list(data) for data in gnDic.items()]
new_df = pd.DataFrame(dataList, columns=['Functions', 'BCC_Raw'])
new_df.to_excel('NS_Functions_Full.xlsx', index=None)

# Count Semantic cluster frequency in BCC
gnDic = {}
for k, gn in enumerate(df['Semantics_V5']):
    freq = df['BCC_Raw'][k]
    gnDic[gn] = gnDic.get(gn, 0) + freq

dataList = [list(data) for data in gnDic.items()]
new_df = pd.DataFrame(dataList, columns=['Semantics', 'BCC_Raw'])
new_df.to_excel('NS_Semantics_Full.xlsx', index=None)

# Count Accessibility frequency in BCC
gnDic = {}
for k, gn in enumerate(df['Accessibility']):
    freq = df['BCC_Raw'][k]
    gnDic[gn] = gnDic.get(gn, 0) + freq

dataList = [list(data) for data in gnDic.items()]
new_df = pd.DataFrame(dataList, columns=['Accessibility', 'BCC_Raw'])
new_df.to_excel('BCC_Accessibility_Raw.xlsx', index=None)

# Count Accessiblity frequency types in BCC
def accessibility_count():
    # Count verb frequency
    One_one = df.groupby('Accessibility').size()
    # Save verb frequency to file
    One_one.to_excel('BCC_Accessibility_Type.xlsx')

accessibility_count()
