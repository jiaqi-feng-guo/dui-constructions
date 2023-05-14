import pandas

file1 = 'HSK.xlsx'
file2 = 'Difficulty.xlsx'

df1 = pandas.read_excel(file1, header=None)
dataList = df1[0].tolist()
data01List = [(data.split('（')[0], data.split('（')[1].replace('）', '')) for data in dataList]
df2 = pandas.read_excel(file2, sheet_name="Sheet2")


def compare(data):
    for tmp in data01List:
        if data == tmp[0]:
            return tmp[1]
    return '7'


df2['Difficulty'] = df2['Verb'].apply(compare)
df2.to_excel('Accessibility.xlsx', sheet_name="Sheet1", index=None)
