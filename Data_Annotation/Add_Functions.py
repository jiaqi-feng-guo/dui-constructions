#coding=utf-8

import pandas

def add_count(filename1, filename2):
    '''
    This function adds a frequency column to the dataset.
    :param filename1: The name of the CSV file which contains the frequency of words e.g., 'Native_Frequency.csv'
    :param filename2: The name of the CSV file which needs the frequency column added e.g., 'Functions_Dict.csv'
    :return: None. An Excel file 'count.xls' is saved as output.
    '''

    # Read the first CSV file and assign column names
    pd = pandas.read_csv(filename1, header=None)
    pd.columns = ['Verb', 'Frequency']

    # Extract verb frequencies and store them in a list of lists
    Data_list = [[data, pd['Frequency'][k]] for k, data in enumerate(pd['Verb'])]

    # Read the second CSV file
    pd2 = pandas.read_csv(filename2)

    # For each verb in the second CSV file, if it matches a verb in the Data_list, add its frequency to the cipin list
    cipin = []
    for verb in pd2['Verb']:
        tag = False
        for data in Data_list:
            if verb == data[0]:
                cipin.append(data[1])
                tag = True
                break
        if not tag:
            cipin.append('')

    # Add the 'cipin' list as a new column '词频' in the second CSV file
    pd2_colname = pd2.columns.tolist()
    pd2_colname.insert(1, 'Frequency')
    pd2['Frequency'] = cipin
    pd2 = pd2.reindex(columns=pd2_colname)

    # Save the updated second CSV file as an Excel file
    pd2.to_excel('count.xls', index=None)

def add_function(filename1, filename2):
    '''
    This function adds a function column to the dataset.
    :param filename1: The name of the Excel file which contains the functions of words e.g., 'MLC动词功能加词频.xls'
    :param filename2: The name of the CSV file which needs the function column added e.g., '向_BCC_语义功能分类.csv'
    :return: None. An Excel file 'Complete.xls' is saved as output.
    '''

    # Read the Excel file
    pd = pandas.read_excel(filename1)

    # Read the CSV file and store verbs and their corresponding functions in a list of lists
    tmp_pd = pandas.read_csv(filename2)
    Data = [[v, tmp_pd['Function'][k]] for k, v in enumerate(tmp_pd['Verb'])]

    # For each verb in the Excel file, if it matches a verb in the Data list, add its function to the ret_data list
    ret_data = []
    for data in pd['Verb']:
        tag = False
        for verb in Data:
            if data == verb[0]:
                ret_data.append(verb[1])
                tag = True
                break
        if not tag:
            ret_data.append('')

    # Add the 'ret_data' list as a new column 'Function' in the Excel file
    pd_columns = pd.columns.tolist()
    pd_columns.insert(2, 'Function')
    pd['Function'] = ret_data
    pd = pd.reindex(columns=pd_columns)

    # Group by all columns and count the size of each group
    pd
