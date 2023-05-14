#coding=utf-8

import pandas
import re

# Function to read data from a text file
def read_data(filename):
    '''
    This function reads a text file and returns a list of all JSON objects in the file.
    :param filename: The name of the text file to be read.
    :return: A list of JSON objects.
    '''

    # Open the file and read the content
    f = open(filename,'r',encoding='utf-8')
    data = f.read().replace('\n','')

    # Regular expression pattern to extract each JSON object
    restr = '({[\s\S]*?{[\s\S]*?{[\s\S]*?\[[\s\S]*?\]}}})'
    result_list = re.findall(restr, data)

    f.close()

    return result_list


# Function to determine the level of a word
def level(result_list, xlsx_str):
    '''
    This function determines the level of a word in the 'result_list'.
    :param result_list: A list of JSON objects.
    :param xlsx_str: A word whose level needs to be determined.
    :return: A list of lists where each list contains the levels of the word.
    '''

    data = []

    # For each JSON object
    for i in result_list:
        # Extract the level of the word
        tmp_restr = '{\'(.*?)\':'
        keywords = re.findall(tmp_restr, i, re.S)

        # Extract the words in the JSON object
        ciyu_restr = '\[(.*?)\]'
        tmp_ciyu_list = re.findall(ciyu_restr, i)
        ciyu_list = tmp_ciyu_list[0].split('、')

        # If the word is in the JSON object
        if xlsx_str in ciyu_list:
            data.append(keywords)

    # If the word is not in any JSON object
    if not len(data):
        return [['','','']]
    else:
        return data


# Main function
def main():
    '''
    This function reads data from a text file and an Excel file, determines the level of each word in the Excel file,
    and saves the result in a new Excel file.
    :return: None
    '''

    # Read data from the text file
    result_list = read_data('Semantics_Dict.txt')

    # Read data from the Excel file
    pd = pandas.read_excel('Native_Frequency.xlsx')

    # Extract the 'Verb' column
    keyword = pd['Verb']

    # List to store the result
    my_data = []

    # For each word in the 'Verb' column
    for k in keyword:
        # Determine the level of the word
        data_list = level(result_list, k)

        # For each level of the word
        for data in data_list:
            # Add the word and its levels to the result list
            my_data.append([k, data[0], data[1], data[2]])

    # Create a DataFrame from the result list
    pd = pandas.DataFrame(my_data, columns=['Verb','Level_1','Level_2','Level_3'])

    # Save the DataFrame to an Excel file
    pd.to_excel('Native_Semantics.xls', index=None)


# Call the main function
if __name__ == '__main__':
    main()
