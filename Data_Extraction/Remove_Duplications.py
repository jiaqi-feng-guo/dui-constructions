import re
import os

# Read lines containing "【对】" from the file
lines = (i for i in open('Native_Corpus.txt', 'r') if "【对】" in i) 

# Create a new file
f = open('Native_Corpus_New.txt', 'w', encoding="utf-8")  

# Write lines containing "【对】" into the file
f.writelines(lines)  

# Close the file
f.close()  

# Rename the original file
os.rename('Native_Corpus.txt', 'test.bak')   

# Remove the old file
os.remove('test.bak') 

# The function of the program is to determine whether there are duplicate sentences in the file
# And print out the duplicate sentences

# Create an empty list res_list, to add unique lines
res_list = [] 

# Create an empty list file_dul, to add duplicate lines
file_dul = [] 

# Open the f1 file
f1 = open('Native_Corpus_New.txt', "r", encoding="utf-8") 

# Iterate through lines in the file
for line in f1.readlines():  
    # If the line already exists in res_list
    if line in res_list:  
        # Add the duplicate to file_dul
        file_dul.append(line)   
    else:                    
        # Add the line to res_list
        res_list.append(line)    

f1.close()

# Create a file for unique lines
f2 = open("Native_Corpus_Clean.txt", "w",encoding="utf-8") 
f2.write("\n".join(res_list))
f2.close()

# Create a file for duplicate lines
f3 = open('Native_Corpus_Dup.txt', 'w', encoding="utf-8")   
f3.write("\n".join(file_dul))
f3.close()
