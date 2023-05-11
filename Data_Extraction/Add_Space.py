# _*_ coding:UTF-8 _*_
import re
import os

# Open the f1 document
f1 = open('Native_Corpus.txt', 'r')  

# Create an empty list
arr_sent = []  

# Iterate through sentences in f1
for line in f1:      
    # Split the sentence with specific punctuation
    arr = re.split(r"[，。；!]", line)  

    # Find out the number of split sentences
    arr_len = len(arr)  

    # Define the split sentence
    sent = ""    

    # Enumerate the position and split sentences in the sentence
    for i, item in enumerate(arr):  
        # Check if the short sentence contains the preposition 【对】
        if "【对】" in item:  
            # sent becomes the split sentence that contains the preposition【对】
            sent = item     
            # Add sent to the list
            arr_sent.append(sent)  

# Close the document
f1.close()  

# Create a new document f2
f2 = open('Native_Corpus_Modified.txt', 'w')   

# Write the sentences from arr_sent into f2
f2.write("\n".join(arr_sent))

# Close document f2
f2.close()  
