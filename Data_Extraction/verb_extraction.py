# coding=utf-8

import time
import urllib.request
import urllib.parse
import json
import hashlib
import base64
import os
import xlrd
import xlwt
import re
import numpy as np
import csv
import string
from numpy.core._multiarray_umath import ndarray
from tqdm import tqdm
import matplotlib as mpl
from wordcloud import WordCloud
import wordcloud
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image  
from ltp import LTP


# Cleaning data - remove space and replace pseudonymised codes with 'Nh'
def sentence_clean(sentence):
    sentence = sentence.replace(' ', '')
    sentence = sentence.replace('NRNR', 'Nh')
    sentence = sentence.replace('NR', 'Nh', 1)
    sentence = sentence.replace('NR', 'Nh', 2)
    sentence = sentence.replace('NS', 'Nh')
    sentence = sentence.replace('NT', 'Nh')
    sentence = sentence.replace('NZ', '你')
    sentence = sentence.replace('(', '')
    sentence = sentence.replace(')', '')
    sentence = re.sub('\W+', '', sentence).replace("_", ' ')
    return sentence


# Noise reduction- replacing everything in front of '对' and replace it with '她' to confine the analysis to the PAVCs
def sentence_split(sentence):
    matchRes = re.match(r'(.*)(对(.*))', sentence, re.M | re.I)
    if matchRes:
        sentence = '她' + matchRes.group(2)
        sentence = sentence.replace('对着', '对')  # 特殊处理
    else:
        print('do not have 对 ', sentence)
    return sentence, "sentence_split"


# # Analyzing Dependency
res_dict = {}


def sentence_analyze(sentence, log=False):
    # log = True
    words = segment(sentence)
    postags = posttagger(words)
    arcs = parse(words, postags)
    roles = role_label(words, postags, arcs)
    if log:
        print_all(words, postags, arcs, roles)


def sentence_split_analyze(sentence_split, log=False):
    # log = True
    words = segment(sentence)
    postags = posttagger(words)
    arcs = parse(words, postags)
    roles = role_label(words, postags, arcs)
    if log:
        print_all(words, postags, arcs, roles)


ltp = LTP(path="base")  # Default is 'Small' mode


def get_result(TEXT, type="sdp"):
    segment, hidden = ltp.seg([TEXT])
    pos = ltp.pos(hidden)[0]
    if type == "cws":
        return segment[0]
    if type == "sdp":
        return ltp.sdp(hidden)[0]
    if type == "dep":
        return ltp.dep(hidden)[0]
    if type == "slr":
        return ltp.slr(hidden)[0]


segment, hidden = ltp.seg(["他叫汤姆去拿外衣。"])
pos = ltp.pos(hidden)
ner = ltp.ner(hidden)
srl = ltp.srl(hidden)
dep = ltp.dep(hidden)
sdp = ltp.sdp(hidden)


def process_one_line(TEXT, log=False):
    segment, hidden = ltp.seg([TEXT])
    cws = segment[0]
    sdp = ltp.sdp(hidden)[0]
    dep = ltp.dep(hidden)[0]
    srl = ltp.srl(hidden)[0]
    pos = ltp.pos(hidden)[0]
    if log:
        print(cws)
        print(pos)

    if log:
        print("SDP")
    sdgp_son = []
    sdpg_parent = []
    sdpg_relate = []
    for index, x in enumerate(sdp):
        sdgp_son.append(sdp[index][0] - 1)
        sdpg_parent.append(sdp[index][1] - 1)
        sdpg_relate.append(x[2])
        if log:
            print(sdgp_son[-1], sdpg_parent[-1], sdpg_relate[-1])

    if log:
        print("DEP")
    dep_son = []
    dep_parent = []
    dep_relate = []
    for index, x in enumerate(dep):
        dep_son.append(dep[index][0] - 1)
        dep_parent.append(dep[index][1] - 1)
        dep_relate.append(x[2])
        if log:
            print(dep_son[-1], dep_parent[-1], dep_relate[-1])

    return cws, pos, srl, \
           sdgp_son, sdpg_parent, sdpg_relate, \
           dep_son, dep_parent, dep_relate


def special_rule(cws, pos, index, method):  # Filtering non-PAV lines
    if (index + 1) == len(cws):
        return cws[index], "special_rule_index"
    if pos[index + 1] in ['v', 'a', 'i', 'n'] and cws[index + 1] is not ["对"]:
        return cws[index + 1], "special_rule_0"

    return None, "not found"


def get_word_sd(TEXT, log=False):
    try:
        TEXT = sentence_clean(TEXT)
        cws, pos, srl, sdgp_son, sdpg_parent, sdpg_relate, dep_son, dep_parent, dep_relate = process_one_line(TEXT, log)
    except BaseException:
        return "", "BaseException"

    # Extracting Verbs
    index_target = 0
    # if use_sdp:
    if True:
        predict_word_sdp = ""
        next_word_sdp = ""
        for index, word in enumerate(sdgp_son):
            if cws[word] == "对":
                index_target = index
                if sdpg_relate[index] in ["mRELA"]:
                    if sdpg_parent[index] >= 0:
                        next_word_sdp = sdpg_parent[index]
                        continue

        if next_word_sdp:
            for index, word in enumerate(sdgp_son):
                if word == next_word_sdp:
                    if sdpg_relate[index] in ["DATV", "REAS", "EXP", "AGT", "dDATV", "LOC", "AFT"]:
                        method = sdpg_relate[index]
                        if sdpg_parent[index] > index_target:
                            predict_word_sdp = cws[sdpg_parent[index]]
                            if predict_word_sdp is not ["对"] and pos[sdpg_parent[index]] in ["v", "a", "i"]:
                                if predict_word_sdp in ["是", "不是", "是否"]:
                                    sp, method = special_rule(cws, pos, sdpg_parent[index], method)
                                    if sp is not None:
                                        return sp, method
                                return predict_word_sdp, method

    # if use_dep:
    if True:
        predict_word_dep = ""
        for index, word in enumerate(dep_son):
            if cws[word] == "对":
                if dep_relate[index] in ["ADV"] and dep_parent[index] > word:
                    predict_word_dep = cws[dep_parent[index]]
                    if predict_word_dep in ["是", "不是", "是否"]:
                        sp, method = special_rule(cws, pos, dep_parent[index], 'ADV')
                        if sp is not None:
                            return sp, method
                    return predict_word_dep, "ADV"

    # use hed
    TEXT = sentence_split(TEXT)
    if log:
        print(TEXT)
    try:
        cws, pos, srl, sdgp_son, sdpg_parent, sdpg_relate, dep_son, dep_parent, dep_relate = process_one_line(TEXT, log)
    except BaseException:
        return "", "BaseException"

    for index, word in enumerate(dep_son):
        if dep_relate[index] in ["HED"]:  # and cws[index] is not ["对"]:
            if cws[index] in ["对"]:
                return cws[index], "HED"

    # SRL
    for index, s in enumerate(srl):
        if len(s) > 0:
            for x in s:
                if x == ('ARGM-DIR', 'ARGM-ADV'):
                    if pos[index] in ["v", "a", "i"] and cws[index_target] is not ["对"]:
                        if cws[index] in ["是" or "不是"]:
                            sp, method = special_rule(cws, pos, index, 'SRL')
                            if sp is not None:
                                return sp, method
                        return cws[index], "srl"

    if next_word_dep:
        for i in range(index_target, len(cws)):
            if pos[i] in ["v" > "a" > "i"] and cws[index_target] is not ["对"]:
                return cws[i], "vai"

    if index_target + 1 < len(cws) and cws[index] in ["v", "a", "i"] and cws[index] is not ["对"]:
        return cws[index_target + 1], "just guess+1"
        return cws[index_target - 1], "just guess_1"

# Note to examiners:
# Please download the 'Native_Corpus.txt' file from the GitHub repository
# (https://github.com/jiaqi-feng-guo/dui-constructions/blob/main/Pre_Data/Native_Corpus.txt)
# and place it in the same directory as this R script. If you choose to place it in a different directory,
# replace "Native_Corpus.txt" in the read_csv function with the correct path to the file.

# Processing corpus text file
f1 = open('Native_Corpus.txt', 'r+')  # Open file f1
Non_BaseException_arr = []  # Create an empty list
BaseException_arr = []
for line in f1:  # process each line in f1
    v_word, method = get_word_sd(sentence_clean(line))
    if method != "BaseException":  # check if the lines have a PAV construction
        Non_BaseException_arr.append(line)  # save the valid lines
    if method == "BaseException":  # check if the line is invalid(non-PAVC)
        BaseException_arr.append(line)  # save them into another file
f1.close()  # close the file

# creat a file with all valid Dui-construction sentences
f2 = open('Native_Valid_Constructions.txt', 'w')
f2.write("".join(Non_BaseException_arr))
f2.close()

# creat a file with all invalid Dui-construction sentences
f3 = open('Non-Dui_Constructions.txt', 'w')
f3.write("".join(BaseException_arr))
f3.close()

# processing the valid PAVC file
with open('Native_Valid_Constructions.txt', "r", encoding="utf-8") as f4:  # open the file
    line = f4.readline()  # process each line
    di = dict()  # create a verb dictionary

    for line in f4:
        v_word, method = get_word_sd(sentence_clean(line))
        if v_word in di.keys():
            di[v_word] += 1
        else:
            di[v_word] = 1

    sorted_di = sorted(di.items(), key=lambda k: k[1], reverse=True)  # count the verbs
    print(sorted_di)  # show all verbs and the frequency

    f5 = open('Native_frequency.csv', 'w', newline='')
    c = csv.writer(f5)
    for data in sorted_di:
        c.writerow(list(data))
    f5.close()

# Place a space in front and behind the collocating verb
f6 = open('Native_Valid_Constructions_Space', 'r+')
target_arr = []
for line in f6:
    target_arr.append(line)
f6.close()

f7 = open('Native_Corpus_New.txt', 'w')  # create a new file - f7
for line in target_arr:  # process all target_arr lines in f6
    v_word, method = get_word_sd(sentence_clean(line))  # define v_word,method and location
    new_sent = []  # creat a new list
    new_sent = re.sub(v_word, " {} ".format(v_word), line)  # define new sentence, separate the verb with spaces
    f7.write(''.join(new_sent))  # write the new_sent into f7
f7.close()  # close file f7


if __name__ == '__main__':
  print(process_excel(count=-1))
# print(get_word_sd('尽管对他是恨的咬牙切齿;', True))

