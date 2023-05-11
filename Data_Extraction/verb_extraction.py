# coding=utf-8
import openai
import time
import urllib.request
import urllib.parse
import json
import hashlib
import base64
import os
from ltp import LTP
import xlrd
import xlwt
import re
import numpy as np
import csv
from tqdm import tqdm
import matplotlib as mpl
from wordcloud import WordCloud
import wordcloud
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image  # Image processing library


# Clean original sentences by removing spaces and 'NR'
def sentence_clean(sentence):
    sentence = sentence.replace(' ', '')
    sentence = sentence.replace('NRNR', 'NR')
    sentence = sentence.replace('NR', '晓', 1)
    sentence = sentence.replace('NR', '他', 2)
    sentence = sentence.replace('NS', '北京')
    sentence = sentence.replace('NT', '学校')
    sentence = sentence.replace('NZ', '你')
    sentence = re.sub('\W+', '', sentence).replace("_", ' ')

    return sentence


# Process sentences by ignoring characters before "对" and replacing them with "她"
def sentence_cut(sentence):
    matchRes = re.match(r'(.*)(对(.*))', sentence, re.M | re.I)
    if matchRes:
        sentence = '她' + matchRes.group(2)
        sentence = sentence.replace('对着', '对')  # Special handling
    else:
        print('do not have 对 ')
    return sentence


# Relationship analysis
res_dict = {}


def sentence_analyze(sentence, log=False):
    words = segment(sentence)
    postags = posttagger(words)
    arcs = parse(words, postags)
    roles = role_label(words, postags, arcs)
    if log:
        print_all(words, postags, arcs, roles)


def sentence_cut_analyze(sentence_cut, log=False):
    words = segment(sentence)
    postags = posttagger(words)
    arcs = parse(words, postags)
    roles = role_label(words, postags, arcs)
    if log:
        print_all(words, postags, arcs, roles)


ltp = LTP()


def get_result(TEXT, type="sdp"):
    segment, hidden = ltp.seg([TEXT])
    if type == "cws":
        return segment[0]
    if type == "sdp":
        return ltp.sdp(hidden)[0]
    if type == "dep":
        return ltp.dep(hidden)[0]
    if type == "slr":
        return ltp.slr(hidden)[0]


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

    if log:
        print("SRL")
    for x in srl:
        if log:
            print(x[0], x[1], x[2], x[3])

    return cws, pos, sdp, dep, srl


def analyze_and_output(result, log=False):
    cws = result[0]
    pos = result[1]
    sdp = result[2]
    dep = result[3]
    srl = result[4]

    if log:
        print(cws)
        print(pos)
        print(sdp)
        print(dep)
        print(srl)

    return cws, pos, sdp, dep, srl


if __name__ == "__main__":
    input_file = "input.txt"
    output_file = "output.txt"
    with open(input_file, "r", encoding="utf-8") as f:
        lines = f.readlines()
        for line in lines:
            line = line.strip()
            if not line:
                continue
            print(line)
            result = process_one_line(line)
            analyze_and_output(result)
