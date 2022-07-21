# reference
# https://pythonguides.com/matplotlib-multiple-bar-chart/

import json
from collections import defaultdict
from typing import Dict, List

import matplotlib.pyplot as plt
import pandas as pd

MS_PER_S = 1_000_000

results: List[Dict] = []
with open('results.json') as fp:
    results = json.load(fp)

all_langs = list(sorted({r['parser_language'] for r in results}))

# set the font size
font = {'family': 'sans',
        'weight': 'normal',
        'size': 22}
plt.rc('font', **font)


def plot(input_file_names, title, filename, langs):
    # map input file names to their results
    inputs_to_results = defaultdict(list)
    for input_file_name in input_file_names:
        for lang in langs:
            # find the result for this parser on this input
            r = [r for r in results if r['input_file_name'] ==
                 input_file_name and r['parser_language'] == lang][0]
            inputs_to_results[input_file_name].append(r)

    # reorganize the data for pandas
    data = []
    for ifp, rs in inputs_to_results.items():
        data.append(
            [ifp] + [r['seconds'] + r['microseconds']/MS_PER_S for r in rs])

    # plot data
    df = pd.DataFrame(data, columns=["Language"] + langs)
    df.plot(x='Language', y=langs, kind="bar", figsize=(20, 10))
    ylabel = 'Execution rime (Seconds)'
    # plt.ylabel(ylabel, rotation=0, labelpad=len(ylabel)*2.5)
    plt.ylabel(ylabel)
    xlabel = 'Language'
    plt.xlabel(xlabel)
    plt.xticks(rotation=0)
    plt.title(title)

    plt.savefig(filename)


plot(['100.txt', '1K.txt', '10K.txt'],
     'Parser execution times by language on 100, 1K, and 10K lines of input',
     '100-1k-10k.jpg',
     all_langs)

plot(['100K.txt'],
     'Parser execution times by language on 100K lines of input',
     '100k.jpg',
     all_langs)

plot(['1M.txt'],
     'Parser execution times by language on 1M lines of input (sans Haskell and Python)',
     '1M.jpg',
     [lang for lang in all_langs if lang not in ['haskell', 'python']])
