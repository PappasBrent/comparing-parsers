import json
from typing import Dict, List

MS_PER_S = 1_000_000

j: List[Dict] = []
with open('results.json') as fp:
    j = json.load(fp)

print('input,language,elapsed time (seconds)')
for r in j:
    ifn = r['input_file_name']
    lang = r['parser_language']
    s = r['seconds']
    ms = r['microseconds']
    elapsed = s + (ms/MS_PER_S)
    print(','.join([ifn, lang, str(elapsed)]))
