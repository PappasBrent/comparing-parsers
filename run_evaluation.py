#!/usr/bin/python3

import filecmp
import os
import subprocess
from dataclasses import dataclass
from datetime import datetime


@dataclass
class Parser:
    dir: str
    build_script: str
    run_script: str


PARSERS = [
    Parser('c', 'make', './main'),
    Parser('c++', 'make', './main'),
    Parser('c#', 'make', 'mono main.exe'),
    Parser('go', '', 'go run main.go'),
    Parser('haskell', 'make', './main'),
    Parser('java', 'make', 'java Main'),
    Parser('javascript', '', 'node main.js'),
    Parser('python', '', 'python3 main.py'),
    Parser('rust', 'cargo build', './target/debug/arith-parser'),
    Parser('typescript', 'make', 'node main.js'),
]


def main():
    # build the parsers
    for p in PARSERS:
        os.chdir(p.dir)
        subprocess.run(p.build_script, shell=True, capture_output=True)
        os.chdir('..')

    ifns = [os.path.join('inputs/', ifn)
            for ifn in os.listdir('inputs/')]

    # run the programs
    for ifn in ifns:
        print(f'# {ifn}')

        for p in PARSERS:
            os.chdir(p.dir)

            start = datetime.now()
            subprocess.run(
                f'cat ../{ifn} | {p.run_script} > ../results/{p.dir}.txt',
                shell=True)
            end = datetime.now()
            elapsed = end - start
            print(f'    {p.dir}: {elapsed.seconds}.{elapsed.microseconds}')

            os.chdir('..')

    # check that all files are the same
    results = os.listdir('results')
    os.chdir('results')
    for r1 in results:
        for r2 in [r for r in results if r != r1]:
            if not filecmp.cmp(r1, r2):
                print(r1, r2)


if __name__ == '__main__':
    main()
