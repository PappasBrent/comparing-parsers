#!/usr/bin/python3

import filecmp
import os
import subprocess
from dataclasses import dataclass
from datetime import datetime, timedelta


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


def time_parser_on_input(p: Parser, fn: str) -> timedelta:
    start = datetime.now()
    subprocess.run(
        f'cat ../{fn} | {p.run_script} > ../results/{p.dir}.txt',
        shell=True)
    end = datetime.now()
    return end - start


def main():
    # build the parsers
    for p in PARSERS:
        os.chdir(p.dir)
        subprocess.run(p.build_script, shell=True, capture_output=True)
        os.chdir('..')

    # collect input files
    fns = [os.path.join('inputs/', fn) for fn in os.listdir('inputs/')]

    # for each input
    for fn in fns:
        print(f'# {fn}')

        # for each parser
        for p in PARSERS:
            # change into the parser's directory
            os.chdir(p.dir)

            # run the parser twice to warm up the cache
            time_parser_on_input(p, fn)
            time_parser_on_input(p, fn)

            # run the parser 5 times, and take the average of those times
            # averaging timedeltas: https://stackoverflow.com/a/3617540/6824430
            times = [time_parser_on_input(p, fn) for _ in range(5)]
            avg = sum(times, timedelta(0)) / len(times)
            print(f'    {p.dir}: {avg.seconds}.{avg.microseconds}')

            # go back to the top-level directory
            os.chdir('..')

        # check that output of all parsers is the same
        results = os.listdir('results')
        os.chdir('results')
        for r1 in results:
            for r2 in [r for r in results if r != r1]:
                if not filecmp.cmp(r1, r2):
                    print(f'{r1} != {r2}')


if __name__ == '__main__':
    main()
