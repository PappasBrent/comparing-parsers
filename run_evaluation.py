#!/usr/bin/python3

import filecmp
import json
import os
import subprocess
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta
from typing import List


@dataclass
class Parser:
    dir: str
    build_script: str
    run_script: str


PARSERS = [
    Parser('bc', '', './bc.sh'),
    Parser('c', 'make', './main'),
    Parser('c#', 'make', 'mono main.exe'),
    Parser('c++', 'make', './main'),
    Parser('go', '', 'go run main.go'),
    Parser('haskell', 'make', './main'),
    Parser('java', 'make', 'java Main'),
    Parser('javascript', '', 'node main.js'),
    Parser('python', '', 'python3 main.py'),
    Parser('rust', 'cargo build', './target/debug/arith-parser'),
    Parser('typescript', 'make', 'node main.js'),
]


@dataclass
class EvaluationResult:
    input_file_name: str
    parser_language: str
    seconds: int
    microseconds: int


def parse_and_print_results(p: Parser, ifp: str, ofp: str) -> timedelta:
    '''
    runs the parser p in the input ifp, prints the results to ofp,
    and returns the time elapsed
    '''
    start = datetime.now()
    subprocess.run(f'{p.run_script} {ifp} >{ofp}', shell=True)
    end = datetime.now()
    return end - start


def main():
    # build the parsers
    for p in PARSERS:
        os.chdir(p.dir)
        subprocess.run(p.build_script, shell=True, capture_output=True)
        os.chdir('..')

    # collect input files
    fns = [os.path.realpath(os.path.join('inputs/', fn))
           for fn in os.listdir('inputs/')]

    # clear old results
    subprocess.run('rm -fr results', shell=True)
    os.makedirs('results', exist_ok=True)

    # store evaluation results in a list
    evaluation_results: List[EvaluationResult] = []

    # for each input
    for fn in fns:
        print(f'{fn}')

        # for each parser
        for p in PARSERS:
            # change to the parer's directory
            os.chdir(p.dir)

            # run bc only once, since it's not a part of the study and we're
            # only using it to check the output of the other parsers
            if p.dir == 'bc':
                # one minor catch here: bc runs until it encounters
                # the quit command, but my parsers run until
                # the end of the input
                # i see three ways around this:
                # 1) read the input line by line, and echo each line to bc
                # 2) change all my parsers to check for the quit command
                # 3) append the quit command to the input, pass it to bc,
                #    then remove the quit command
                # (1) is slow, and I don't like (2) because that's tedious.
                # so I opt for (3).

                # append the quit command to the file
                subprocess.run(
                    f'echo quit >> {fn}', shell=True, capture_output=True)

                # run bc on the temp file
                parse_and_print_results(p, fn, f'../results/{p.dir}.txt')

                # remove the quit command from the file
                subprocess.run(
                    f"sed -i -z 's/quit\\n//g' {fn}", shell=True, capture_output=True)

            else:
                # run the parser twice to warm up the cache
                parse_and_print_results(p, fn, f'../results/{p.dir}.txt')
                parse_and_print_results(p, fn, f'../results/{p.dir}.txt')

                # run the parser 5 times, and take the average of those times
                # averaging timedeltas: https://stackoverflow.com/a/3617540/6824430
                times = [parse_and_print_results(p, fn, f'../results/{p.dir}.txt')
                         for _ in range(5)]
                avg = sum(times, timedelta(0)) / len(times)
                # extract the input's file name
                ifn = os.path.split(fn)[1]
                # add result to results list
                r = EvaluationResult(ifn, p.dir, avg.seconds, avg.microseconds)
                evaluation_results.append(r)
                print(f'    {p.dir}: {avg.seconds}.{avg.microseconds}')

            # change back to the top-level directory
            os.chdir('..')

        # check that output of all parsers is the same as that of bc
        bc_result = os.path.realpath('results/bc.txt')
        parser_results = [os.path.realpath(os.path.join('results/', r))
                          for r in os.listdir('results/')
                          if r != 'bc.txt']
        for r in parser_results:
            if not filecmp.cmp(bc_result, r):
                print(f'{r} != {bc_result}')

    # print results to json
    with open('results.json', 'w') as fp:
        json.dump([asdict(r) for r in evaluation_results], fp, indent=4)


if __name__ == '__main__':
    main()
