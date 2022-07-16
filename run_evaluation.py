import filecmp
import os
import subprocess
from dataclasses import dataclass
from datetime import datetime


@dataclass
class Program:
    dir: str
    build_script: str
    run_script: str


INTERPRETERS = [
    Program('c', 'make', './main'),
    Program('go', '', 'go run main.go'),
    Program('haskell', 'make', './main'),
    Program('python', '', 'python3 main.py'),
    Program('rust', 'cargo build', './target/debug/arith-interpreter'),
]


def main():
    # build the programs
    for inter in INTERPRETERS:
        os.chdir(inter.dir)
        subprocess.run(inter.build_script, shell=True, capture_output=True)
        os.chdir('..')

    ifns = ['inputs/100k.txt']

    # run the programs
    for ifn in ifns:
        print(f'# {ifn}')

        for inter in INTERPRETERS:
            os.chdir(inter.dir)

            start = datetime.now()
            subprocess.run(
                f'cat ../{ifn} | {inter.run_script} > ../results/{inter.dir}.txt',
                shell=True)
            end = datetime.now()
            elapsed = end - start
            print(f'    {inter.dir}: {elapsed.seconds}.{elapsed.microseconds}')

            os.chdir('..')

    # check that all files are the same
    results = os.listdir('results')
    os.chdir('results')
    for r1 in results:
        for r2 in results:
            if not filecmp.cmp(r1, r2):
                print(r1, r2)


if __name__ == '__main__':
    main()
