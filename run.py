import os
import argparse
from subprocess import *

def run(args):
    if args.file is not None:
        call(['dune', 'exec', './bin/cmc.exe', args.file])
    else:
        call(['dune', 'exec', './bin/cmc.exe'])

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--file', default=None)
    args = parser.parse_args()
    run(args)