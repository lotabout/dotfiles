#!/usr/bin/env python3

# Usage
# <some command> | ./rewind-logger.py -s 100M log.txt

import argparse
import re
import os
import sys
import io

def parse_size(size):
    """pase size spec, e.g. 8192, """
    groups = re.findall(r'(\d+)([kKmMgG]?)', size)
    if len(groups) != 1:
        raise Exception(f'could not parse size {size}')
    count, unit = groups[0] # e.g. '10', 'k'
    multiplier = {'': 1, 'K': 1024, 'M': 1024*1024, 'G': 1024*1024*1024}
    return int(count) * multiplier[unit.upper()]

parser = argparse.ArgumentParser(description='rewind-writer')
parser.add_argument('-f', '--force', action='store_true', help='Overwrite the existing file')
parser.add_argument('-s', '--size', required=True, type=str, help='max size of file to write, e.g. 512, 4K, 1M, 2G')
parser.add_argument('filename', type=str, help='The file to output')

args = parser.parse_args()

# check if the file already exits or not
file_exists = os.path.exists(args.filename)
if file_exists and not args.force:
    raise Exception(f'file {args.filename} exists, if you want to write to it anyway, pass `--force`')

open_mode = 'r+' if file_exists else 'w+'
max_allowed_size = parse_size(args.size)
with open(args.filename, open_mode) as fp:
    fp.seek(0, io.SEEK_END) # go to the end of file
    for line in sys.stdin:
        cur_offset = fp.tell()
        if cur_offset > max_allowed_size:
            fp.seek(0) # rewind
        fp.write(line)
