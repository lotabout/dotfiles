#!/usr/bin/env python3

import sys
arg = ' '.join(sys.argv[1:])

from datetime import datetime

if arg.isdigit():
    timestamp = int(arg)

    if timestamp > 3000000000:
        # > 2065-01-24 13:20:00
        # guess that it is milliseconds
        date = datetime.fromtimestamp(timestamp // 1000)
    else:
        date = datetime.fromtimestamp(timestamp)
elif arg == '':
    # print current datetime
    date = datetime.now()
else:
    # guessing format: 2021-01-01 11:11:11

    date_format = '%Y-%m-%d'
    if ' ' in arg:
        colon_count = arg.count(':')
        if colon_count == 0:
            date_format += ' %H'
        elif colon_count == 1:
            date_format += ' %H:%M'
        else:
            date_format += ' %H:%M:%S'

    date = datetime.strptime(arg, date_format)

print(date.strftime('%Y-%m-%d %H:%M:%S'))
print(int(date.timestamp()))
