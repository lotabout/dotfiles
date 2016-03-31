#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime
import urllib
import socket

import os
import sys
import re
from subprocess import call

#dir like NationalGeographic/year/month/
today =  datetime.today()
root = os.path.join(os.path.expanduser('~'), 'Pictures/pod/')
STOREDIR = root

def setWallPaper(filename):
    call(['fbsetbg', filename])

def getPicture(fname):
    sock = urllib.urlopen("http://photography.nationalgeographic.com/photography/photo-of-the-day/")
    htmlSource = sock.read()
    sock.close()

    p = re.compile('//images.nationalgeographic.com/.*(?:1600x1200|990x742).*\.jpg')
    match = p.findall(htmlSource)
    print match
    urllib.urlretrieve('http:' + match[0], fname)

def setWallpaperOfToday():
    filename = STOREDIR + str(today.strftime('%Y%m%d')) + '.jpg'
    print filename
    getPicture(filename)
    setWallPaper(filename)

setWallpaperOfToday()
