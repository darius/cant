#!/usr/bin/env python
"""
Adapted from https://github.com/ambertests/advent_of_code_2018/blob/master/aoc_day01.py
For other ways see also https://old.reddit.com/r/adventofcode/comments/a2lesz/2018_day_3_solutions/eb1hd1z/
To be run from ../.. (the main Cant dir).
"""

import os, requests

# Grab the session cookie from the https://adventofcode.com/ site:
# right-click, Inspect, Application tab, Cookies, session.
session = os.environ['ADVENT_SESSION']

def download(day, year):
    text = fetch(day, year)
    with open('examples/advent-of-code/%d/data/%02d.in' % (year, day), 'w') as f:
         f.write(text)

def fetch(day, year):
    url = 'https://adventofcode.com/%d/day/%d/input' % (year, day)
    headers = {
	'cookie': "session=" + session
    }
    input = requests.request('GET', url, headers=headers)
    return input.text

if __name__ == '__main__':
    import sys
    year = 2020                 # default
    if sys.argv[1:2] == ['-y'] and 3 <= len(sys.argv):
        year = int(sys.argv[2])
        del sys.argv[1:3]
    if len(sys.argv) != 2:
        raise Exception("usage: %s [-y year] day" % sys.argv[0])
    day = int(sys.argv[1])
    download(day, year)
