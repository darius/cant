#!/usr/bin/env python
"""
Adapted from https://github.com/ambertests/advent_of_code_2018/blob/master/aoc_day01.py
For other ways see also https://old.reddit.com/r/adventofcode/comments/a2lesz/2018_day_3_solutions/eb1hd1z/
To be run from ../.. (the main Squeam dir).
TODO: command-line arg to choose the year
"""

import os, requests

# Grab the session cookie from the https://adventofcode.com/ site:
# right-click, Inspect, Application tab, Cookies, session.
session = os.environ['ADVENT_SESSION']
default_year = 2015

def download(day, year=default_year):
    year = year % 100
    text = fetch(day)
    with open('eg/advent-of-code/%d/data/%02d.in' % (year, day), 'w') as f:
         f.write(text)

def fetch(day, year=default_year):
    url = 'https://adventofcode.com/%d/day/%d/input' % (year, day)
    headers = {
	'cookie': "session=" + session
    }
    input = requests.request('GET', url, headers=headers)
    return input.text

if __name__ == '__main__':
    import sys
    if len(sys.argv) != 2:
        raise Exception("usage: %s day" % sys.argv[0])
    download(int(sys.argv[1]))
