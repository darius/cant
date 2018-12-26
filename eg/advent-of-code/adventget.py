#!/usr/bin/env python
"""
Adapted from https://github.com/ambertests/advent_of_code_2018/blob/master/aoc_day01.py
For other ways see also https://old.reddit.com/r/adventofcode/comments/a2lesz/2018_day_3_solutions/eb1hd1z/
"""

import os, requests

# Grab the session cookie from the https://adventofcode.com/2018 site:
# right-click, Inspect, Application tab, Cookies, session.
session = os.environ['ADVENT_SESSION']

def download(day):
    text = fetch(day)
    with open('advent%02d' % day, 'w') as f:
         f.write(text)

def fetch(day):
    url = 'https://adventofcode.com/2018/day/%d/input' % day
    headers = {
	'cookie': "session=" + session
    }
    input = requests.request('GET', url, headers=headers)
    return input.text

if __name__ == '__main__':
    import sys
    assert len(sys.argv) == 2
    download(int(sys.argv[1]))
