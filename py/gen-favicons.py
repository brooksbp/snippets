#!/usr/bin/env python

# Copyright (c) 2013 Brian Brooks. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

__version__ = '0.01-dev'

from bs4 import BeautifulSoup
from urllib.parse import urlparse, urljoin
import requests
import re
from PIL import Image     # pip install pillow
import io
import sys
import time


def get_req_text(url):
    try:
        r = requests.get(url, timeout=2)
        return r.text
    except:
        return None

def make_soup(url):
    r = get_req_text(url)
    return None if r is None else BeautifulSoup(r)

def crawl(url, workfn, nextfn, cur_depth, max_depth):
    if cur_depth >= max_depth:
        return []
    s = make_soup(url)
    if s is None:
        return []
    w = workfn(s)
    l = nextfn(s)
    for link in l:
        w += crawl(link, workfn, nextfn, cur_depth + 1, max_depth)
    return w

def crawler(url, workfn, nextfn, depth):
    # robots.txt
    s = make_soup(url)
    if s is None:
        return []
    w = workfn(s)
    l = nextfn(s)
    if (depth > 1):
        for link in l:
            w += crawl(link, workfn, nextfn, 1, depth)
    return w


print('Generating list of pages...')
pages = []

def workfn_hackernews(soup):
    '''get all the links that we will pull favicons from'''
    w = []
    for a in soup.findAll('a', attrs={'href': re.compile("^http://")}):
        u = urlparse(a['href'])
        w.append('http://' + u.netloc)
    return w

def nextfn_hackernews(soup):
    '''filter soup for the next links to crawl'''
    time.sleep(31) # should derive from robots.txt
    for a in soup.findAll('a'):
        if a.string == 'More':
            return [urljoin('http://news.ycombinator.com/', a['href'])]

pages += crawler(url='http://news.ycombinator.com/',
                 workfn=workfn_hackernews,
                 nextfn=nextfn_hackernews,
                 depth=5)

def workfn_reddit(soup):
    '''get all the links that we will pull favicons from'''
    w = []
    for a in soup.findAll('a', attrs={'href': re.compile("^http://"), 'class': 'title'}):
        u = urlparse(a['href'])
        w.append('http://' + u.netloc)
    return w

def nextfn_reddit(soup):
    '''filter soup for the next links to crawl'''
    time.sleep(21) # should derive from robots.txt
    for a in soup.findAll('a'):
        if a.string == 'next ›':
            return [a['href']]
    return []

pages += crawler(url='http://www.reddit.com/',
                 workfn=workfn_reddit,
                 nextfn=nextfn_reddit,
                 depth=10)

REDDIT_URL = 'http://www.reddit.com/'
pages.append(REDDIT_URL)
subreddits = [
    'programming', 'gamedev', 'investing',
    'electronics', 'Python', 'networking',
    'arduino', 'haskell', 'MachineLearning',
    'finance',
    'pics', 'funny', 'politics', 'gaming',
    'worldnews', 'news', 'wtf', 'technology',
    'science', 'music', 'videos', 'aww',
    'movies', 'bestof', 'trees', 'gifs',
    'minecraft', 'nba', 'starcraft', 'historyporn',
    'soccer', 'facepalm', 'jokes', 'harrypotter',
    'nfl', 'hockey', 'celebs', 'tf2', 'apple',
    'baseball', 'guns', 'hiphopheads', 'wow',
    'food', 'getmotivated', 'starwars', 'wallpapers'
]
for subreddit in subreddits:
    pages.append(REDDIT_URL + 'r/' + subreddit)

single_urls = [
    'http://online.wsj.com/',
    'http://www.marketwatch.com/',
    'http://online.barrons.com/',
    'http://www.nytimes.com/',
    'http://seekingalpha.com/',
    'https://news.google.com/'
]
pages += single_urls

def unique_list(l):
    seen = set()
    seen_add = seen.add
    return [ x for x in l if x not in seen and not seen_add(x) ]

unique_pages = unique_list(pages)

print(unique_pages)
print('Generating image..')

rows = 8
max_imgs = 640
assert max_imgs % rows == 0
cols = max_imgs / rows
rows_px = int(rows*16)
cols_px = int(cols*16)

the_img = Image.new(mode='RGB',
                    size=(cols_px, rows_px),
                    color=(255, 255, 255, 0))

def save_img():
    global the_img
    the_img.save('woot.jpg', 'JPEG', quality=95)

x = 0
y = 0

def get_paste_coordinates():
    global x,y
    return (x,y)
def update_paste_coordinates():
    global x,y,rows
    if y == (rows - 1) * 16:
        y = 0
        x = x + 16
    else:
        y = y + 16

icons = set()
i = 0

# could do some caching to fs somewhere here...

for page in pages:
    try:
        resp = requests.get(page, timeout=2)
    except:
        continue
    soup = BeautifulSoup(resp.text)
    for link in soup.findAll('a', attrs={'href': re.compile("^http://")}):
        url = urlparse(link.get('href'))
        if url.netloc not in icons:
            icons.add(url.netloc)
            favicon_uri = 'http://' + url.netloc + '/favicon.ico'
            try:
                favicon_resp = requests.get(favicon_uri, timeout=2)
            except:
                continue
            if 'content-type' in favicon_resp.headers:
                if favicon_resp.headers['content-type'] == 'image/x-icon':
                    try:
                        img = Image.open(io.BytesIO(favicon_resp.content))
                        img.thumbnail((16,16))
                        the_img.paste(img, get_paste_coordinates())
                        update_paste_coordinates()
                        i = i + 1
                        if i >= max_imgs:
                            # just save the_img and bail...
                            save_img()
                            print('hit max_imgs=' + str(max_imgs) + ', image written, bailing...')
                            sys.exit()
                    except IOError:
                        print('IOError: ' + favicon_uri)

save_img()
