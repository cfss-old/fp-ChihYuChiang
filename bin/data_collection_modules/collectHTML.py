# Import libraries
from urllib import request
import os
import re
from bs4 import BeautifulSoup

# Create directory if dataset directory does not already exist
output_fldr = '../data/raw/'
if not os.access(output_fldr, os.F_OK):
    os.mkdir(output_fldr)

# Get html file and save locally
def getHTML(url):
    searchStr = re.search('/[0-9]+-[0-9]+/$', url).group(0)[1:-1] # extract id from url for filenane
    response = request.urlopen(url)
    html = response.read().decode('utf-8')
    content = open(output_fldr + searchStr + '.html', 'w+')
    content.write(html)
    content.close()

# Get review links from category page
reviewLinks = []
def getURL(page):
    response = request.urlopen('http://www.gamespot.com/reviews/?page=' + str(page))
    html = response.read().decode('utf-8')
    html = BeautifulSoup(html, 'html5lib')

    chunks = html.select('#js-sort-filter-results .js-event-tracking')
    for chunk in chunks:
        result = chunk.get('href')
        reviewLinks.append(result)
    return reviewLinks
