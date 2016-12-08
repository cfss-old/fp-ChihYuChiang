# Import libraries
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np

def extractSoup(soup, webSource):
    if webSource == 'GameSpot':
        # Game title
        try: gameTitle = soup.find(class_ = 'related-game__title').a.get_text()
        except: gameTitle = None

        # GS review score
        try: GSScore = soup.find(class_ = 'gs-score__cell').span.get_text()
        except: GSScore = None

        # User review score
        try: userScore = soup.find(class_ = 'breakdown-reviewScores__userAvg').a.get_text()
        except: userScore = None

        # GS review
        GSReview = ""
        try:
            chunks = soup.select('#default-content p')
            for chunk in chunks:
                result = chunk.get_text()
                GSReview = GSReview + ' ' + result
        except:
            GSReview = None

        # Author name
        try: authorName = soup.find(class_ = 'authorCard-name').strong.get_text()
        except: authorName = None

        # Release date
        try: releaseDate = soup.find(class_ = 'pod-objectStats-info__release').span.get_text()
        except: releaseDate = None

        # Game short description
        try: shortDescript = soup.find(class_ = 'pod-objectStats-info__deck').get_text()
        except: shortDescript = None

        # ESRB category
        try: ESRB = soup.find(class_ = 'pod-objectStats__esrb').dt.get_text()
        except: ESRB = None

        # main table
        try:
            df_main = {
                'Game Title'       : gameTitle,
                'GS Score'         : GSScore,
                'User Score'       : userScore,
                'Author Name'      : authorName,
                'Release Date'     : releaseDate,
                'Short Description': shortDescript,
                'Review'           : GSReview,
                'ESRB'             : ESRB
            }
            df_main = pd.DataFrame(df_main, index = [1])
        except: df_main = None

        # ----------------------------------------------------------------------
        # Platforms
        platform = []
        try:
            chunks = soup.select('.clearfix strong')
            for chunk in chunks:
                result = chunk.get_text()
                platform.append(result)
        except: platform = None

        # platform table
        try:
            df_platform = {
                'Game Title': np.repeat(gameTitle, len(platform)),
                'Platform'  : platform
            }
            df_platform = pd.DataFrame(df_platform)
        except: df_platform = None

        # ----------------------------------------------------------------------
        # scrape for developer, publisher, genre
        chunks = soup.find(class_ = 'pod-objectStats-additional').find_all('dd')


        # Developer
        developer = []
        try:
            results = chunks[0].find_all('a')
            for res in results:
                result = res.get_text()
                developer.append(result)
        except: developer = None

        # developer table
        try:
            df_developer = {
                'Game Title': np.repeat(gameTitle, len(developer)),
                'Developer' : developer
            }
            df_developer = pd.DataFrame(df_developer)
        except: df_developer = None

        # ----------------------------------------------------------------------
        # Publisher
        publisher = []
        try:
            results = chunks[1].find_all('a')
            for res in results:
                result = res.get_text()
                publisher.append(result)
        except: publisher = None

        # publisher table
        try:
            df_publisher = {
                'Game Title': np.repeat(gameTitle, len(publisher)),
                'Publisher' : publisher
            }
            df_publisher = pd.DataFrame(df_publisher)
        except: df_publisher = None

        # ----------------------------------------------------------------------
        # genre
        genre = []
        try:
            results = chunks[2].find_all('a')
            for res in results:
                result = res.get_text()
                genre.append(result)
        except: genre = None

        # genre table
        try:
            df_genre = {
                'Game Title': np.repeat(gameTitle, len(genre)),
                'Genre'     : genre
            }
            df_genre = pd.DataFrame(df_genre)
        except: df_genre = None


        return (df_main, df_platform, df_developer, df_publisher, df_genre)
