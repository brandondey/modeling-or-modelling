
# import arxivscraper.arxivscraper as ax
import pandas as pd

open('arxivscraper-master/arxivscraper.py', 'r')

scraper = ax.Scraper(category='stat', date_from='2020-01-01',t=10, filters={'abstract':['modeling']}) # 'categories':['stat.ml']
output = scraper.scrape()
# print(output)
cols = ('id', 'title', 'categories', 'abstract', 'doi', 'created', 'updated', 'authors')
df = pd.DataFrame(output,columns=cols)
df.to_csv("modelling_output.csv", index=False)
