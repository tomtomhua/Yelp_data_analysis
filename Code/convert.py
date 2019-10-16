import pandas as pd
import json

business = pd.read_json('data/business.json', lines=True)
business.to_csv('business.csv')

tip = pd.read_json('data/tip.json', lines=True)
tip.to_csv('tip.csv')

with open("data/user.json", encoding="utf-8") as json_file:      
    data = json_file.readlines()
    data = list(map(json.loads, data)) 
pd.DataFrame(data).to_csv('user.csv')

with open("data/review.json", encoding="utf-8") as json_file:      
    data = json_file.readlines()
    data = list(map(json.loads, data)) 
pd.DataFrame(data).to_csv('review.csv')