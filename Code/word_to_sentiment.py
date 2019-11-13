#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from nltk import sent_tokenize

from functools import partial
import itertools
import re

import nltk
nltk.download('punkt')

# In[2]:


total_data = pd.read_csv('reivew_business_user.csv')


# In[3]:


#total_data.review_id


# In[4]:


# text_Madison_raw = total_data[total_data.city == "Madison"][["text","review_id"]]
# text_Madison_raw.head(5)


# In[5]:


city_list = ["Madison","Las Vegas","Phoenix","Charlotte","Pittsburgh"]


# In[6]:


for city in city_list:
    city_new = city.replace(" ","")
    exec('text_{}_filter = total_data[total_data.city =="{}"][[\'text\',\'review_id\']]'.format(city_new,city))
    exec('text_{}_raw = [tuple(x) for x in text_{}_filter.values]'
        .format(city_new,city_new))


# #### change reviews to sentences, and seperate senteences with transition conjuctions (but,however, ... ,etc.)

# In[11]:


def text_to_sentence(text_raw,transition):
    
    def flat_list_in_element(list_of_tuples):
        list_out = []
        for content in list_of_tuples:
            for seperation in content[0]:
                list_out.append((seperation,content[1]))
        return(list_out)

    # content[0] represent text, content[1] represent id
    text = [(sent_tokenize(str(content[0])), content[1]) for content in text_raw]
    text_flatted = flat_list_in_element(text)
    # content[0] represent text, content[1] represent id
    text_trans_split = [(re.split("|".join(transition),content[0]),content[1]) for content in text_flatted]
    text_final = flat_list_in_element(text_trans_split)
    return(text_final)


# In[12]:


transition = ["but","however","despite","nevertheless","even if","even though","yet"]

text_Madison = text_to_sentence(text_Madison_raw,transition)
text_LasVegas = text_to_sentence(text_LasVegas_raw,transition)
text_Phoenix = text_to_sentence(text_Phoenix_raw,transition)
text_Charlotte = text_to_sentence(text_Charlotte_raw,transition)
text_Pittsburgh = text_to_sentence(text_Pittsburgh_raw,transition)


# #### create a function, list all reviews that contains the positive & negative information
# 
# input: list of tuples('text','id')
# 1. from list of tuples to dataframe
# 2. function word_contain show whether word contains 
# 3. function word_contain_pos show whether word and positive adj. contains 
# 4. function word_contain_neg show whether word and negative adj. contains 

# In[121]:


test_word = "burger"
pos_words = ["advanced","amazing","amusing","awesome","balanced","bright",
             "calm","cheerful","classic","confort","considerate","delicious",
             "enjoyable","excellent","extraordinary","fancy","favourite","fine","fluent",
             "friendly","fruity","glad","glorious","good-looking","incredible",
             "graceful","gracious","great","harmonious","healthy","impreessive",
             "intelligent","knowledgable","magnificant","master","nourishing",
             "outstanding","perfect","pleasant","pleased","preferable","preferred",
             "professional","promising","quick","quickly","qualified","reasonable",
             "relaxed","remarkable","resolved","resourceful","respectable",
             "responsible","rich","satisfying","satisfied","skillful","super",
             "tasty","tastable","thrilling","understanding","warm","welcomed",
             "welcoming","worthy","yummy"]
neg_words = ["angry","annoying","anxious","awful","bad","boring","broken","cold",
             "confused","contradictory","damage","damaging","depressed","depressing",
             "dirty","digusting","disaster","dishonest","faulty","fear","foul",
             "fault","harmful","hate","horrible","hurt","hurtful","ignore",
             "ignorant","ill","immature","injure","jealous","messy","missing",
             "misunderstood","misunderstand","offensive","petty","reject","scare",
             "shocking","terrifying","terrible","too spicy","too salty",
             "too sweet","too sour","ugly","undelicious","unfair","unhealthy",
             "unpleasant","unsatisfactory","unwanted","unwelcome","upset","worthless"]


# In[122]:


# return 1 if contain word else 0
word_contain = lambda word,text: 1 if word in text else 0

judge_exist = lambda word,sentence: True if word in sentence else False
# return 1 if contain positive adj + word else 0
word_contain_pos = lambda word,pos,text: 1 if any([judge_exist(pos_word,str.lower(text)) & (word in text) for pos_word in pos]) else 0
# return 1 if contain negative adj + word else 0
word_contain_neg = lambda word,neg,text: 1 if any([judge_exist(neg_word,str.lower(text)) & (word in text) for neg_word in neg]) else 0


# In[8]:


food_list = ["fries", "chips", "onion rings", "nachos", "wings", "burger",
              "monzarella stick", "quesadillas", "cheese", "sandwitch",
              "pizza", "taco", "popcorn", "bacon", "slider", "fried", "shrimp",
              "salads", "dip", "nugget", "oysters", "sauce", "peanut", "chicken"]
drink_list = ["beer", "wine", "cocktail", "spark", "spirit", "rum", "whiskey",
               "vodka", "tequila", "brandy", "gin", "negroni", "daiquiri",
               "manhattan", "mojito", "margarita", "martini"]
service_list = ["waiter", "waitress", "park", "wifi", "restroom", "manner", "price",
                "reservation", "seat", "booking", "employee" ]
atomsphere_list = ["music", "band", "light", "environment", "atomsphere"]

word_list = food_list + drink_list + service_list+atomsphere_list


# #### Create DataFrame for each city

# In[24]:


for city in city_list:
    city_new = city.replace(" ","")
    exec('text_{}_df = pd.DataFrame(text_{},columns=[\"text\",\"id\"])'.format(city_new,city_new))


# #### Add columns for all variables to be used in the count

# In[25]:


for city in city_list:
    city_new = city.replace(" ","")
    for word in word_list:
        word_new = word.replace(" ","_")
        exec('{}_contain = partial(word_contain,"{}")'.format(word_new,word))
        exec('{}_contain_pos = partial(word_contain_pos,"{}",pos_words)'.format(word_new,word))
        exec('{}_contain_neg = partial(word_contain_neg,"{}",neg_words)'.format(word_new,word))
        exec('input_text = text_{}_df.text'.format(city_new))
        # create list and add them to dataframe
        exec('output = list(map({}_contain,input_text))'.format(word_new))
        exec('output_pos = list(map({}_contain_pos,input_text))'.format(word_new))
        exec('output_neg = list(map({}_contain_neg,input_text))'.format(word_new))
        exec('text_{}_df["{}"] = output'.format(city_new,word_new))
        exec('text_{}_df["{}_pos"] = output_pos'.format(city_new,word_new))
        exec('text_{}_df["{}_neg"] = output_neg'.format(city_new,word_new))
        del(output,output_pos,output_neg)
    del(input_text,city_new)


# #### Output the result into csv
# 
# Output format:<br>
# text: the analyzed sentence<br>
# ID: the review ID<br>
# for word in word_list:<br>
# * word: an indicator of whether the word is in the sentence<br>
# * &emsp word_pos: an indicator of whether the word and positive adj. is in the sentence<br>
# * emsp word_neg: an indicator of whether the word and negative adj. is in the sentence<br>

# In[26]:


for city in city_list:
    city_new = city.replace(" ","")
    exec('text_{}_df.to_csv(\"text_{}.csv\")'.format(city_new,city_new))


# In[28]:


for city in city_list:
    city_new = city.replace(" ","")
    exec('del text_{}_df'.format(city_new,city_new))


