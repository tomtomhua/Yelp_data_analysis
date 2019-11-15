#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from nltk import sent_tokenize

from functools import partial
import itertools
import re

from scipy import stats
from scipy.stats import chi2_contingency



# Read the total merged data, which is too big we can't put it in git, its link is in  data file fold's readme. 
total_data = pd.read_csv('reivew_business_user.csv')




city_list = ["Madison","Las Vegas","Phoenix","Charlotte","Pittsburgh"]




# Create word list that we used to draw information from sentences.
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

word_list = food_list + drink_list + service_list + atomsphere_list




# read the data that convert the sentences into sentiments.


for city in city_list:
    city_new = city.replace(" ","")
    exec('text_{}_df = pd.read_csv(\"text_{}.csv\")'.format(city_new,city_new))




# Merge senteces back into reviews according to review ids. 


for city in city_list:
    city_new = city.replace(" ","")
    exec('word_list_all = list(text_{}_df.columns[3:])'.format(city_new))
    columns_list = ['review_id','review_stars','business_id']+word_list_all
    text_list = list()
    text_array = np.array(text_list)
    exec('review_id_list = pd.unique(text_{}_df.id)'.format(city_new))
    for review_id in review_id_list:
        exec('temp_text_df = text_{}_df[text_{}_df.id == review_id]'.format(city_new,city_new))
        text_list.append(review_id)
        text_list.append(total_data[total_data.review_id == review_id].review_stars.values[0])
        text_list.append(total_data[total_data.review_id == review_id].business_id.values[0])
        text_array = np.r_[text_array,sum(temp_text_df[temp_text_df.id==review_id].values[:,3:])]
    
    text_array_reshaped = text_array.reshape(int(len(text_array)/(len(columns_list)-3)),len(columns_list)-3)
    text_list_reshaped = np.array(text_list).reshape(int(len(text_list)/3),3)
    text_combined = np.c_[text_list_reshaped,text_array_reshaped]
    exec('sentiment_stars_{} = pd.DataFrame(text_combined, columns=columns_list )'.format(city_new))
    exec('sentiment_stars_{}.to_csv("sentiment_stars_{}.csv")'.format(city_new,city_new))


# #### Using t-test, chisq test to test words in word list whether they have significant influence on review_stars.

# #### Using fisher's exact test to do the enrichment test, thus, we can find how significant it is to improve (decrease) review star when a word 
# #### is positively (negatively) mentioned. 


for city in city_list:
    city_new = city.replace(" ","")
    city_t_p_value = list()
    city_chisq_p_value = list()
    city_enrich_pos_p_value = list()
    city_enrich_neg_p_value = list()
    for word in word_list:
        word_new = word.replace(" ","_")
        exec('pos_stars = sentiment_stars_{}[sentiment_stars_{}.{}_pos == 1].review_stars.astype(\'float64\')'.format(city_new,city_new,word_new))
        exec('neg_stars = sentiment_stars_{}[sentiment_stars_{}.{}_neg == 1].review_stars.astype(\'float64\')'.format(city_new,city_new,word_new))
        if len(pos_stars)>0 and len(neg_stars)>0:
            t_p_value = stats.ttest_ind(pos_stars,neg_stars).pvalue
        else:
            t_p_value = 'NA'
        city_t_p_value.append(t_p_value)

    
        exec('high_pos = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') > 4].{}_pos.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('high_neg = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') > 4].{}_neg.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('low_pos = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') < 3].{}_pos.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('low_neg = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') < 3].{}_neg.astype(\'int\'))'.format(city_new,city_new,word_new))
        if (high_pos+high_neg)==0 or (low_pos+low_neg)==0 or (high_pos+low_pos)==0 or (high_neg+low_neg)==0:
            chisq_p_value = 'NA'
        else:
            chisq_p_value = stats.chi2_contingency(np.array([high_pos,high_neg,low_pos,low_neg]).reshape((2,2)))[1]
        city_chisq_p_value.append(chisq_p_value)
    
    
        exec('high_pos = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') > 3].{}_pos.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('high_non = sum(1-sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') > 3].{}.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('low_pos = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') <= 3].{}_pos.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('low_non = sum(sentiment_stars_{}[1-sentiment_stars_{}.review_stars.astype(\'float64\') <= 3].{}.astype(\'int\'))'.format(city_new,city_new,word_new))
        if (high_pos+high_non)==0 or (low_pos+low_non)==0 or (high_pos+low_pos)==0 or (high_non+low_non)==0:
            chisq_p_value = 'NA'
        else:
            pos_p_value = stats.fisher_exact(np.array([high_pos,high_non,low_pos,low_non]).reshape((2,2)))[1]
        city_enrich_pos_p_value.append(pos_p_value)

    
        exec('high_non = sum(1-sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') > 3].{}.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('high_neg = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') > 3].{}_neg.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('low_non = sum(1-sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') <= 3].{}.astype(\'int\'))'.format(city_new,city_new,word_new))
        exec('low_neg = sum(sentiment_stars_{}[sentiment_stars_{}.review_stars.astype(\'float64\') <= 3].{}_neg.astype(\'int\'))'.format(city_new,city_new,word_new))
        if (high_non+high_neg)==0 or (low_non+low_neg)==0 or (high_non+low_non)==0 or (high_neg+low_neg)==0:
            chisq_p_value = 'NA'
        else:
            neg_p_value = stats.chi2_contingency(np.array([high_non,high_neg,low_non,low_neg]).reshape((2,2)))[1]
        city_enrich_neg_p_value.append(neg_p_value)    

    
    data = {'word':word_list,
           't_p_value':city_t_p_value,
           'chisq_p_value':city_chisq_p_value,
           'enrich_pos_p_value':city_enrich_pos_p_value,
           'enrich_neg_p_value':city_enrich_neg_p_value}
    exec('{}_test_df = pd.DataFrame(data)'.format(city_new))
    exec('{}_test_df.to_csv(\"{}_test_df.csv\")'.foramt(city_new,city_new))     


# #### Converge reviews back into business



word_list_all = list(sentiment_stars_Madison.columns[4:])
word_count_list = list()
for word in word_list_all:
    exec('word_count_list.append(\'{}_count\')'.format(word))
columns_list = ['business_id','avg_review_stars','review_count']+word_count_list


#
#
#text_list = list()
#text_array = np.array(text_list)
#business_id_list = pd.unique(sentiment_stars_Madison.business_id)
#for business_id in business_id_list:
#    temp_text_df = sentiment_stars_Madison[sentiment_stars_Madison.business_id == business_id]
#    text_list.append(business_id)
#    text_list.append(sum(temp_text_df.review_stars.values)/len(temp_text_df.review_stars.values))
#    text_list.append(len(temp_text_df.review_stars.values))
#    text_array = np.r_[text_array,sum(temp_text_df[temp_text_df.business_id == business_id].values[:,4:])]
#text_array_reshaped = text_array.reshape(int(len(text_array)/(len(columns_list)-3)),len(columns_list)-3)
#text_list_reshaped = np.array(text_list).reshape(int(len(text_list)/3),3)
#text_combined = np.c_[text_list_reshaped,text_array_reshaped]
#business_count_Madison = pd.DataFrame(text_combined, columns=columns_list )
#business_count_Madison.to_csv("business_count_Madison.csv")
#


for city in city_list:
    city_new = city.replace(" ","")
    exec('word_list_all = list(sentiment_stars_{}.columns[4:])'.format(city_new))
    word_count_list = list()
    for word in word_list_all:
        exec('word_count_list.append(\'{}_count\')'.format(word))
    columns_list = ['business_id','avg_review_stars','review_count']+word_count_list
    text_list = list()
    text_array = np.array(text_list)
    exec('business_id_list = pd.unique(sentiment_stars_{}.business_id)'.format(city_new))
    for business_id in business_id_list:
        exec('temp_text_df = sentiment_stars_{}[sentiment_stars_{}.business_id == business_id]'.format(city_new,city_new))
        text_list.append(business_id)
        text_list.append(sum(temp_text_df.review_stars.values)/len(temp_text_df.review_stars.values))
        text_list.append(len(temp_text_df.review_stars.values))
        text_array = np.r_[text_array,sum(temp_text_df[temp_text_df.business_id == business_id].values[:,4:])]
    text_array_reshaped = text_array.reshape(int(len(text_array)/(len(columns_list)-3)),len(columns_list)-3)
    text_list_reshaped = np.array(text_list).reshape(int(len(text_list)/3),3)
    text_combined = np.c_[text_list_reshaped,text_array_reshaped]
    exec('business_count_{} = pd.DataFrame(text_combined, columns=columns_list)'.format(city_new))
    exec('business_count_{}.to_csv(\"business_count_{}.csv\")'.format(city_new,city_new))

