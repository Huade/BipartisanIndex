
# coding: utf-8

# In[3]:

from unidecode import unidecode
import pandas as pd

scores = pd.read_csv("Score.csv")

# remove all the accents
for i in range(len(scores)):
    scores.LastName[i] = unidecode(unicode(scores.LastName[i]))
    scores.FirstName[i] = unidecode(unicode(scores.FirstName[i]))

scores.to_csv("Score_no_accent.csv")

