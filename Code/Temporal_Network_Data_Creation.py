
"""

This code calculates the cosine similarity between
all pairs of entries on the same date.

The ouputs are two JSON files:

- network_df.json contains a list of all the edges
  with their date and cosine similarity score.
  
- entry_data_sentiments.json contains a list of all
  entries with their corresponding diary node number
  and sentiment score.

Last Updated
------------
31/05/2024

"""


## Load Packages ----

import os
import re
import math
import json
import nltk
import string
import random
import statistics
import numpy as np
import pandas as pd
import pathpy as pp
from itertools import count
from scipy.signal import argrelextrema
from nltk.stem import WordNetLemmatizer
from sentence_transformers import SentenceTransformer
from nltk.sentiment import SentimentIntensityAnalyzer
from sklearn.metrics.pairwise import cosine_similarity


## Working Directory ----

wd = "Temporal Network Analysis/Data/"


## Load Diary Metadata ----

diary_metadata = pd.read_csv(wd + "SLNSW_Diary_Metadata.csv")  # Read in the csv containing the diary metadata


## Load and Reformat Diary Data ----

data_directory = wd + "Diaries - Clean/"  # Path for the directory where the clean diary data is stored

diary_files = [file for file in os.listdir(data_directory) if file.endswith('.json')]  # Get a list of the diary files

entry_data = pd.DataFrame(data = [], columns = ['Document Name', 'Date', 'Entry'])  # Create an empty dataframe to store all of the diary entries

author_list = []  # Create an empty list to store the names of the authors

document_node = 1  # Initialise a number to create the document nodes

exclusion_list = ['Diary_0003', 'Diary_0119', 'Diary_0135', 'Diary_0144', 'Diary_0146', 'Diary_0147', 'Diary_0199', 'Diary_0216', 'Diary_0239', 'Diary_0256', 'Diary_0298', 'Diary_0304', 'Diary_0320', 'Diary_0417', 'Diary_0421']  # List of diaries to exclude as they are out of order

for filename in diary_files:  # For every diary

    document_name = filename[14:-5]  # Find the name of the diary

    if document_name not in exclusion_list:  # If this isn't a diary we need to exclude

        filtered_metadata = diary_metadata[diary_metadata['Document Name'] == document_name]  # Find metadata for diary we are working with

        author_name = filtered_metadata.iloc[0,1] + ' ' + filtered_metadata.iloc[0,2]  # Get authors name by combining their first and last name from the relevant metadata

        author_list.append(author_name)  # Add the author's name to the list of author names

        entries = pd.read_json(data_directory + filename)  # Read in the diary's entries

        entries.insert(1, 'Author', author_name)  # Add author name

        entries.insert(1, 'Node', document_node)  # Add document node

        entry_data = pd.merge(entry_data, entries, how = 'outer')  # Add these entries to the dataframe containing all entries

        document_node = document_node + 1  # Increase count on document node

entry_data = entry_data[entry_data['Date'] != 'unknown']  # Remove entries with unknown date

entry_data = entry_data[entry_data['Entry'] != '']  # Remove empty entries

entry_data_sorted = entry_data

entry_data_sorted.sort_values(by = ['Date'])  # Sort the entries by date


## Get the Entry Sentiment ----

sia = SentimentIntensityAnalyzer()  # Load VADER

entry_data_sentiments = entry_data_sorted

entry_data_sentiments.insert(5, 'Sentiment', 0)  # Add a column for sentiment

for row in range(entry_data_sentiments.shape[0]):  # For every entry

    sentiment = sia.polarity_scores(entry_data_sentiments.loc[entry_data_sentiments.index[row], 'Entry'])  # Calculate the sentiment using VADER

    entry_data_sentiments.loc[entry_data_sentiments.index[row], 'Sentiment'] = sentiment['compound']  # Add the sentiment score to our dataframe of entries


## Find the Cosine Similarity Score for Each Edge ----

model = SentenceTransformer('bert-base-nli-mean-tokens')  # Load the BERT Transformer Model

network_list = []  # Initialise an empty list to store the network edges in

entry_list = entry_data_sorted['Entry'].tolist() # Create a list of all entries

sen_embeddings = model.encode(entry_list) # Convert entries to vector representation with BERT

print(entry_data_sorted.shape[0])

for i in range(entry_data_sorted.shape[0]):  # For every entry

    print(i)

    date_i = entry_data_sorted.loc[entry_data_sorted.index[i], 'Date']  # Get the date

    node_i = entry_data_sorted.loc[entry_data_sorted.index[i], 'Node']  # Get the node number

    for j in range(i+1, entry_data_sorted.shape[0]):  # For every entry after i

        date_j = entry_data_sorted.loc[entry_data_sorted.index[j], 'Date']  # Get the entry date

        if date_i == date_j:  # If the entry dates are equal and neither text is empty

            node_j = entry_data_sorted.loc[entry_data_sorted.index[j], 'Node']  # Get the node number

            sim = cosine_similarity([sen_embeddings[i]], [sen_embeddings[j]])  # Calculate the cosine similarity of the two entries

            network_list.append([node_i, node_j, date_i, sim[0][0]])  # Add the edge to our network list

network_df = pd.DataFrame(network_list, columns = ['From', 'To', "Date", 'Sim'])  # Create a pandas dataframe of our network list


## Save the Network Data ----

with open(wd + 'entry_data_sentiments.json', 'w', encoding='utf-8') as file:
    entry_data_sentiments.to_json(file, force_ascii=False)

with open(wd + 'network_df.json', 'w', encoding='utf-8') as file:
    network_df.to_json(file, force_ascii=False)








