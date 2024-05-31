
"""

This code calculates the metrics for the static
graph on each date given the similarity threshold.

The output of this code is a csv containing the metrics.

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
import igraph
import random
import statistics
import numpy as np
import pandas as pd
import pathpy as pp
import networkx as nx
from itertools import count
import matplotlib.pyplot as plt
from teneto import TemporalNetwork
from scipy.signal import argrelextrema
from nltk.stem import WordNetLemmatizer
from sentence_transformers import SentenceTransformer
from nltk.sentiment import SentimentIntensityAnalyzer
from teneto.communitydetection import temporal_louvain
from sklearn.metrics.pairwise import cosine_similarity
from networkx.algorithms.community.centrality import girvan_newman


## Load Network Data ----

data_dir = 'Temporal Network Analysis/Data/'  # Directory where Data is Stored

network_df = pd.read_json(data_dir + 'network_df.json', encoding = 'utf-8')  # Read in the network data frame

entry_data = pd.read_json(data_dir + 'entry_data_sentiments.json', encoding = 'utf-8')  # Read in the entry data


## Remove Edges with Similarity below the Threshold ----

similarity_threshold = 0.736  # Set similarity threshold

network_subset_df = network_df[network_df['Sim'] >= similarity_threshold]  # Only keep edges with similarity >= threshold


# Create a summary for each day with chosen metrics:

metrics_summary = pd.DataFrame()  # Initialise an empty data frame to store the metrics for each day

unique_dates = sorted(list(network_subset_df['Date'].astype('int').unique()))  # Get a list of unique dates in the network sorted from smallest to largest

for date in unique_dates:  # For every unique date

    network_date_subset = network_subset_df[network_subset_df['Date'] == date]  # Subset the network for just nodes/edges on that date

    network_date_subset = network_date_subset[['To', 'From', 'Sim']]  # Keep only necessary columns

    network_date_subset_list = network_date_subset.values.tolist()  # Convert data frame to list
  
    entries_date_subset = entry_data[entry_data['Date'] == date]  # Subset the entry data for just the chosen date

    unique_nodes = list(entries_date_subset['Node'].unique())  # List of unique nodes for the chosen date

    node_attribute = dict(zip(entries_date_subset['Node'], entries_date_subset['Sentiment']))  # Get node attribute (sentiment)

    g = nx.Graph()  # Create NetworkX graph

    g.add_nodes_from(unique_nodes)  # Add nodes to the network

    nx.set_node_attributes(g, node_attribute, "Sentiment")  # Add the node attribute of sentiment

    g.add_weighted_edges_from(network_date_subset_list)  # Add weighted edges to the network

    # Summary: Number of Nodes

    num_nodes = g.number_of_nodes()  # How many nodes are in the graph

    # Summary: Number of Edges

    num_edges = g.number_of_edges()  # How many edges are in the graph

    # Summary: Density

    graph_density = nx.density(g)  # Ratio of edges to potential edges

    # Summary: Transitivity

    graph_transitivity = nx.transitivity(g)  # Ratio of triangles to triads

    # Summary: Number of Nodes with Edges

    g.remove_nodes_from(list(nx.isolates(g)))  # Remove nodes that have no edges

    num_nodes_wth_edges = g.number_of_nodes()  # How many nodes have esges in the graph

    # Combining metrics together

    metrics_summary = pd.concat([metrics_summary, pd.DataFrame({'Date' : date,
                                                                'Num Nodes' : num_nodes,
                                                                'Num Nodes with Edges' : num_nodes_wth_edges,
                                                                'Num Edges' : num_edges,
                                                                'Density' : graph_density,
                                                                'Transitivity' : graph_transitivity}, index = [0])]).reset_index(drop = True)


metrics_summary.to_csv(data_dir + "Method 1 Metrics/Method_1_Metrics_Summary_Threshold_" + str(similarity_threshold) + ".csv", index = False)  # Save metrics summary to CSV

