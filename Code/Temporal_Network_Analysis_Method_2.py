
"""

This code creates the network of diaries and calculates
the metrics for each diary, given the similarity threshold.

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

data_dir = 'Temporal Network Analysis/Data/'

network_df = pd.read_json(data_dir + 'network_df.json', encoding='utf-8')  # Read in the network data frame

entry_data = pd.read_json(data_dir + 'entry_data_sentiments.json', encoding='utf-8')  # Read in the entry data


## Remove Edges with Similarity below the Threshold ----

similarity_threshold = 0.736 # Set similarity threshold

network_subset_df = network_df[network_df['Sim'] >= similarity_threshold]  # Only keep edges with similarity >= threshold


## Create Static Network ----

edge_list = []  # Create empty list to store edge information

unique_destinations = list(network_subset_df['To'].unique())  # List of unique nodes in the "To" column

unique_nodes = list(entry_data['Node'].unique())  # List of unique nodes in the data set

for t in unique_nodes:  # For every unique node

    t_subset = network_subset_df[network_subset_df['To'] == t]  # Subset of every edge in the network with our "To" node

    unique_origins = list(t_subset['From'].unique())  # Create a list of unique "From" nodes in this subset

    for f in unique_nodes:  # For every unique "From" node

        f_subset = t_subset[t_subset['From'] == f]  # Subset of every edge in our subset with our "From" node as origin

        count1 = f_subset.shape[0]  # The number of times we have this From/To combination

        f_subset2 = network_subset_df[network_subset_df['To'] == f]  # Subset of every edge in the network with our "From" node as destination

        t_subset2 = f_subset2[f_subset2['From'] == t]  # Subset of every edge in the network with our "To" node as origin

        count2 = t_subset2.shape[0]  # The number of times we have this To/From combination

        count = count1 + count2  # Combined count of this combination

        t1 = min(t,f)  # The smaller node number out of our "To" and "From" nodes

        f1 = max(t,f)  # The larger node number out of our "To" and "From" nodes

        if count > 0:  # If at least one set of similar entries:

            edge_list.append([t1, f1, count])  # Add this edge and count to our edge list

## Removes Duplicates:

edge_list2 =[]  # Create a second empty edge list

for elem in edge_list:  # For every element in our original edge list
    
    if elem not in edge_list2:  # If the element is not in our second edge list

        edge_list2.append(elem)  # Add it to the second edge list


g = nx.Graph()  # Create empty NetworkX network

g.add_nodes_from(unique_nodes)  # Add nodes to network

g.add_weighted_edges_from(edge_list2)  # Add weighted edges to network


## Network Summary Statistics ----

### Summary: Number of Nodes
##
##num_nodes = g.number_of_nodes()  # How many nodes are in the graph
##
##print("Num Nodes: " + str(num_nodes))
##
### Summary: Number of Nodes with Edges
##
##num_nodes_edges = 0
##
##for n in unique_nodes:  # For every node
##
##    if g.degree[n] > 0:  # If the node has at least one edge
##
##        num_nodes_edges = num_nodes_edges + 1  # Increase the count
##
##    else:
##
##        print(n)
##        
##print("Num Nodes with Edges: " + str(num_nodes_edges))
##
### Summary: Number of Edges
##
##num_edges = g.number_of_edges()  # How many edges are in the graph
##
##print("Num Edges: " + str(num_edges))
##
### Summary: Density
##
##graph_density = nx.density(g)  # Ratio of edges to potential edges
##
##print("Density: " + str(graph_density))
##
### Summary: Components
##
##num_components = nx.number_connected_components(g)  # Number of connected components
##
##print("Num Connected Components: " + str(num_components))
##
### Summary: Clustering
##
##graph_clustering = nx.transitivity(g)  # Ratio of triangles to triads
##
##print("Clustering (Transitivity): " + str(graph_clustering))


## Investigate diaries -----

metrics_summary = pd.DataFrame()

ec_weighted = nx.eigenvector_centrality(g, weight = 'weight')  # Calculate the vector of eigenvalue centrality scores

for node in unique_nodes:  # For every diary

    # Summary: Weighted Degree

    weighted_node_degree = g.degree(weight = 'weight')[node]  # Calculate the weighted degree

    metrics_summary = pd.concat([metrics_summary, pd.DataFrame({'Node' : node,
                                                                'Weighted Degree' : weighted_node_degree,
                                                                'Weighted EC' : ec_weighted[node]},index = [0])]).reset_index(drop = True)


metrics_summary.to_csv(data_dir + "Method 2 Metrics/Method_2_Metrics_Summary_Threshold_" + str(similarity_threshold) + ".csv", index=False)


