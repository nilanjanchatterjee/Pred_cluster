# Pred_cluster
Moveapps Github repository: https://github.com/nilanjanchatterjee/Pred_cluster

## Description   
The app predicts time, location and length of predation clusters from tracking data. It uses the input as move/movestack as the output is given in the form of a spreadsheet with the columns with the centroid location of the clusters, number of locations in the cluster, average radius of the clusters and duration of the cluster.

## Documentation   
The App applies sequential clustering algorithm to location data based on input parameters (radius, window_days and minimum location required for clustering) and appends results to the dataframe. The app returns a summary dataframe with attributes for each cluster commonly used as covariates in subsequent modeling efforts. The functions used here for clustering were adapted from *GPSeqClus* package by Clapp et al. (2021) https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/2041-210X.13572. The *GPSeqClus* package has been removed from CRAN and currently the archived version is only available.

## Input data   
move/moveStack in Movebank format and optional threshold speed and window length

## Output data   
move/moveStack in Movebank format

## Artefacts   
*Cluster_summary_output.csv*: csv with the individual id, cluster centroid location, time spent at the cluster and radius of the cluster

## Parameters   

*search_radius*: Search radius (meters) from cluster centroid when building clusters.

*window_days*: window (days) to search for new locations from the most recent location to form one cluster

*clus_min_locs*: Minimum number of locations required to form a cluster. Default used is 2.

*centroid_calc*: Method for recalculating centroids when actively building clusters - e.g., "median" or "mean" (default). 
