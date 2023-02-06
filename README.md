# Pred_cluster
Moveapps Github repository: https://github.com/nilanjanchatterjee/Pred_cluster

## Description
The app predicts the time, location and duration of predation clusters from tracking data. The output is given in the form of a map of cluster locations and a table containing the centroid location, number of locations, average radius, and duration of each identified cluster.

## Documentation
The App applies sequential clustering algorithm to location data based on input parameters (radius, window_days and minimum location required for clustering) and appends results to the dataframe. The app returns a summary dataframe with attributes for each cluster commonly used as covariates in subsequent modeling efforts, a map of cluster locations, and cluster IDs added to the movestack output. The functions used here for clustering were adapted from *GPSeqClus* package by [Clapp et al. (2021)](https://doi.org/10.1111/2041-210X.13572). The *GPSeqClus* package has been removed from CRAN and currently the archived version is only available https://cran.r-project.org/src/contrib/Archive/GPSeqClus/. See [Cluff and Mech (2023)](https://doi.org/10.1002/2688-8319.12204) for a field test of the *GPSeqClus* package.

Notes:
* Input parameters should reflect both the expected behavior of the study animals and the accuracy and sampling frequency of the tracking data. Use the [Track Summary Statistics App](https://www.moveapps.org/apps/browser/8ca03c5a-d61a-466d-860b-11beb6bf6404) to assess fix intervals for each animal.  
* To restrict the analysis and alerts to recent incoming data from [automated feeds in Movebank](https://www.movebank.org/cms/movebank-content/live-data-feeds), we currently recommend using the [Filter by Last X Days App](https://www.moveapps.org/apps/browser/861808be-fb15-4e03-af3d-533642ec797e) as a previous step in the workflow. This can be combined with the [Email Alert App](https://www.moveapps.org/apps/browser/362b42c7-d7a2-4fa6-8d08-b3ddae002f9e) to be notified of new cluster events.

## Input data
move/moveStack in Movebank format and optional threshold speed and window length

## Output data
move/moveStack in Movebank format

## Artefacts
*Cluster_summary_output.csv*: csv with the individual id, cluster centroid location, time spent at the cluster and radius of the cluster

*Cluster_locations_plot.jpeg*: jpeg plot showing a map of the tracking data along with identified cluster locations

## Parameters

*search_radius*: Search radius (meters) from cluster centroid when building clusters.

*window_days*: Window (days) to search for new locations from the most recent location to form one cluster.

*clus_min_locs*: Minimum number of locations required to form a cluster. Default used is 2.

*centroid_calc*: Method for recalculating centroids when actively building clusters; currently "mean" is always used. 
