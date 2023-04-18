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

*Cluster_summary_output.csv*: csv with followig columns

- trackId	- Identification of the animal
- clus_ID	- Unique cluster iedntification number
- clus_start	-  Timestamp of the first location of the cluster
- clus_end	- Timestamp of the Last location of the cluster
- clus_status	- "Closed" if the time window (window_days) has expired for the cluster according to users Sys.time() output. These clusters therefore should not change if appending new location data.           "Open" if the time window remains open at the time the function was run. "Open" clusters have the ability to shift sequence, combine with other clusters, emerge as a new cluster, etc. This attribute becomes relevant when appending new satellite data to the location dataframe, and may serve as an index of whether an animal continues to actively visit the cluster site within the time window.
- g_c_location_long	g_c_location_lat - Mean latitude/longitude of the cluster	
- g_med_location_long	g_med_location_lat - Median latitude/longitude of the cluster		
- clus_dur_hr	- Hours from the first to last locations of the cluster
- n_clus_locs	- Number of locations within the cluster
- visits - Number of visits/revisits to the cluster based on the number of times locations fall outside the search radius and return to add locations to the cluster
- max_foray	- Maximum location distance (meters) from centroid during cluster duration for all locations.
- clus_radius	- Maximum location distance (meters) from centroid during cluster duration for cluster-attributed locations.
- avg_clus_dist	- Mean distance from all cluster locations to centroid
- night_pts	- Number of night cluster locations at night
- night_prop -  Proportion of cluster locations at night
*Cluster_locations_plot.jpeg*: jpeg plot showing a map of the tracking data along with identified cluster locations

## Parameters

*search_radius*: Search radius (meters) from cluster centroid when building clusters.

*window_days*: Window (days) to search for new locations from the most recent location to form one cluster.

*clus_min_locs*: Minimum number of locations required to form a cluster. Default used is 2.

*centroid_calc*: Method for recalculating centroids when actively building clusters; currently "mean" is always used. 
