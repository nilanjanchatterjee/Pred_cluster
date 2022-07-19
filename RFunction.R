library(geosphere)
library(suncalc)
library(purrr)
library(plyr)
library(move)

#' \describe{
#'   \item{individual.local.identifier}{Animal identification}
#'   \item{clus_ID}{Sequential cluster ID number}
#'   \item{clus_start}{Timestamp of first location in cluster}
#'   \item{clus_end}{Timestamp of last location in cluster}
#'   \item{clus_status}{"Closed" if the time window (window_days) has expired for the cluster according to users Sys.time() output.
#'                      These clusters are therefore solidified and should not change if appending new location data.
#'                      "Open" if the time window remains open at the time the function was run. "Open" clusters have the ability
#'                      to shift sequence, combine with other clusters, emerge as a new cluster, etc. This attribute becomes
#'                      relevant when appending new satellite data to the location dataframe, and may serve as an index of whether
#'                      an animal continues to actively visit the cluster site within the time window.}
#'   \item{g_c_Long}{Geometic centroid longitude value calculated using the mean}
#'   \item{g_c_Lat}{Geometic centroid latitude value calculated using the mean}
#'   \item{g_med_Long}{Geometic centroid longitude value calculated using the median}
#'   \item{g_med_Lat}{Geometic centroid latitude value calculated using the median}
#'   \item{clus_dur_hr}{Hours from the first to last locations of the cluster}
#'   \item{n_clus_locs}{Number of locations within the cluster}
#'   \item{visits}{Number of visits/revisits to the cluster based on the number of times locations fall outside the search radius and return
#'                 to add locations to the cluster}
#'   \item{max_foray}{Maximum location distance (meters) from centroid during cluster duration for all locations}
#'   \item{clus_radius}{Maximum location distance (meters) from centroid during cluster duration for cluster-attributed locations}
#'   \item{avg_clus_dist}{Mean distance from all cluster locations to centroid}
#'   \item{night_pts}{Number of night cluster locations based on 'daylight_hrs' argument}
#   \item{night_prop}{Proportion of night cluster locations}
# }

######################################################################################

rFunction <-function(data, search_radius, window_days, clus_min_locs, centroid_calc="mean",daylight_hrs=NA ){
  
  dat <-as.data.frame(data)
  
  if(("tag_local_identifier" %in% colnames(dat))==FALSE){stop("No 'local_identifier' column found.")}
  
  if(inherits(dat$timestamp, 'POSIXct')==FALSE){stop("'timestamp' must be POSIXct.")}
  #ensure arguments are valid below
  if(is.na(centroid_calc) | is.null(centroid_calc)){stop("'centroid_calc' argument must = 'median' or 'mean'")}
  if((!is.na(centroid_calc) && !is.null(centroid_calc) && (centroid_calc == "mean" | centroid_calc == "median"))==FALSE){stop("'centroid_calc' argument must = 'median' or 'mean'")}
  if(is.na(search_radius) | is.null(search_radius)){stop("invalid 'search_radius'")}
  if((!is.na(search_radius) && !is.null(search_radius) && (search_radius>=0))==FALSE){warning("No clusters identified when 'search_radius_meter' <= 0.")}
  if(clus_min_locs<2){warning("Clusters must have at least 2 locations. 'clus_min_locs' argument < 2 returns default of 2.")}
  
  moveMe <- function(data, tomove, where = "last", ba = NULL) {
    temp <- setdiff(names(data), tomove)
    x <- switch(
      where,
      first = data[c(tomove, temp)],
      last = data[c(temp, tomove)],
      before = {
        if (is.null(ba)) stop("must specify ba column")
        if (length(ba) > 1) stop("ba must be a single character string")
        data[append(temp, values = tomove, after = (match(ba, temp)-1))]
      },
      after = {
        if (is.null(ba)) stop("must specify ba column")
        if (length(ba) > 1) stop("ba must be a single character string")
        data[append(temp, values = tomove, after = (match(ba, temp)))]
      })
    x
  }
  
  dat<-dat[order(dat$tag_local_identifier,dat$timestamp),]     #make sure we have this line active in final function, hashed for testing
  #set up location data output with cluster number attribute
  dat2<-dat[1,]
  dat2$clus_ID<-NA
  dat2<-dat2[-1,]
  uni_individual.local.identifier<-as.character(unique(dat$tag_local_identifier))            #loop through individual.local.identifiers
  #message("TOTAL PROGRESS")
  #pb <- utils::txtProgressBar(min=0, max=length(uni_individual.local.identifier), style=3)   #initiate base progress bar
  for(zz in 1:length(uni_individual.local.identifier)) {                        #start loop
    out_all<-subset(dat, tag_local_identifier == uni_individual.local.identifier[zz])                                    #get rows per individual.local.identifier
    if(length(which(is.na(out_all$location_lat)))==0){warning(paste(uni_individual.local.identifier[zz], "shows no missed locations. Ensure 'failed' fix attempts are included for accurate cluster attributes."))}
    #subset successful fixes for algorithm
    out<-out_all[which(!is.na(out_all$location_lat)),]                                   #subset only usable locations
    if(nrow(out)>0){
      ##############################################
      ###########Construct Primary Cluster Algorithm
      ##############################################
      #add/prep columns
      out$index<-seq(1:nrow(out))
      out$ClusID<-0
      out<-moveMe(out, "ClusID", "first")
      out<-moveMe(out, "index", "first")
      #first internal loop with windows progress bar and label by animal and process
      c<-1                                                                    #seed ticker for sequential cluster numbers
      #pb2 <- tcltk::tkProgressBar(min = 0, max = nrow(out), width = 500)
      for(j in 1:nrow(out)){                                                  #for each sequential location
        if(out$ClusID[j] == 0){                                              #IF THE LOCATION HAS NOT BEEN ASSIGNED A CLUSTER
          cent<-c(out[j,"location_long"], out[j,"location_lat"])                                   #LL reference point location as cluster centroid
          t<- out[which(out$timestamp > out[j,"timestamp"] & out$timestamp <= out[j,"timestamp"] + as.difftime(window_days, units= "days")),] #create time window
          AR_Clus<- unique(t[which(t$ClusID >0),"ClusID"])                      #list of clusters IDs already formed within the time window
          for(m in 1:length(AR_Clus)){  #this small loop updates locations within the time window that already are clustered the centroid of thier clusters
            b<-out[which(out$ClusID == AR_Clus[m]),]                            #pull all the locations from that cluster
            if(nrow(t)>0){
              if(centroid_calc == "median"){
                t[which(t$ClusID == AR_Clus[m]),"location_long"]<-stats::median(b$location_long)             #recalc gc location_long   -- median --
                t[which(t$ClusID == AR_Clus[m]),"location_lat"]<-stats::median(b$location_lat)               #recalc gc location_lat
              } else {
                t[which(t$ClusID == AR_Clus[m]),"location_long"]<-mean(b$location_long)             #recalc gc location_long  -- mean --
                t[which(t$ClusID == AR_Clus[m]),"location_lat"]<-mean(b$location_lat)               #recalc gc location_lat
              }
            }
          }
          if(nrow(t)>0){
            t$centlocation_long<-cent[1]                                                   #LL attribute all point distances from cent
            t$centlocation_lat<-cent[2]
            t$dist<-geosphere::distm(cbind(t$centlocation_long,t$centlocation_lat), cbind(t$location_long,t$location_lat), fun = geosphere::distHaversine)[1,]
          }
          if(length(t[which(t$dis<=search_radius),"index"])>0){                           #if a point meets distance criteria
            if(out$ClusID[min(t[which(t$dist<search_radius),"index"])]==0){                #and if that candidate also has not been assigned
              out$ClusID[j]<-c                                                  #assign the reference point a new clusID
              out$ClusID[min(t[which(t$dist<search_radius),"index"])]<-c                   #also assign the candidate point the same ID to create the new cluster
              c<-c+1                                                            #then hit the ticker
            } else {                                                            #OR, if the candidate is already part of a cluster
              out$ClusID[j]<-out$ClusID[min(t[which(t$dist<search_radius),"index"])]       #just assign the point the candidate that cluster ID
            }
          }
        } else { #IF THE LOCATION HAS ALREADY BEEN ASSIGNED A CLUSTER - compare cluster centroid as reference point
          if(centroid_calc == "median"){
            cent<- c(stats::median(out[which(out$ClusID == out$ClusID[j]),"location_long"]),stats::median(out[which(out$ClusID == out$ClusID[j]),"location_lat"]))#recalc centroid --median--
          } else {
            cent<- c(mean(out[which(out$ClusID == out$ClusID[j]),"location_long"]),mean(out[which(out$ClusID == out$ClusID[j]),"location_lat"]))#recalc centroid --mean--
          }
          t<- out[which(out$timestamp > out[j,"timestamp"] & out$timestamp <= out[j,"timestamp"] + as.difftime(window_days, units= "days")),] #create time window
          AR_Clus<- unique(t[which(t$ClusID >0),"ClusID"])                      #same loop as before to update candidate cluster centers
          for(m in 1:length(AR_Clus)){
            b<-out[which(out$ClusID == AR_Clus[m]),]
            if(nrow(t)>0){
              if(centroid_calc == "median"){
                t[which(t$ClusID == AR_Clus[m]),"location_long"]<-stats::median(b$location_long)             #recalc gc location_long   -- median --
                t[which(t$ClusID == AR_Clus[m]),"location_lat"]<-stats::median(b$location_lat)               #recalc gc location_lat
              } else {
                t[which(t$ClusID == AR_Clus[m]),"location_long"]<-mean(b$location_long)             #recalc gc location_long  -- mean --
                t[which(t$ClusID == AR_Clus[m]),"location_lat"]<-mean(b$location_lat)               #recalc gc location_lat
              }
            }
          }
          if(nrow(t)>0){
            t$centlocation_long<-cent[1]                                                   #LL
            t$centlocation_lat<-cent[2]
            t$dist<-geosphere::distm(cbind(t$centlocation_long,t$centlocation_lat), cbind(t$location_long,t$location_lat), fun = geosphere::distHaversine)[1,]
          }
          if(length(t[which(t$dist<=search_radius),"index"])>0){                           #if a location meets distance criteria
            if(out$ClusID[min(t[which(t$dist<search_radius),"index"])]==0){                #and if that candidate also has not been assigned
              out$ClusID[min(t[which(t$dist<search_radius),"index"])]<- out$ClusID[j]      #assign candidate the reference cluster ID
            } else{ #if the candidate is also part of a cluster - we need to merge the clusters into one
              #the next line looks at all locations from both clusters for the minimum timestamp between them for that cluster ID
              merge_num<-out[which(out$timestamp == min(c(out[which(out$ClusID == out$ClusID[j]),"timestamp"],out[which(out$ClusID == out$ClusID[min(t[which(t$dist<search_radius),"index"])]),"timestamp"]))), "ClusID"]
              #then we attribute both clusters that ID
              out[which(out$ClusID == out$ClusID[j]),"ClusID"]<-merge_num
              out[which(out$ClusID == out$ClusID[min(t[which(t$dist<search_radius),"index"])]),"ClusID"]<-merge_num
              rm(merge_num)
            }
          }
        }
        #tcltk::setTkProgressBar(pb2, j, title=paste("Animal", uni_individual.local.identifier[zz], "...building clusters...", round(j/nrow(out)*100, 0), "% completed"))                                                        #update progress bar
      }
      #close(pb2)
      rm(b,c,m,j,t,AR_Clus,cent)
      
      ###################fin primary cluster algorithm
      see<-rle(out$ClusID)                                          #run the rle function, to get bouts of identical values (cluster IDs)
      bout_end<- cumsum(rle(out$ClusID)$lengths)                    #record the start row
      bout_start<- as.integer(c(1, bout_end +1))                    #and end row of each bout
      bout_start<- bout_start[-length(bout_start)]                  #remove the last start bout
      bout_start<-out$timestamp[bout_start]                         #convert bout starts to timestamps
      bout_end<-out$timestamp[bout_end]                             #convert bout ends to timestamps
      bout_duration<-round(difftime(bout_end, bout_start, units= "hours"),1)  #calculate bout durations
      bouts<-data.frame(bout_start,bout_end, bout_duration, see[["lengths"]], see[["values"]]) #merge into a new bouts dataframe
      names(bouts)[names(bouts) == "see...values..."]<- "ClusID"            #rename column for Cluster ID
      names(bouts)[names(bouts) == "see...lengths..."]<- "consec_locs"      #rename column for consective locations
      rm(bout_start, bout_end, see)                                         #clear variables
      
      #we now have a dataframe that shows temporal bouts and number of locations in sequence
      #but these include non-cluster bouts and jump back and forth among cluster IDs
      bouts2<-subset(bouts, ClusID != 0)                            #so remove non-cluster bouts and put in new bout2 df
      bouts2<-bouts2[order(bouts2$ClusID, bouts2$bout_start),]      #sort by cluster ID and bout start
      #summarize clusters
      clus_summary<-plyr::ddply(bouts2, "ClusID", summarize,
                          clus_start= min(bout_start), clus_end= max(bout_end),
                          clus_dur_hr=round(difftime(max(bout_end), min(bout_start), units="hours"),1),
                          n_clus_locs= sum(consec_locs))
      #eliminate clusters that dont meet the required minimum number of locations
      clus_summary<-clus_summary[which(clus_summary$n_clus_locs >= clus_min_locs),]          #need to add a check below and ensure at least one cluster exists
      if(nrow(clus_summary)==0){
        warning(paste("Zero clusters identified for", uni_individual.local.identifier[zz], "given user-entered parameters."))
        out_all$clus_ID<-NA
      } else {
        #sort and relabel new clusters in sequence
        clus_summary<-clus_summary[order(clus_summary$clus_start),] #sort
        clus_summary$clus_ID3<-seq(1:nrow(clus_summary))
        clus_summary<-moveMe(clus_summary, "clus_ID3" , "first")
        #now reattribute cluster/revisit in bouts2
        bouts2$clus_ID3<-NA
        bouts2$clus_ID3<-clus_summary[match(bouts2$ClusID, clus_summary$ClusID), "clus_ID3"]  #match bouts with summary cluster numbers
        bouts2<-moveMe(bouts2, "clus_ID3" , "first")
        bouts2<-bouts2[order(bouts2$clus_ID3, bouts2$bout_start),] #sort by cluster ID and bout start
        clus_summary$ClusID<-NULL
        bouts2$ClusID<-NULL
        bouts2<-bouts2[which(!is.na(bouts2$clus_ID3)),]  #remove bouts associated with clusters that dont meet minimum point criteria
        #Calculate number of visits/revisits to each cluster and add as a cluster summary attribute
        see<-rle(bouts2$clus_ID3)         #run the fancy rle again to get the number of revisits
        clus_summary$visits<-see$lengths  #add number of visits to the cluster summary
        #now reattribute cluster IDs to the whole dataset including missed locations
        out_all$clus_ID3<-NA
        for(k in 1:nrow(bouts2)){
          out_all[which(out_all$timestamp >= bouts2$bout_start[k] & out_all$timestamp <= bouts2$bout_end[k]), "clus_ID3"]<- bouts2$clus_ID3[k]
        }
        rm(k, bouts, bouts2, bout_duration, see, out)
        #relabel and clean up columns
        names(clus_summary)[names(clus_summary) == "clus_ID3"]<- "clus_ID"
        names(out_all)[names(out_all) == "clus_ID3"]<- "clus_ID"
        clus_summary$tag_local_identifier<-out_all$tag_local_identifier[1]
        clus_summary<-moveMe(clus_summary, "tag_local_identifier", "first")
        
        #############fin with cluster identification###################################
        ###################Build Cluster Attributes for modeling######################
        #add which clusters are currently open during this system run time
        clus_summary$clus_status<-"Closed"
        clus_summary<-moveMe(clus_summary, "clus_status", "after", "clus_end")
        clus_summary[which(clus_summary$clus_end >= Sys.time() - as.difftime(window_days, units= "days")), "clus_status"]<- "Open"
        #calculate geometric center of clusters from cluster points and attribute to clus_summary    -- compare median vs mean location
        xxx<-plyr::ddply(out_all, "clus_ID", summarize,
                   g_c_location_long= mean(location_long, na.rm=TRUE), g_c_location_lat= mean(location_lat, na.rm=TRUE), 
                   g_med_location_long= stats::median(location_long, na.rm=TRUE), g_med_location_lat= stats::median(location_lat, na.rm=TRUE))
        xxx<-xxx[1:nrow(xxx)-1,] #remove the last row that summarizes non-cluster points
        clus_summary<-cbind(clus_summary, xxx[,2:5]) #bind to cluster summary
        clus_summary<-moveMe(clus_summary,c("g_c_location_long", "g_c_location_lat", "g_med_location_long", "g_med_location_lat"), "after", "clus_status")
        rm(xxx)
        
        #add some additional cluster attributes
        clus_summary$max_foray<-NA              # max distance from center cluster during cluster duration
        clus_summary$clus_radius<-NA            # max dist within cluster locs -cluster radius
        clus_summary$avg_clus_dist<-NA          # avg. dist from cluster locs to centroid
        # clus_summary$n_24_per<-NA               # number of 24 hr periods with cluster points over cluster duration
        # clus_summary$bin_24hr<-NA               # cluster points span less than or greater than one 24 hr period
        # clus_summary$season<-NA                 # seasons sequence based on julian breaks
        clus_summary$night_pts<-NA              # number of night cluster locs
        clus_summary$night_prop<-NA             # number night locs (1800-0600)/total cluster locs (night=0, day=1)
        #if(!is.na(season_breaks_jul[1])){clus_summary$season<-0}  #if the season argument exists, set all to reference season == 0 before loop
        #loop to calculate attributes
        #pb3 <- tcltk::tkProgressBar(min = 0, max = nrow(clus_summary), width = 500)
        for(i in 1:nrow(clus_summary)){
          ggg<-out_all[which(out_all$timestamp >= clus_summary$clus_start[i] & out_all$timestamp <= clus_summary$clus_end[i]),] #subset cluster fix attempts
          #clus_summary$fix_succ_clus_dur[i]<-round(nrow(ggg[which(!is.na(ggg$location_lat)),])/nrow(ggg),2)                              #prop fix success during cluster duration
          #clus_summary$adj_clus_locs[i]<- round(clus_summary$n_clus_locs[i] / clus_summary$fix_succ_clus_dur[i],1)              #adjusted cluster locations
          fff<-ggg[which(!is.na(ggg$location_lat)),]                                                                                     #subset cluster locations
          fff<-fff[which(fff$clus_ID != clus_summary$clus_ID[i] | is.na(fff$clus_ID)),]                                         #locations off cluster
          #clus_summary$fid[i]<- clus_summary$n_clus_locs[i] - nrow(fff)                                                         #locs on - locs off = Fidelity
          ggg$centlocation_long<-clus_summary[i, "g_c_location_long"]                                                                             #LL distances from centroid
          ggg$centlocation_lat<-clus_summary[i, "g_c_location_lat"]
          ggg$dist<-geosphere::distm(cbind(ggg$centlocation_long,ggg$centlocation_lat), cbind(ggg$location_long,ggg$location_lat), fun = geosphere::distHaversine)[1,]
          ggg$days<-difftime(ggg$timestamp, ggg$timestamp[1], units="days")                                                     #sets up 24 hr period covariates
          clus_summary$max_foray[i]<-round(max(ggg$dist, na.rm=T))                                                              #max foray distance from cluster
          clus_summary$clus_radius[i]<-round(max(ggg$dist[which(ggg$clus_ID == clus_summary$clus_ID[i])],na.rm=T))              #max dist of cluster locations = cluster radius
          clus_summary$avg_clus_dist[i]<- round(mean(ggg$dist[which(ggg$clus_ID == clus_summary$clus_ID[i])],na.rm=T))          #mean distance of cluster locations from center
          aa<-ggg[which(ggg$clus_ID == clus_summary$clus_ID[i]),]                                                               #subset cluster locs
          
          #day/night locations for each cluster
          aa<-aa[which(!is.na(aa$location_lat)),]
          if(!is.na(daylight_hrs[1])){
            #option to set own breaks for day v night locations
            aa$hour<-NA
            aa$hour<-as.numeric(format(aa$timestamp, format='%H'))
            clus_summary$night_pts[i]<-nrow(aa[which(aa$hour<=daylight_hrs[1] | aa$hour>=daylight_hrs[2]),])
            clus_summary$night_prop[i]<-round(clus_summary$night_pts[i] / clus_summary$n_clus_locs[i],2)
          } else{
            ttt<-aa[,c("timestamp", "location_long", "location_lat")]
            #since the format %Z occasionally returns multiple zones, we will use the attr() call
            #ttt$date<-as.Date(aa$timestamp, tz=(unique(base::format(out_all$timestamp, format="%Z"))))
            ttt$date<-as.Date(aa$timestamp, tz=attr(dat$timestamp,"tzone"))
            names(ttt)[names(ttt) == "location_lat"]<- "lat"
            names(ttt)[names(ttt) == "location_long"]<- "lon"
            #dd<-getSunlightTimes(data=ttt ,tz=(unique(base::format(out_all$timestamp, format="%Z")))) #get times
            dd<-suncalc::getSunlightTimes(data=ttt ,tz=attr(dat$timestamp,"tzone"))
            dd$timestamp<-aa$timestamp
            #determine night locations
            #aa$night<-dd %>% rowwise() %>% mutate(match = ifelse(between(timestamp, sunrise, sunset), 1, 0))# %>% select(match) #night=0, day=1
            aa$night<-ifelse(((dd$timestamp>=dd$sunrise) & (dd$timestamp<=dd$sunset)),1,0)
            clus_summary$night_pts[i]<-length(which(aa$night == 0))
            clus_summary$night_prop[i]<-round(clus_summary$night_pts[i] / clus_summary$n_clus_locs[i],2)
            rm(dd, ttt)
          }
          #tcltk::setTkProgressBar(pb3, i, title=paste("Animal",uni_individual.local.identifier[zz], "...building cluster covariates...", round(i/nrow(clus_summary)*100, 0), "% completed"))
        }
        #close(pb3)
        rm(ggg, fff, aa, i)
        ####END cluster prep####
      }
      
      ######################################################################################
      #### Model output
      if(!exists("t_summ") & (ncol(clus_summary)==17)){
        t_summ<-clus_summary[1,]
        t_summ<-t_summ[-1,]
      }
      if(ncol(clus_summary)==17){
        t_summ<-rbind(t_summ, clus_summary)
      }
    } else {
      warning(paste(uni_individual.local.identifier[zz], "has no 'successful' fixes."))
      out_all$clus_ID<-NA
      if(!exists("dat2")){
        dat2<-out_all[1,]
        dat2<-out_all[-1,]
      }
    }
    dat2<-rbind(dat2,out_all)   #append dat2 with out_all data
    #utils::setTxtProgressBar(pb, zz)                         #update the base progress bar % for each animal
  }
  #close(pb)                                          #when finished close the base progress bar
  dat<-dat2  #write the updated location output back to dat
  names(dat) <- make.names(names(dat2),allow_=FALSE)
  ###Converting the data.frame output into move-stack object
  data_move <- move(x=dat$location.long, y=dat$location.lat,
                    time=as.POSIXct(dat$timestamp,format="%Y-%m-%d %H:%M:%S"),
                    data=dat, proj=CRS("+proj=longlat +ellps=WGS84"),
                    animal=dat$tag.local.identifier)
  data_movestack <- moveStack(data_move,forceTz="UTC")
  rm(dat2)
  clus_summary<-t_summ  #write the cluster summary info back to clus_summary
  rm(zz, uni_individual.local.identifier, out_all, t_summ)
  write.csv(clus_summary, file= paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "Cluster_summary_output.csv"),row.names=FALSE)
  #write.csv(data_move, file= "Cluster_all_output.csv")
  return(data_movestack)
}


