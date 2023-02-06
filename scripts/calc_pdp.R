# the Partial Dependency Plots can be slow to run and 'lock up' r in the process. 
# This slim, stand alone script lets us make the pdp plots in the background,
# by launching as a job.

library(tidyverse)
library(doParallel)     # make partial dependency plots less-slow
library(pdp)            # partial dep plots
library(tictoc)

# read in the model object(s)
load('../Massive_forests/cf_no_dins_2023-02-02.Rdata')


# read in already-calculated variable importance rankings
# FIXME update with newly created VIs
(old_labs <- read_csv(paste0(getwd(), '/data/Table_Deviance_Explained_2022-03-08.csv')) %>%
  tidylog::select(Variable = `Variable name`, Label, Type))


(variable_importance <- 
    # read_csv('output_data/variable_importance_ll_2023-02-02.csv') |> 
    read_csv('output_data/variable_importance_2023-02-03.csv') |> 
    group_by(Model) |> 
    mutate(rank = row_number(Model)) |> 
    ungroup() |> 
    left_join(old_labs, by = 'Variable')
)

# extract the variables we wish to make pdps using top n predictors
top_n <- 9      # set n (can revise to 20 or any other value greater than zero and less than 71)
(pdp_cf_no_dins_vars <- 
    variable_importance %>% 
    filter(Model == 'cf_no_dins') %>% 
    slice(1:top_n) %>% 
    pull(Variable)) 

# find pdp for each variable

# 1 build_near_dest
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.

  
  cl<-makeCluster(split);
  registerDoParallel(cl);

  pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_near_dest'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
             ) %>% 
  as_tibble() %>% 
  group_by(build_near_dest) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_near_dest) %>% 
  mutate(iv = 'build_near_dest') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_near_dest_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # ~1 hr 40 mins w 10 cores



# 2 build_area
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_area'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(build_area) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_area) %>% 
  mutate(iv = 'build_area') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_area_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 


# 3 parcel_Build_P
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'parcel_Build_P'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(parcel_Build_P) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = parcel_Build_P) %>% 
  mutate(iv = 'parcel_Build_P') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/parcel_Build_P_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 


# 4 build_Mean_distroad
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_Mean_distroad'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(build_Mean_distroad) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_Mean_distroad) %>% 
  mutate(iv = 'build_Mean_distroad') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_Mean_distroad_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 





# 5 build_Mean_elev_30m
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.


cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_Mean_elev_30m'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(build_Mean_elev_30m) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_Mean_elev_30m) %>% 
  mutate(iv = 'build_Mean_elev_30m') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_Mean_elev_30m_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 








# 6 build_p_otherpaved_300
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_p_otherpaved_300'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(build_p_otherpaved_300) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_p_otherpaved_300) %>% 
  mutate(iv = 'build_p_otherpaved_300') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_p_otherpaved_300_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 





# 7 build_Mean_builddens
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_Mean_builddens'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(build_Mean_builddens) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_Mean_builddens) %>% 
  mutate(iv = 'build_Mean_builddens') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_Mean_builddens_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 





# 8 build_p_grass_300
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_p_grass_300'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(build_p_grass_300) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_p_grass_300) %>% 
  mutate(iv = 'build_p_grass_300') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_p_grass_300_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 








# 9 build_p_tree_100
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_no_dins               # the model
             , pred.var = 'build_p_tree_100'
             , plot = FALSE
             # , plot.engine = "ggplot2"
             # , rug = TRUE
             # , progress = "text"
             , quantiles = TRUE
             , probs = 0:20/20
             , ice = TRUE
             , center = TRUE
             # , alpha = .1
             , parallel = TRUE
) %>% 
  as_tibble() %>% 
  group_by(build_p_tree_100) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_p_tree_100) %>% 
  mutate(iv = 'build_p_tree_100') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/build_p_tree_100_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 