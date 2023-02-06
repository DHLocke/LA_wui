# the Partial Dependency Plots can be slow to run and 'lock up' r in the process. 
# This slim, stand alone script lets us make the pdp plots in the background,
# by launching as a job.

library(tidyverse)
library(doParallel)     # make partial dependency plots less-slow
library(pdp)            # partial dep plots
library(tictoc)

# read in the model object(s)
load('../Massive_forests/cf_dins_binary_2023-02-03.RData')


# read in already-calculated variable importance rankings
# FIXME update with newly created VIs
(old_labs <- read_csv(paste0(getwd(), '/data/Table_Deviance_Explained_2022-03-08.csv')) %>%
  tidylog::select(Variable = `Variable name`, Label, Type))


(variable_importance <- read_csv('output_data/variable_importance_2023-02-03.csv'))


# (variable_importance <- 
#     read_csv('output_data/variable_importance_ll_2023-02-02.csv') |>
#     group_by(Model) |>
#     mutate(rank = row_number(Model)) |>
#     ungroup() |>
#     left_join(old_labs, by = 'Variable')
# )

# extract the variables we wish to make pdps using top n predictors
top_n <- 9      # set n (can revise to 20 or any other value greater than zero and less than 71)
(pdp_cf_no_dins_vars <- 
    variable_importance %>% 
    filter(Model == 'cf_dins') %>% 
    slice(1:top_n) %>% 
    pull(Variable)) 

# find pdp for each variable

# 1 build_WINDOWPANE
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.

  
  cl<-makeCluster(split);
  registerDoParallel(cl);

  pdp::partial(cf_dins_binary               # the model
             , pred.var = 'build_WINDOWPANE'
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
  group_by(build_WINDOWPANE) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_WINDOWPANE) %>% 
  mutate(iv = 'build_WINDOWPANE') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_WINDOWPANE_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 



# 2 build_EAVES
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.


cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
             , pred.var = 'build_EAVES'
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
  group_by(build_EAVES) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_EAVES) %>% 
  mutate(iv = 'build_EAVES') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_EAVES_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 



# 3 build_near_dest
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
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
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_near_dest_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 





# 4 build_p_otherpaved_300
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
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
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_p_otherpaved_300_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 





# 5 build_VENTSCREEN
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
             , pred.var = 'build_VENTSCREEN'
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
  group_by(build_VENTSCREEN) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_VENTSCREEN) %>% 
  mutate(iv = 'build_VENTSCREEN') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_VENTSCREEN_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 








# 6 build_Mean_distroad
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
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
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_Mean_distroad_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 




# 7 build_ROOFCONSTR
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
             , pred.var = 'build_ROOFCONSTR'
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
  group_by(build_ROOFCONSTR) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_ROOFCONSTR) %>% 
  mutate(iv = 'build_ROOFCONSTR') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_ROOFCONSTR_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 





# 8 build_FENCEATTAC
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
             , pred.var = 'build_FENCEATTAC'
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
  group_by(build_FENCEATTAC) %>% 
  summarise(ci = list(mean_cl_normal(yhat))) %>% 
  unnest(ci) %>% 
  rename(x = build_FENCEATTAC) %>% 
  mutate(iv = 'build_FENCEATTAC') %>% 
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_FENCEATTAC_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 




# 9 build_Mean_elev_30m
detectCores(); # detect the number of cores of your cpu
split <- 10 # could do 10, but 8 gives us more headroom for other processes.




cl<-makeCluster(split);
registerDoParallel(cl);

pdp::partial(cf_dins_binary               # the model
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
  write_csv(paste0(getwd(), '/output_data/pdp/dins/build_Mean_elev_30m_', Sys.Date(), '.csv'))

stopCluster(cl); toc(); beepr::beep() # 




