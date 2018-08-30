#########################################################################################################################
#
# The purpose of this script is to share how heuristic and algorithmic multitouch attribution models can be built in R.
# The original script served as a proof of concept to show that this could be done with my business's data.
# A mock dataset is provided that is meant to mimic the look of my original dataset 
# Author: Keenan Morrison
#
#########################################################################################################################

library(dplyr)
library(tidyr)

##### generate mock dataset
set.seed(116)
data <- data.frame(
  id = sample(1:50, 500, replace=TRUE),
  session_id = paste0('GA', 1:500),
  current_session_channel = sample(c('Affiliate', 'Direct', 'Email', 'Organic', 'SEO', 'SEM', 'Social'), 500, replace=TRUE),
  session_timestamp = as.POSIXct(sample(as.POSIXct('2017-01-01 00:08:00 UTC'):as.POSIXct('2017-5-31 00:06:00 UTC'), 500, replace=TRUE), origin = '1970-01-01 00:00:00'),
  stringsAsFactors = FALSE
                  ) %>%
  arrange(id, session_timestamp) %>%
  group_by(id) %>%
  mutate(first_touch_channel = first(current_session_channel, order_by = session_timestamp)
        ,first_lead_channel = ifelse(id %in% c(sample(1:50, 8)), NA, sample(current_session_channel, 1)) #introducing some NA's to mirror real dataset
        ,first_opportunity_channel = ifelse(id %in% c(sample(1:50, 10)), NA, sample(current_session_channel, 1))
        ,first_purchase_channel = last(current_session_channel, order_by = session_timestamp)
        # The timestamp columns will be used later for specific models
        ,first_lead_timestamp = first(subset(., current_session_channel == first_lead_channel)$session_timestamp, order_by = session_timestamp)
        ,first_opportunity_timestamp = first(subset(., current_session_channel == first_opportunity_channel)$session_timestamp, order_by = session_timestamp)
        ,first_purchase_timestamp = last(session_timestamp, order_by = session_timestamp)
  )

############################################ End Dataset generation ##################################################

###################### Linear Model
linear <- data %>%
  select(id, current_session_channel, session_id) %>%
  group_by(id, current_session_channel) %>%
  summarise(channel_counts = n_distinct(session_id))  %>%
  group_by(id) %>%
  mutate(total_counts = sum(channel_counts)) %>%
  ungroup() %>%
  mutate(score = case_when(!is.na(current_session_channel) ~ channel_counts / total_counts,
                           TRUE ~ 0)) %>%
  rename(channel = current_session_channel) %>%
  spread(channel, score) %>%
  select(-channel_counts, -total_counts) %>%
  group_by(id) %>%
  summarise_all(funs(na.omit(.)[1])) %>%
  gather(channel, raw_score, Affiliate:Social) %>%
  group_by(channel) %>%
  summarise(channel_score = sum(raw_score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_scored = channel_score / sum(channel_score, na.rm = TRUE)
        ,model = 'Linear')

heuristic_models <- linear

###################### Full-Path Model

ftc <- data %>% select(id, first_touch_channel) %>% distinct() %>% 
  spread(first_touch_channel, first_touch_channel) %>%  mutate_all(funs(replace(., !is.na(.), 0.225)))

flc <- data %>% select(id, first_lead_channel) %>% distinct() %>% 
  spread(first_lead_channel, first_lead_channel) %>%  mutate_all(funs(replace(., !is.na(.), 0.225))) %>% select(-'<NA>')

foc <- data %>% select(id, first_opportunity_channel) %>% distinct() %>% 
  spread(first_opportunity_channel, first_opportunity_channel) %>% mutate_all(funs(replace(., !is.na(.), 0.225))) %>% select(-'<NA>')

fpc <- data %>% select(id, first_purchase_channel) %>% distinct() %>% 
  spread(first_purchase_channel, first_purchase_channel) %>%  mutate_all(funs(replace(., !is.na(.), 0.225))) 

full_path_init = rbind(ftc, flc, foc, fpc) %>%
  mutate_all(funs(as.numeric(.))) %>%
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  gather(channel, raw_score, Affiliate:Social) 

full_path <- data %>%
  select(id, current_session_channel, session_id) %>%
  group_by(id, current_session_channel) %>%
  summarise(unc_channel_counts = n_distinct(session_id))  %>%
  group_by(id) %>%
  mutate(unc_total_counts = sum(unc_channel_counts)) %>%
  rename(channel = current_session_channel) %>%
  ungroup() %>%
  right_join(., full_path_init, by = c('id', 'channel')) %>%
  mutate(correction = raw_score / 0.225
        ,channel_counts = ifelse(unc_channel_counts - correction < 0, 0, unc_channel_counts - correction)
        ,total_counts = ifelse(unc_total_counts - correction < 0, 0, unc_total_counts - correction)
        ,score = case_when(!is.na(channel) ~ channel_counts / total_counts, 
                          TRUE ~ 0)) %>%
  spread(channel, score) %>%
  select(-channel_counts, -total_counts, -unc_channel_counts, -unc_total_counts, -correction, -raw_score) %>%
  group_by(id) %>%
  summarise_all(funs(na.omit(.)[1])) %>%
  gather(channel, raw_score, Affiliate:Social) %>%
  group_by(channel) %>%
  summarise(channel_score = sum(raw_score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_scored = channel_score / sum(channel_score, na.rm = TRUE)
         ,model = 'Full Path')

heuristic_models = rbind(heuristic_models, full_path)
rm(flc, foc, fpc, ftc, full_path_init, linear, full_path)

###################### W model

wtc <- data %>% select(id, first_touch_channel) %>% distinct() %>% 
  spread(first_touch_channel, first_touch_channel) %>%  mutate_all(funs(replace(., !is.na(.), 0.30)))

wlc <- data %>% select(id, first_lead_channel) %>% distinct() %>% 
  spread(first_lead_channel, first_lead_channel) %>%  mutate_all(funs(replace(., !is.na(.), 0.30))) %>% select(-'<NA>')

woc <- data %>% select(id, first_opportunity_channel) %>% distinct() %>% 
  spread(first_opportunity_channel, first_opportunity_channel) %>% mutate_all(funs(replace(., !is.na(.), 0.30))) %>% select(-'<NA>')

wmod_init = rbind(wtc, wlc, woc) %>%
  mutate_all(funs(as.numeric(.))) %>%
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  gather(channel, raw_score, Affiliate:Social) 

wmod = data %>%
  filter(session_timestamp <= first_opportunity_timestamp | session_timestamp <= first_lead_timestamp) %>%
  select(id, current_session_channel, session_id) %>%
  group_by(id, current_session_channel) %>%
  summarise(unc_channel_counts = n_distinct(session_id))  %>%
  group_by(id) %>%
  mutate(unc_total_counts = sum(unc_channel_counts)) %>%
  rename(channel = current_session_channel) %>%
  ungroup() %>%
  right_join(., wmod_init, by = c('id', 'channel')) %>%
  mutate(correction = raw_score / 0.3
         ,channel_counts = ifelse(unc_channel_counts - correction < 0, 0, unc_channel_counts - correction)
         ,total_counts = ifelse(unc_total_counts - correction < 0, 0, unc_total_counts - correction)
         ,score = case_when(!is.na(channel) ~ channel_counts / total_counts, 
                            TRUE ~ 0)) %>%
  spread(channel, score) %>%
  select(-channel_counts, -total_counts, -unc_channel_counts, -unc_total_counts, -correction, -raw_score) %>%
  group_by(id) %>%
  summarise_all(funs(na.omit(.)[1])) %>%
  gather(channel, raw_score, Affiliate:Social) %>%
  group_by(channel) %>%
  summarise(channel_score = sum(raw_score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_scored = channel_score / sum(channel_score, na.rm = TRUE)
         ,model = 'Wmod')

heuristic_models <- rbind(heuristic_models, wmod)

rm(wmod_init, wmod, wlc, woc, wtc)


###################### U model

utc <- data %>% select(id, first_touch_channel) %>% distinct() %>% 
  spread(first_touch_channel, first_touch_channel) %>%  mutate_all(funs(replace(., !is.na(.), 0.40)))

ulc <- data %>% select(id, first_lead_channel) %>% distinct() %>% 
  spread(first_lead_channel, first_lead_channel) %>%  mutate_all(funs(replace(., !is.na(.), 0.40))) %>% select(-'<NA>')

umod_init = rbind(utc, ulc) %>%
  mutate_all(funs(as.numeric(.))) %>%
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  gather(channel, raw_score, Affiliate:Social) 

umod = data %>%
  filter(session_timestamp <= first_lead_timestamp) %>%
  select(id, current_session_channel, session_id) %>%
  group_by(id, current_session_channel) %>%
  summarise(unc_channel_counts = n_distinct(session_id))  %>%
  group_by(id) %>%
  mutate(unc_total_counts = sum(unc_channel_counts)) %>%
  rename(channel = current_session_channel) %>%
  ungroup() %>%
  right_join(., umod_init, by = c('id', 'channel')) %>%
  mutate(correction = raw_score / 0.4
         ,channel_counts = ifelse(unc_channel_counts - correction < 0, 0, unc_channel_counts - correction)
         ,total_counts = ifelse(unc_total_counts - correction < 0, 0, unc_total_counts - correction)
         ,score = case_when(!is.na(channel) ~ channel_counts / total_counts, 
                            TRUE ~ 0)) %>%
  spread(channel, score) %>%
  select(-channel_counts, -total_counts, -unc_channel_counts, -unc_total_counts, -correction, -raw_score) %>%
  group_by(id) %>%
  summarise_all(funs(na.omit(.)[1])) %>%
  gather(channel, raw_score, Affiliate:Social) %>%
  group_by(channel) %>%
  summarise(channel_score = sum(raw_score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_scored = channel_score / sum(channel_score, na.rm = TRUE)
         ,model = 'Umod')

heuristic_models <- rbind(heuristic_models, umod)

rm(umod_init, umod, ulc, utc)


###### Working area.  Working on shapley value solution (i.e. google analytics data-driven model) & MCMC

####################################### What needs to happen in order to create our own shapley value solution

# 1.) Specify a start and end date date range
# 2.) Pull all sessions data between the date range. DF should look like (UID, session_id, session_medium, session_order (within timeframe), and any KPI's as dummy variables).  Rows are session level
# 3.) Break each user into session_medium_sequences that are either capped by a conversion (kpi-dependent) or are capped by the end date.  Users with multiple conversions within the time frame will occur on multiple rows.
# 4.) Group all rows by the session_medium_sequences and calculate conversion percentages%
# 5.) 