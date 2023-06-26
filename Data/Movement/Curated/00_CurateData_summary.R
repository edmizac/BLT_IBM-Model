# Script name: CurateData_summary.R
# Script purpose: summarize number of days per month and other metrics for simulations (simulation-time metrics)

# Date created: 2022-10-14d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("hms")
library("lubridate")

theme_set(theme_bw(base_size = 16))

# Read data
dat.all <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv")
                    , stringsAsFactors = TRUE
                    )  #%>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
  
# str(dat.all)
# dat.all$id %>% levels()
dat.all$id_day_all %>% levels()
dat.all$id_month %>% levels()

dat.all$behavior %>% unique()

# Explore when tamarins rest and derive resting probabilities based on 2023-06-24d conversation with Ronald:
target <- c("Resting", "Inactive")
rest_data <- dat.all %>% 
  mutate(
    datetime = ymd_hms(datetime)
  ) %>% 
  mutate(
    date = as.Date(datetime),
    time = hms::as_hms(datetime)
  ) %>% 
  # group_by(group, id_month, date) %>% 
  group_by(group, id_month) %>% 
  mutate(
    count = n() # timesteps
  ) %>% 
  dplyr::filter(behavior %in% target) %>% 
  mutate(
    count_rest = n()
  )


lims <-  c("06:00:00", "18:00:00") %>% hms::as_hms()

# Option 1: Resting and idle separated
rest_data %>% 
  ggplot() +
  geom_density(aes(x = time, color = behavior), 
               # adjust = 5,
               linewidth = 0.7) +
  # geom_jitter(aes(x= time, y = 0, height = 0.001, color = behavior), 
  # geom_point(aes(x= time, y = 0, color = behavior), 
  #             size = 0.7, alpha = 0.5) +
  geom_rug(aes(x = time, color = behavior), linewidth = 0.4,
           length = unit(0.05, "npc")
  ) +
  facet_wrap(~group+id_month) +
  ggtitle("Probability of resting or idle") +
  scale_x_time(breaks = scales::breaks_width("2 hours"), limits = lims) +
  theme(
    axis.text.x = element_text(
      # size = 12,
      angle = 90
    )
  ) +
  scale_color_manual(values = c("#2E86C1", "#D35400")) +
  ggpp::geom_text_npc(aes(label=paste("n=", count_rest, "/", count)),
                      npcx = "center", npcy = "top"
  )

# # Save plot
# ggsave(here("Data", "Movement", "Curated", "Param_siminputrow",
#             '00_Resting_probability1.png'), height = 7, width = 10, dpi = 600)

# Option 2: resting = idle + resting
rest_data <- rest_data %>% 
  mutate(
    behavior = case_when(behavior == "Inactive" ~ "Resting",
                       TRUE ~ behavior)
  )

rest_data$behavior %>% unique()

rest_data %>% 
  ggplot() +
  geom_density(aes(x = time, color = behavior), 
               # adjust = 5,
               linewidth = 0.7) +
  # geom_jitter(aes(x= time, y = 0, height = 0.001, color = behavior), 
  # geom_point(aes(x= time, y = 0, color = behavior), 
  #             size = 0.7, alpha = 0.5) +
  geom_rug(aes(x = time, color = behavior), linewidth = 0.4,
           length = unit(0.05, "npc")
  ) +
  facet_wrap(~group+id_month) +
  ggtitle("Probability of resting (resting + idle)") +
  scale_x_time(breaks = scales::breaks_width("2 hours"), limits = lims) +
  theme(
    axis.text.x = element_text(
      # size = 12,
      angle = 90
    )
  ) +
  # scale_color_manual(values = c("black")) + #, "#D35400")) +
  scale_color_manual(values = c("#2E86C1")) + #, "#D35400")) +
  ggpp::geom_text_npc(aes(label=paste("n=", count_rest, "/", count)),
                      npcx = "center", npcy = "top"
  )

# # Save plot
# ggsave(here("Data", "Movement", "Curated", "Param_siminputrow",
#             '00_Resting_probability2.png'), height = 7, width = 10, dpi = 600)


# Explore how many times in sequence tamarins rest in sequence
rest_seq <- dat.all %>% 
  group_by(group, id_month, id_day_all) %>% 
  mutate(
    behavior = case_when(behavior == "Inactive" ~ "Resting",
                         TRUE ~ behavior)
  ) %>% 
  mutate(
    counter = data.table::rleid(behavior),
    resting_sequential = data.table::rowid(counter)
  ) %>% 
  # dplyr::filter(behavior %in% target)
  dplyr::filter(behavior == "Resting") %>% 
  mutate(
    datetime = ymd_hms(datetime)
  ) %>% 
  mutate(
    date = as.Date(datetime),
    time = hms::as_hms(datetime)
  )

# get only max values of sequential resting:
rest_seq <- rest_seq %>% 
  group_by(group, id_month, id_day_all) %>% 
  mutate(
    max_count = max(resting_sequential)
  )

# Relevel all fragment names to size categories
rest_seq <- rest_seq %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(
    fragment = case_when(
      group == "Suzano" ~ "Riparian",
      group == "Guareí" ~ "Small",
      group == "Santa Maria" ~ "Medium",
      group == "Taquara" ~ "Continuous",
      TRUE ~ "check"
    )
  ) %>% 
  mutate(
    fragment = forcats::fct_relevel(fragment, "Riparian", "Small", "Medium", "Continuous")
  )

rest_seq %>% 
  ggplot(
    aes(x = time, y = resting_sequential)
  ) +
  geom_point() +
  # geom_density(aes(x = time, color = behavior), 
               # adjust = 5,
               # linewidth = 0.7) +
  # # geom_jitter(aes(x= time, y = 0, height = 0.001, color = behavior), 
  # # geom_point(aes(x= time, y = 0, color = behavior), 
  # #             size = 0.7, alpha = 0.5) +
  # geom_rug(aes(x = time, color = behavior), linewidth = 0.4,
  #          length = unit(0.05, "npc")
  # ) +
  facet_wrap(~group+id_month) +
  # ggtitle("Probability of resting (resting + idle)") +
  # scale_x_time(breaks = scales::breaks_width("2 hours"), limits = lims) +
  theme(
    axis.text.x = element_text(
      # size = 12,
      angle = 90
    )
  ) #+
  # scale_color_manual(values = c("black")) + #, "#D35400")) +
  # scale_color_manual(values = c("#2E86C1")) + #, "#D35400")) +
  # ggpp::geom_text_npc(aes(label=paste("n=", count_rest, "/", count)),
  #                     npcx = "center", npcy = "top"
  # )

# # Save plot
# ggsave(here("Data", "Movement", "Curated", "Param_siminputrow",
#             '00_Resting_counter1.png'), height = 7, width = 10, dpi = 600)



rest_seq %>% 
  dplyr::select(fragment, id_month, resting_sequential) %>% 
  distinct() %>%
  ggplot(
    aes(x = fragment, y = resting_sequential, group = id_month, color = id_month)
  ) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.7)) +
  # facet_wrap(~group) +
  ggtitle("Sequential steps resting (resting + idle)") +
  # scale_x_time(breaks = scales::breaks_width("2 hours"), limits = lims) +
  theme(
    # axis.text.x = element_text(
      # size = 12,
      # angle = 90
    # )
  ) +
  scale_color_viridis_d()
# scale_color_manual(values = c("black")) + #, "#D35400")) +
# scale_color_manual(values = c("#2E86C1")) + #, "#D35400")) +
# ggpp::geom_text_npc(aes(label=paste("n=", count_rest, "/", count)),
#                     npcx = "center", npcy = "top"
# )

# # Save plot
# ggsave(here("Data", "Movement", "Curated", "Param_siminputrow",
#             '00_Resting_counter2.png'), height = 7, width = 10, dpi = 600)


# How many times tamarins rest per day per timeframe?
rest_seq <- rest_seq %>% 
  group_by(group, id_month) %>% 
  mutate(
    n_days = n_distinct(id_day_all)
  ) %>% 
  group_by(group, id_month, id_day_all) %>% 
  mutate(
    n_resting_instances = n() / n_days
  )

rest_seq %>% 
  dplyr::select(group, id_month, n_resting_instances) %>% 
  distinct() %>%
  ggplot(
    aes(x = group, y = n_resting_instances, group = id_month, color = id_month)
  ) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.7)) +
  # facet_wrap(~group) +
  ggtitle("Instances where tamarins rested (resting + idle) by day") +
  # scale_x_time(breaks = scales::breaks_width("2 hours"), limits = lims) +
  theme(
    # axis.text.x = element_text(
    # size = 12,
    # angle = 90
    # )
  ) +
  scale_color_viridis_d()
# scale_color_manual(values = c("black")) + #, "#D35400")) +
# scale_color_manual(values = c("#2E86C1")) + #, "#D35400")) +
# ggpp::geom_text_npc(aes(label=paste("n=", count_rest, "/", count)),
#                     npcx = "center", npcy = "top"
# )

# # Save plot
# ggsave(here("Data", "Movement", "Curated", "Param_siminputrow",
#             '00_Resting_counter3.png'), height = 7, width = 10, dpi = 600)



# Summarise important variables
dat.summary <- dat.all %>% 
  group_by(group, id_month) %>%
  dplyr::summarise(
    timesteps = n(),
    ndays = n_distinct(id_day_all),
    mean_timesteps = round(timesteps/ndays),
    midday = round(mean_timesteps/2),
    midday_start = round(0.15 * mean_timesteps), # 15% of 
    midday_end = round(0.85 * mean_timesteps)
  )

# Attach resting+idle counts
rest_data_summary <- rest_data %>% 
  dplyr::select(group, id_month, count_rest) %>% 
  distinct()

dat.summary <- dat.summary %>% 
  dplyr::left_join(rest_data_summary, by = c("group", "id_month")) %>% 
  mutate(prob_rest = count_rest/timesteps)

# Attach resting+idle sequential counts
rest_seq_summary <- rest_seq %>% 
  group_by(group, id_month) %>% 
  summarise(duration = max(resting_sequential))

dat.summary <- dat.summary %>% 
  dplyr::left_join(rest_seq_summary, by = c("group", "id_month"))



# Save csv:
# dat.summary %>%
#   write.csv(here("Data","Movement",  "Curated", "BLT_groups_data_summary.csv"),
#             row.names = FALSE)


# Filter timeframes with ndays < 2
dat.summary.siminputrow <- dat.summary %>%
  dplyr::filter(ndays > 2) %>% 
  droplevels()

# Save csv:
# dat.summary.siminputrow %>%
#   write.csv(here("Data","Movement",  "Curated", "BLT_groups_data_summary_siminputrow.csv"),
#                  row.names = FALSE)


# Filter curated data to the months specified on the siminputrow table (from 7096 to 5902 rows (=timesteps))
target <- dat.summary.siminputrow[ , 1:2]

dat.all.siminputrow <- dat.all %>%
  inner_join(target) %>% # FILTER IS WRONG. YOU HAVE TO USE INNER_JOIN() # https://stackoverflow.com/questions/67097301/dplyr-filter-not-filtering-entire-dataset
  # dplyr::filter_if(group %in% dat.summary.siminputrow$group, id_month %in% dat.summary.siminputrow$id_month)
  droplevels()
  
# dat.all$group %in% dat.summary.siminputrow$group

# # Save csv
# dat.all.siminputrow %>% 
#   write.csv(here("Data","Movement",  "Curated", "BLT_groups_data_siminputrow.csv"),
#             row.names = FALSE)
#   
  
  
  
 