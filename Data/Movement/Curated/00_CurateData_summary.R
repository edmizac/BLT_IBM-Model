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

# Explore when tamarins rest
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

  
lims <-  c("06:00", "18:00") %>% as_hms()

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



# Summarise important variables
dat.summary <- dat.all %>% 
  group_by(group, id_month) %>%
  dplyr::summarise(
    timesteps = n(),
    ndays = n_distinct(id_day_all),
    mean_timesteps = round(timesteps/ndays)
  )

# Attach resting+idle counts
rest_data_summary <- rest_data %>% 
  dplyr::select(group, id_month, count_rest) %>% 
  distinct()

dat.summary <- dat.summary %>% 
  dplyr::left_join(rest_data_summary, by = c("group", "id_month"))



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

# Save csv
dat.all.siminputrow %>% 
  write.csv(here("Data","Movement",  "Curated", "BLT_groups_data_siminputrow.csv"),
            row.names = FALSE)
  
  
  
  
 