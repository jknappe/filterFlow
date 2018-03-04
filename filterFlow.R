# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# filterFlow                                                           #
# -----------                                                          #
#                                                                      #
# R script to calculate hydraulich retention time through a filter     #
# author: Jan Knappe                                                   #
# www.janknappe.com / jan.knappe@gmail.com                             #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# LIBRARIES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## @knitr loadLibraries

library(tidyverse)
library(readtext)
library(lubridate)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# CONSTANTS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## @knitr defineConstants

# .kFlowFactor ----
# volume of water (in liters) per count
kFlowFactor = 0.85 

# .kInflowTime ----
# duration of inflow pulse (in seconds)
kInflowTime = 300

# .kInterval ----
# interval bin width (in seconds)
kInterval = 30 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# DATA IMPORT ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## @knitr importData

# .rawData ----
# read textfile created by "Stopwatch & Trainer (v1.1.7)" app for Android
rawData = 
  list.files(pattern = "\\.txt$", recursive = TRUE) %>%
  lapply(., function(x) 
    readtext(x, 
             docvarsfrom = "filenames",
             docvarnames = c("type", "date"),
             dvsep = "-")) %>%
  bind_rows(.) %>%
  mutate(date = ymd(date))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# DATA PROCESSING ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## @knitr cumOutflow
# .cumOutflow ----
# calculate cummulative flow from parsing lap times
cumOutflow = 
  str_match_all(rawData[[2]], "Total Time - (.*?)\n\nLap")[[1]][, 2] %>%
  as_tibble(.) %>%
  mutate(hours = as.numeric(substring(value, 1, 2)),
         mins = as.numeric(substring(value, 4, 5)),
         secs = as.numeric(substring(value, 7, 8)),
         mils = as.numeric(substring(value, 10, 12))) %>%
  transmute(cumOutflowVolume.l = as.numeric(row.names(.)) * kFlowFactor,
            elapsedTime.s = (mils + (1000 * secs) + (60000 * mins) + (3600000 * hours))/1000)

## @knitr intervalOutflow
# .intervalOutflow ----
# calculate outflow per interval bin
intervalOutflow = 
  cumOutflow %>%
  mutate(intervalStartTime.s = kInterval * (round((elapsedTime.s + kInterval/2) / kInterval))) %>%
  group_by(intervalStartTime.s) %>% 
  summarize(cumOutflowVolume.l = max(cumOutflowVolume.l),
            intervalOutflowRate.l_min = n() * kFlowFactor * 60/kInterval) 

## @knitr paddedIntervals
# .paddedIntervals ----
# pad flow data with intervals bins of no-flow conditions
# and calculate storage and net flow
paddedIntervals = 
  max(intervalOutflow$intervalStartTime.s) %>%
  seq(from = 0, to = ., by = kInterval) %>%
  tibble(intervalStartTime.s = .) %>%
  left_join(intervalOutflow, by = "intervalStartTime.s") %>%
  fill(cumOutflowVolume.l) %>%
  transmute(intervalStartTime.min = intervalStartTime.s / 60,
            intervalOutflowRate.l_min = ifelse(is.na(intervalOutflowRate.l_min), 0, intervalOutflowRate.l_min),
            intervalInflowRate.l_min = ifelse(intervalStartTime.s < kInflowTime, sum(intervalOutflowRate.l_min)/((kInflowTime/kInterval)), 0),
            cumOutflowVolume.l = ifelse(is.na(cumOutflowVolume.l), 0, cumOutflowVolume.l),
            cumInflowVolume.l = cumsum(intervalInflowRate.l_min) / (60/kInterval),
            storedWaterVolume.l = cumInflowVolume.l - cumOutflowVolume.l,
            intervalVolumeChange.l_min = intervalInflowRate.l_min - intervalOutflowRate.l_min)

## @knitr residenceTime
# .residenceTime ----
# calculate residence time in water filter
# assumption: water flow between count events is constant
residenceTime = as_tibble(list(intervalStartTime.min = as.vector(rep(NA, times = length(paddedIntervals$intervalStartTime.min))), 
                               outflowEndTime.min = as.vector(rep(NA, times = length(paddedIntervals$intervalStartTime.min)))))
linearFlowTimeRatio = NA
linearEndTime.min = NA

for (i in 1:length(paddedIntervals$intervalStartTime.min)) {
  for (j in 2:length(paddedIntervals$intervalStartTime.min)) {
    # ratio between two time slices
    linearFlowTimeRatio = 
      ifelse (paddedIntervals$cumInflowVolume.l[[i]] <= paddedIntervals$cumOutflowVolume.l[[j]], 
              break, 
              (paddedIntervals$cumInflowVolume.l[[i]] - paddedIntervals$cumOutflowVolume.l[[j]]) / (paddedIntervals$cumOutflowVolume.l[[j+1]] - paddedIntervals$cumOutflowVolume.l[[j]]));
    # linear time point between two slices
    linearEndTime.min = 
      linearFlowTimeRatio * (paddedIntervals$intervalStartTime.min[[j+1]] - paddedIntervals$intervalStartTime.min[[j]]) + paddedIntervals$intervalStartTime.min[[j]];
  }
  residenceTime$intervalStartTime.min[[i]] = paddedIntervals$intervalStartTime.min[[i]]
  residenceTime$outflowEndTime.min[[i]] = linearEndTime.min
}

## @knitr combinedFlows
# .combinedFlows ----
# combine data
combinedFlows = 
  paddedIntervals %>%
  left_join(residenceTime) %>%
  mutate(outflowStartTime.min = lag(outflowEndTime.min, default = min(cumOutflow$elapsedTime.s) / 60),
         # remove observations without change in time intervals
         cumOutflowVolume.l = ifelse(cumOutflowVolume.l == lag(cumOutflowVolume.l) & cumOutflowVolume.l != 0, NA, cumOutflowVolume.l),
         storedWaterVolume.l = ifelse(storedWaterVolume.l == lag(storedWaterVolume.l) & intervalInflowRate.l_min == 0, NA, storedWaterVolume.l),
         # remove obervations that don't make sense after inflow stopped
         outflowStartTime.min = ifelse(intervalInflowRate.l_min == 0, NA, outflowStartTime.min),
         outflowEndTime.min = ifelse(intervalInflowRate.l_min == 0, NA, outflowEndTime.min),
         residenceTime.min = ifelse(intervalInflowRate.l_min == 0, NA, outflowStartTime.min - intervalStartTime.min))

## @knitr filterFlows
# .filterFlows ----
# tidy data
filterFlows = 
  combinedFlows %>%
  select(intervalStartTime.min, 
         intervalInflowRate.l_min, 
         intervalOutflowRate.l_min, 
         intervalVolumeChange.l_min) %>%
  gather(key = flowType,
         value = flowRate.l_min,
         -intervalStartTime.min) %>%
  filter(!is.na(flowRate.l_min)) %>%
  mutate(flowType = case_when(flowType %in% "intervalInflowRate.l_min" ~ "interval inflow rate",
                              flowType %in% "intervalOutflowRate.l_min" ~ "interval outflow rate",
                              flowType %in% "intervalVolumeChange.l_min" ~ "interval stored volume change rate"),
         flowType = factor(flowType),
         date = rawData$date)
  

## @knitr filterVolumes
# .filterVolumes ----
# tidy data 
filterVolumes = 
  combinedFlows %>%
  select(intervalStartTime.min, 
         cumInflowVolume.l, 
         cumOutflowVolume.l, 
         storedWaterVolume.l) %>%
  gather(key = volumeType,
         value = waterVolume.l,
         -intervalStartTime.min) %>%
  filter(!is.na(waterVolume.l)) %>%
  mutate(volumeType = case_when(volumeType %in% "cumInflowVolume.l" ~ "cummulative water inflow",
                                volumeType %in% "cumOutflowVolume.l" ~ "cummulative water outflow",
                                volumeType %in% "storedWaterVolume.l" ~ "stored water"),
         volumeType = factor(volumeType),
         date = rawData$date)  

## @knitr filterTimes
# .filterTimes ----
# tidy data 
filterTimes = 
  combinedFlows %>%
  select(intervalStartTime.min, 
         outflowStartTime.min,
         outflowEndTime.min,
         residenceTime.min) %>%
  gather(key = timeType,
         value = elapsedTime.min,
         -intervalStartTime.min) %>%
  filter(!is.na(elapsedTime.min)) %>%
  mutate(timeType = case_when(timeType %in% "outflowEndTime.min" ~ "outflow end",
                              timeType %in% "outflowStartTime.min" ~ "outflow start",
                              timeType %in% "residenceTime.min" ~ "residence time"),
         timeType = factor(timeType),
         date = rawData$date)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
# PLOTS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## @knitr plot-filterFlows
# .filterFlows ----
ggplot(data = filterFlows, aes(x = intervalStartTime.min, y = flowRate.l_min, color = flowType)) + 
  #geom_line(size = 1.2) + 
  geom_point(size = 2.5, shape = 21, fill = "white") +
  geom_step(size = 1.2) +
 # facet_wrap(~flowType, nrow = 1) +
  theme_light() + 
  scale_color_manual(values=c( "#FF6655", "#77BBFF", "#66CC99", "#CCBB66")) +
  labs(x = "elapsed Time [min]", y = expression(paste("water flow [L", " ", min^{-1}, "]"))) +
  theme(text = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.position = "bottom",
        panel.background = element_rect(fill = "#F4F4F4")) 

## @knitr plot-filterVolumes
# .filterVolumes ----
ggplot(data = filterVolumes, aes(x = intervalStartTime.min, y = waterVolume.l, color = volumeType)) + 
  geom_line(size = 1.2) + 
  geom_point(size = 2.5, shape = 21, fill = "white") +
  #geom_step(size = 1.2) +
  # facet_wrap(~flowType, nrow = 1) +
  theme_light() + 
  scale_color_manual(values=c( "#FF6655", "#77BBFF", "#66CC99", "#CCBB66")) +
  labs(x = "elapsed Time [min]", y = expression(paste("water volume [L]"))) +
  theme(text = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.position = "bottom",
        panel.background = element_rect(fill = "#F4F4F4")) 

## @knitr plot-filterTimes
# .filterTimes ----
ggplot(data = filterTimes, aes(x = intervalStartTime.min, y = elapsedTime.min, color = timeType)) + 
  geom_line(size = 1.2) + 
  geom_point(size = 2.5, shape = 21, fill = "white") +
  #geom_step(size = 1.2) +
  # facet_wrap(~flowType, nrow = 1) +
  theme_light() + 
  scale_color_manual(values=c( "#FF6655", "#77BBFF", "#66CC99", "#CCBB66")) +
  labs(x = "elapsed Time [min]", y = expression(paste("time [min]"))) +
  theme(text = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.position = "bottom",
        panel.background = element_rect(fill = "#F4F4F4")) 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

