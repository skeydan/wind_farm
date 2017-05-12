library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(DMwR2)

# sensor dataset
s <- read_delim('data.csv',
               col_names = c('id','datetime','v_id', 'value', 'device', 'facility', 'n_entries', 'status',
                             'modified', 'name', 'unit', 'datagroup', 'statuses'),
               #col_types = cols(datetime = col_datetime('%Y-%m-%d %H:%M:%OS')),
               delim = ';',
               skip = 1)
s

s_ordered <- s %>% arrange(datetime)
s_ordered
s_ordered_desc <- s %>% arrange(desc(datetime))
s_ordered_desc

ggplot(s_ordered, aes(x = datetime, y = value, color = name)) + geom_point()

s_filtered <- s_ordered %>% filter(datetime > ymd('2017-02-16') & datetime < ymd('2017-02-18')) %>%
  filter(name =='grid_power_active_avg')
s_filtered

ggplot(s_filtered, aes(x = datetime, y = value)) + geom_point()


# logbook dataset
l <- read_delim('logbook.csv',
                delim = ';')

l_selected <- l %>% select(Id, variable_id, device_name, facility_name, LogId, TimeDetected, TimeReset, TimeAcknowledged,
                           EventCodeNumber, EventCodeText)

l_with_deltas <- l_selected %>% mutate(delta = TimeReset - TimeDetected)
l_with_deltas

l_events <- l_with_deltas %>% group_by(EventCodeText, EventCodeNumber) %>% summarise(cnt = n(), avg_delta = mean(delta)) %>% 
  arrange(desc(avg_delta))
l_events


# thermo sensors

t <- read_delim('thermo.csv',
                 col_names = c('id','datetime','v_id', 'value', 'device', 'facility', 'n_entries', 'status',
                               'modified', 'name', 'unit', 'datagroup', 'statuses'),
                 #col_types = cols(datetime = col_datetime('%Y-%m-%d %H:%M:%OS')),
                 delim = ';',
                 skip = 1)
t

# union of all sensors
st <- s %>% dplyr::union(t)
st <-st %>% select(datetime, value, name)
head(st)

st_pivoted <- st %>% spread(key = name, value = value)
st_pivoted

st_pivoted_wo_date <- st_pivoted %>% select(-datetime)

# number of NAs per column
num_not_na <- apply(st_pivoted_wo_date, 2, function(colvec) sum(is.na(colvec))) %>% sort()
ggplot(data_frame(x = num_not_na), aes(x = x)) + geom_histogram(binwidth = 10)

st_complete <- st_pivoted_wo_date[complete.cases(st_pivoted_wo_date), ]
nrow(st_complete)

impute <- function(x, aggfunc = mean) {
  m <- aggfunc(x, na.rm = TRUE)
  x[is.na(x)] <- m
  return(x)
}

st_imputed_matrix <- apply(st_pivoted_wo_date, 2, impute)
st_imputed <- as.data.frame(st_imputed)

# remove all 0 columns
not_all_0 = apply(st_imputed, 2, sum) > 0
sum(not_all_0)
st_imputed_not0 <- st_imputed[ , not_all_0]


# remove constant columns
not_all_same <- apply(st_imputed_not0, 2, function(x) length(unique(x))) > 1
st_imputed_notallsame <- st_imputed_not0[ , not_all_same]

# PCA
pca <- prcomp(st_imputed_notallsame, center = TRUE, scale. = TRUE)
pca
summary(pca)
plot(pca)



