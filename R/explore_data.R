
rm(list=ls())
source('context.R')

# Functions to load csv data

prepare_rainfall_data <-function(csv_file){
  
  data_file <- file.path(dir_data, csv_file)
  dt <- data.table(read.csv(file.path(data_file)))
  setnames(dt, 
           old = c("Date..NZST.","Rainfall..mm.","Data.quality.code"), 
           new = c("datetime","rainfall_mm","dq_code"))
  dt[, datetime := as.POSIXct(datetime,  format='%d/%m/%y %H:%M')]
  return(dt)
}

prepare_stage_data <-function(csv_file){
  
  data_file <- file.path(dir_data, csv_file)
  dt <- data.table(read.csv(file.path(data_file)))
  setnames(dt, 
            old = c("Date..NZST.","Stage..mm.","Data.Quality...."), 
            new = c("datetime","stage_mm","dq_code"))
  dt[, datetime := as.POSIXct(datetime,  format='%d/%m/%Y %H:%M:%S')]
  dt[, stage_mm := parse_number(stage_mm)]
  return(dt)
}

# Data prep and combine

mbs <- prepare_stage_data('rua_bruce_stage.csv')
mbs[ , `:=`(site = "mtbruce")]
mbs[ , `:=`(measure = "stage")]
mbs[ , `:=`(value = stage_mm)]
mbs[ , `:=`(label = paste(site, "stage", sep = "_"))]

#Only retain hourly measurements in line with hourly rainfall measurements
mbs <- mbs[lubridate::minute(datetime)==0,]

rb <- prepare_rainfall_data('rua_basin_rainfall.csv')
rb[ , `:=`(site = "basin")]
rb[ , `:=`(measure = "rainfall")]
rb[ , `:=`(value = rainfall_mm)]
rb[ , `:=`(label = paste(site, "rain", sep = "_"))]

mb <- prepare_rainfall_data('rua_bruce_rainfall.csv')
mb[ , `:=`(site = "mtbruce")]
mb[ , `:=`(measure = "rainfall")]
mb[ , `:=`(value = rainfall_mm)]
mb[ , `:=`(label = paste(site, "rain", sep = "_"))]

keep_cols <- c("datetime", "site", "label", "value")
data <- rbind(rb[,..keep_cols],
              mb[,..keep_cols],
              mbs[,..keep_cols])

dt_min <- c("2023-03-13 0:00")
dt_max <- c("2023-03-22 0:00")

data <- data[datetime >= dt_min & datetime <= dt_max]

p0 <- ggplot(data, aes(x= datetime, y = value, color = label))+
  geom_line()+
  scale_x_datetime(breaks = "1 day",
                   guide = guide_axis(angle = 90))+
  labs(title = 'Stage Height Ruamahanga Mt. Bruce\n with rainfall there and in Ruamahanga headwaters basin\n', 
       x = 'Date',
       y = 'Rainfall (mm)\n') +
  theme_dan1() +
  facet_wrap(~fct_rev(label), ncol = 1, scales = 'free_y')

grid.arrange(p0)

data1 <- dcast(data, datetime + site ~ label, value.var = "value")
head(data1)


head(rb)


