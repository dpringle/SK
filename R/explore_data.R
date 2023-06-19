

rm(list = ls())
source('context.R')

# Functions to load csv data

prepare_rainfall_data <- function(csv_file) {
  data_file <- file.path(dir_data, csv_file)
  dt <- data.table(read.csv(file.path(data_file)))
  setnames(
    dt,
    old = c("Date..NZST.", "Rainfall..mm.", "Data.quality.code"),
    new = c("datetime", "rainfall_mm", "dq_code")
  )
  dt[, datetime := as.POSIXct(datetime,  format = '%d/%m/%y %H:%M')]
  return(dt)
}

prepare_stage_data <- function(csv_file) {
  data_file <- file.path(dir_data, csv_file)
  dt <- data.table(read.csv(file.path(data_file)))
  setnames(
    dt,
    old = c("Date..NZST.", "Stage..mm.", "Data.Quality...."),
    new = c("datetime", "stage_mm", "dq_code")
  )
  dt[, datetime := as.POSIXct(datetime,  format = '%d/%m/%Y %H:%M:%S')]
  dt[, stage_mm := parse_number(stage_mm)]
  return(dt)
}

# Data prep and combine

mbs <- prepare_stage_data('rua_bruce_stage.csv')
mbs[, `:=`(site = "mtbruce")]
mbs[, `:=`(measure = "stage")]
mbs[, `:=`(value = stage_mm)]
mbs[, `:=`(label = paste(site, "stage", sep = "_"))]

#Only retain hourly measurements in line with hourly rainfall measurements
mbs <- mbs[lubridate::minute(datetime) == 0, ]

rb <- prepare_rainfall_data('rua_basin_rainfall.csv')
rb[, `:=`(site = "basin")]
rb[, `:=`(measure = "rainfall")]
rb[, `:=`(value = rainfall_mm)]
rb[, `:=`(label = paste(site, "rain", sep = "_"))]

mb <- prepare_rainfall_data('rua_bruce_rainfall.csv')
mb[, `:=`(site = "mtbruce")]
mb[, `:=`(measure = "rainfall")]
mb[, `:=`(value = rainfall_mm)]
mb[, `:=`(label = paste(site, "rain", sep = "_"))]

keep_cols <- c("datetime", "site", "label", "value")
data <- rbind(rb[, ..keep_cols],
              mb[, ..keep_cols],
              mbs[, ..keep_cols])

dt_min <- c("2023-03-01 0:00")
dt_max <- c("2023-03-31 0:00")

data <- data[datetime >= dt_min & datetime <= dt_max]

p0 <- ggplot(data, aes(x = datetime, y = value, color = label)) +
  geom_line() +
  scale_x_datetime(breaks = "1 day",
                   guide = guide_axis(angle = 90)) +
  labs(title = 'Stage Height Ruamahanga Mt. Bruce\n with rainfall there and in Ruamahanga headwaters basin\n',
       x = 'Date',
       y = 'Rainfall (mm)\n') +
  theme_dan1() +
  facet_wrap( ~ fct_rev(label), ncol = 1, scales = 'free_y')

grid.arrange(p0)

# Look at correlation / lag between basin rain and bridge stage
dt_min <- c("2023-01-01 0:00")
dt_max <- c("2023-06-31 0:00")

mbs1 <- mbs[datetime >= dt_min & datetime <= dt_max,
            c("datetime", "stage_mm")]
setnames(mbs1,
         old = c("stage_mm"),
         new = c("mb_stage"))

mbr1 <- mb[datetime >= dt_min & datetime <= dt_max,
           c("datetime", "rainfall_mm")]
setnames(mbr1,
         old = c("rainfall_mm"),
         new = c("mb_rain"))

rbr1 <- rb[datetime >= dt_min & datetime <= dt_max,
           c("datetime", "rainfall_mm")]
setnames(rbr1,
         old = c("rainfall_mm"),
         new = c("rb_rain"))

dt <- merge(x = mbs1,
            y = rbr1,
            by = c("datetime"))
dt <- merge(x = dt,
            y = mbr1,
            by = c("datetime"))
dt[, `:=`(
  rb_rain_lag1 = lag(rb_rain, 1),
  rb_rain_lag2 = lag(rb_rain, 2),
  rb_rain_lag3 = lag(rb_rain, 3),
  rb_rain_lag4 = lag(rb_rain, 4),
  rb_rain_lag5 = lag(rb_rain, 5),
  rb_rain_lag6 = lag(rb_rain, 6)
)]

dt[, `:=`(mb_rain_lag1 = lag(mb_rain, 1),
          mb_rain_lag6 = lag(mb_rain, 6))]

dt[, `:=`(mb_stage_lag1 = lag(mb_stage, 1),
          mb_stage_lag2 = lag(mb_stage, 2),
          mb_stage_lag3 = lag(mb_stage, 3),
          mb_stage_lag6 = lag(mb_stage, 6))]

#view(dt)

basin_rain = dt$rb_rain
bridge_rain = dt$mb_rain
bridge_stage = dt$mb_stage

ccf(basin_rain, bridge_stage)
print(ccf(basin_rain, bridge_stage))

ccf(bridge_rain, bridge_stage)
print(ccf(bridge_rain, bridge_stage))

acf(bridge_stage)
print(acf(bridge_stage))

m2 <- lm(data = dt, mb_stage ~ 
           rb_rain_lag4 +
           rb_rain_lag6)

summary(m2)

n_fit <- length(m2$fitted.values)

comp <- tail(dt,n_fit)
comp$fitted <- m2$fitted.values


head(comp)

pp <- ggplot(data = comp, aes(x=datetime,))+
  geom_line(aes(x=datetime, y = mb_stage), color = "darkred")+
  geom_line(aes(x=datetime, y = fitted), color="steelblue", linetype="twodash")+
  theme_dan1()

grid.arrange(pp)



# applying fitted values to my data frame
dt1 <- dt[]
dt$fitted <- m1$fitted.values

view(dt)

# creating ggplot object for visualization
lmodel_plot <- ggplot(dfmodel, aes(x= date, y= var1)) +
  geom_line(aes(y= fitted))

