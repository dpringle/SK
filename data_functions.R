

#source('context.R')


function 
rua_basin_rain <- data.table(read.csv(file.path(dir_data, 'rua_basin_rainfall.csv')))
rb_rain <- rua_basin_rain

names(rb_rain)
setnames(rb_rain, 
         old = c("Date..NZST.","Rainfall..mm.","Data.quality.code"), 
         new = c("datetime","rainfall_mm.","dq_code"))

rb_rain


table(rb_rain$dq_code)
glimpse(rb_rain)
dt <- copy(rb_rain[])
p1 <- ggplot(dt)