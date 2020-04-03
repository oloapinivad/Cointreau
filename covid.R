# loading packages
library("rjson")
library("growthmodels")
library("minpack.lm")
library("data.table")
library("ggplot2")
library("gridExtra")
library("nlstools")
library("svglite")

# colors
kol <- c("darkgreen", "red", "orange", "blue", "brown", "black", "green", "pink", "aquamarine3", "darkviolet")

# directories declaration
# COVID <- "/Users/paolo/Desktop/covid"
COVID <- "/home/paolo/covid"

# need local fectch of John Hopkins and Italian Civil Protection data
DIR_ITA <- file.path(COVID, "COVID-ITALY/dati-json")
DIR_WORLD <- file.path(COVID, "COVID-WORLD/csse_covid_19_data/csse_covid_19_time_series")

# for figures
FIGDIR <- "/work/users/paolo/figures/COVID-19"
FIGDIR_ITA <- file.path(FIGDIR, "forecast", "italy")
FIGDIR_WOR <- file.path(FIGDIR, "forecast", "world")
FIGDIR_REG <- file.path(FIGDIR, "forecast", "italian_regions")
dir.create(FIGDIR_ITA, recursive = T, showWarnings = FALSE)
dir.create(FIGDIR_REG, recursive = T, showWarnings = FALSE)
dir.create(FIGDIR_WOR, recursive = T, showWarnings = FALSE)

# for updates
now <- Sys.time()

# forecast mode: run it on today or on the last 5 days
forecast_mode <- "today"
#forecast_mode <- "reforecast"

# select regions and countries
countries <- c("Italy", "Spain", "China", "France", "United Kingdom", "Germany", "Iran", "Netherlands", "US")
regions <- c("Lombardia", "Veneto", "Emilia-Romagna", "Piemonte", "Marche", "Liguria", "Toscana")

# days of prediction since the first data
end <- 210

# graphical parameter
bsize <- 18
xdates <- as.Date(c("2020-01-20", "2020-07-01"))

# graphical function
theme_calendar <- function(base_size = 18) {
  theme_light(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1), legend.position = c(.01, 0.99),
      legend.justification = c("left", "top"), legend.box.just = "right",
      legend.background = element_rect(fill = "transparent"), legend.title = element_blank()
    )
}
# moving average (beta)
ma <- function(x, n = 5) {
  avg <- na.omit(filter(x, rep(1 / n, n), sides = 2))
  d <- floor(n/2)
  out <- c(x[1:d], avg, x[(length(x)-d+1):length(x)])
  return(out)
}

# option for forecast date
if (forecast_mode == "today") {
  forecast_dates <-  as.Date(now) 
} else if (forecast_mode == "reforecast") {
  forecast_dates <- seq(as.Date("2020-03-16"), as.Date(now) - 1, by = 1)
  print(forecast_dates)
}

# load the R workspace 
evo_file <- file.path(FIGDIR,"daily_prediction_evolution.Rsave")
if (file.exists(evo_file)) {
  load(evo_file)
} else {
evo_df <- data.frame(
    date = as.Date(character()), country = character(),
    predict = numeric(), upper = numeric(), lower = numeric()
    )
}

# load the R workspace
world_evo_file <- file.path(FIGDIR,"world_daily_prediction_evolution.Rsave")
if (file.exists(world_evo_file)) {
  load(world_evo_file)
} else {
world_evo_df <- data.frame(
    date = as.Date(character()), country = character(),
    predict = numeric(), upper = numeric(), lower = numeric()
    )
}



#############
# FUNCTIONS #
#############

# set up a dataframe from a list for a simpler plotting
dataframing <- function(input_list, names) {
  df <- rbindlist(input_list, fill = T)

  # factorize names and set boundaries to 0 when validity is 0
  df$name <- factor(df$name, levels = names)
  df$upper[df$validity==0] <- 0
  df$lower[df$validity==0] <- 0
  return(df)
}

# create a couple of useful annotations
somelegend <- function(df) {

  # predict and delta for both the plots
  forecast <- unique(merge(aggregate(cbind(predict,delta,lower,upper) ~ name, df, max),df[,c("name", "validity")]))
  maxforecast <- unique(merge(aggregate(cbind(predict,delta) ~ name, df, function(x) max(diff(x))),df[,c("name", "validity")]))

  # order to follow factor order
  forecast <- forecast[order(forecast$name),]
  maxforecast <- maxforecast[order(maxforecast$name),]

  # set to NA when validity is negative
  maxforecast[maxforecast$validity == 0, c("predict","delta")] <- NA
  forecast[forecast$validity == 0,c("predict","delta","lower", "upper")] <- NA

  # create legends
  legend_forecast <- paste0(forecast[,1], ": ", round(forecast[, 2]), "+-", round(forecast[, 3]))
  legend_diff <- paste0(maxforecast[,1], ": ", round(maxforecast[, 2]), "+-", round(maxforecast[, 3]))

  #return a list
  outlist <- list(forecast = legend_forecast, diff = legend_diff, values = forecast)
  return(outlist)
}

# gompertz fitter
predict.covid <- function(calendar, death, name = "Italy", end_date = as.Date(now), total_length = 150, min_death = 5, verbose = F, moving_avg = T) {

  # Gompertz initialization parameters
  # They are taken from here, but as long as they converge they are fine
  # https://www.researchgate.net/post/Is_there_an_R_code_for_Gompertz_model
  alpha <- 9526
  beta <- 9.1618
  k <- 0.0028


  # subset death up to the day of forecast
  if (end_date != as.Date(Sys.time())) {
    death <- death[1:which(calendar == end_date)]
  }

  # days
  days <- 1:length(death)

  # remove missing values
  days <- days[!is.na(death)]
  death <- death[!is.na(death)]

  # if want to use movinng avg
  if (moving_avg) {
    death <- ma(death,5)
  }

  # create dataframe
  data <- data.frame(days = days[death > min_death], death = death[death > min_death])

  # gompertz fit
  nls.gompertz <- minpack.lm::nlsLM(data$death ~ alpha * exp(-beta * exp(-k * data$days)),
    data = data,
    start = list(alpha = alpha, beta = beta, k = k), control = list(maxiter = 500)
  )

  # upper and lower bound coefficinets
  # upper.coef.gompertz <- coef(nls.gompertz) + 2 * summary(nls.gompertz)$parameter[, 2]
  # lower.coef.gompertz <- coef(nls.gompertz) - 2 * summary(nls.gompertz)$parameter[, 2]
  confidence <- confint2(nls.gompertz)
  upper.coef.gompertz <- confidence[, "97.5 %"]
  lower.coef.gompertz <- confidence[, "2.5 %"]

  # predict gompertz
  predict.gompertz <- growthmodels::gompertz(1:total_length, alpha = coef(nls.gompertz)[["alpha"]], beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]])
  upper.gompertz <- growthmodels::gompertz(1:total_length, alpha = upper.coef.gompertz[["alpha"]], beta = upper.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]])
  lower.gompertz <- growthmodels::gompertz(1:total_length, alpha = lower.coef.gompertz[["alpha"]], beta = lower.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]])
  delta.gompertz <- (upper.gompertz - lower.gompertz) / 2

  # lower boundary cannot go below zero
  lower.gompertz[lower.gompertz < 0] <- 0

  # lagged
  lagged.death <- death[death > 100]

  # reliability of the prediction
  if (max(delta.gompertz) > max(predict.gompertz)/2) {
    validity <- 0 
  } else {
    validity <- 1
  }

  # outlist
  out <- list(
    calendar = calendar, days = 1:length(calendar), death = c(death, rep(NA, length(calendar) - length(death))),
    lagged = c(lagged.death, rep(NA, length(calendar) - length(lagged.death))),
    predict = predict.gompertz, upper = upper.gompertz, lower = lower.gompertz,
    delta = delta.gompertz, name = rep(name, length(calendar)),
    validity = factor(rep(validity, length(calendar)))
  ) # , coef = coef(nls.gompertz))
}

for (i in seq_along(forecast_dates)) {
  forecast_date <- forecast_dates[i]
  print(forecast_date)

# load files
file_ita <- file.path(DIR_ITA, "dpc-covid19-ita-andamento-nazionale.json")
file_region <- file.path(DIR_ITA, "dpc-covid19-ita-regioni.json")
file_world <- file.path(DIR_WORLD, "time_series_covid19_deaths_global.csv")
data_ita <- fromJSON(file = file_ita)
data_regioni <- fromJSON(file = file_region)
data_world <- read.csv(file_world, check.names = F)

# create a big multipanel plot
theplot <- list()

#######################
# WORLD DATA ANALYSIS #
#######################

# remove useless rows
good <- 5:length(data_world[1, ])

# set up dates and calenar
dates <- as.Date(colnames(data_world[data_world[, 2] == "Italy", ])[good], format = "%m/%d/%y")
world_plot_calendar <- seq(dates[1], dates[1] + end - 1, 1)

# loop on countries, checks for regions, list handling
out_country <- list()
for (country in countries) {

  # country options
  if (country == "France" | country == "United Kingdom" | country == "Netherlands") {
    death <- unlist(data_world[data_world[, 1] == "" & data_world[, 2] == country, ])[good]
  } else if (country == "China" | country == "US") {
    death <- unlist(apply(data_world[data_world[, 2] == country, 4 + 1:length(dates)], 2, sum))
  } else {
    death <- unlist(data_world[data_world[, 2] == country, ])[good]
  }

  # call covid prediction
  out_country[[country]] <- predict.covid(world_plot_calendar, death, end_date = forecast_date, total_length = end, name = country, verbose = T)

  # print info
  print(paste(country, ":", rev(death)[1], "->", rev(diff(death))[1]))
}

# create a data frame and factorize
#df <- rbindlist(out_country, fill = T)
#df$name <- factor(df$name, levels = countries)

# create a data frame
df <- dataframing(out_country, names = countries)

# create some legen
mm <- somelegend(df)

# status of deaths, lagged
world_plot <- list()
theplot[[7]] <- world_plot[[1]] <- ggplot(df, aes(x = days, y = lagged, col = name)) +
  geom_point() + geom_line() +
  labs(title = "World COVID-19 Deaths when exceeding 100 deaths", x = "# days since 100 deaths", y = "# of deaths") +
  theme_calendar() +
  scale_color_manual(values = kol) +
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 15000))

theplot[[8]] <- world_plot[[2]] <- ggplot(df, aes(x = days, y = c(0, diff(lagged)), col = name)) +
  geom_point() + geom_line() +
  labs(title = "World COVID-19 Daily Deaths when exceeding 100 deaths", x = "# days  since 100 deaths", y = "# of deaths") +
  theme_calendar() +
  scale_color_manual(values = kol) +
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1500))

ml <- arrangeGrob(grobs = world_plot, ncol = 1, nrow = 2, plot = F)
ggsave(
  filename = file.path(FIGDIR_WOR, paste0("status_world_", forecast_date, ".pdf")), plot = ml,
  width = 10, height = 10
)

# convergences (if error larger than value set to Inf)
#forecast <- aggregate(df[, c("predict", "delta")], by = list(df$name), max)
#maxforecast <- aggregate(df[, c("predict", "delta")], by = list(df$name), function(x) max(diff(x)))
#forecast[which(forecast[, 3] > forecast[, 2]), 2:3] <- NA
#maxforecast[which(maxforecast[, 3] > maxforecast[, 2]), 2:3] <- NA
#legend_text <- paste0(countries, ": ", round(forecast[, 2]), "+-", round(forecast[, 3]))
#legend_text2 <- paste0(countries, ": ", round(maxforecast[, 2]), "+-", round(maxforecast[, 3]))
#df$upper[df$validity==0] <- 0
#df$lower[df$validity==0] <- 0
# ggplot conversion of world plots
world_daily <- list()
theplot[[5]] <- world_daily[[1]] <- ggplot(df) +
  geom_point(mapping = aes(x = calendar, y = death, col = name)) +
  geom_line(mapping = aes(x = calendar, y = predict, col = name, linetype = validity)) + 
  #geom_ribbon(aes(x = calendar, y = predict, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1) +
  #scale_fill_manual(values = kol, guide = "none") +
  scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
  labs(title = paste0("World Estimated COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  scale_color_manual(values = kol, labels = mm$forecast) +
  coord_cartesian(ylim = c(0, 15000))

theplot[[6]] <- world_daily[[2]] <- ggplot(df) +
  geom_point(df, mapping = aes(x = calendar, y = c(0, diff(death)), col = name)) +
  geom_line(df, mapping = aes(x = calendar, y = c(0, diff(predict)), col = name, linetype = validity)) +
  #geom_ribbon(aes(x = calendar, y = c(0,diff(predict)), ymin = c(0, diff(lower)), ymax = c(0,diff(upper)), fill = name), alpha = .2, size = 0.1)  +
  #scale_fill_manual(values = kol, guide = "none") +
  scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
  labs(title = paste0("World Estimated daily COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  scale_color_manual(values = kol, labels = mm$diff) +
  coord_cartesian(ylim = c(0, 1500))

ml <- arrangeGrob(grobs = world_daily, ncol = 1, nrow = 2, plot = F)
ggsave(
  file = file.path(FIGDIR_WOR, paste0("world_COVID_prediction_day_", forecast_date, ".pdf")),
  ml, height = 10, width = 12
)


# create a R workspace for storing the forecast in time
world_evo_df <- unique(rbind(world_evo_df, list(date = as.Date(rep(forecast_date, length(countries))), 
                                                               country = countries, predict = mm$values$predict, 
                                                               lower = mm$values$lower, upper = mm$values$upper)))
save(world_evo_df, file=world_evo_file)



#######################
# ITALY DATA ANALYSIS #
#######################
print("National Analysis...")
death <- unlist(lapply(data_ita, "[[", "deceduti"))
calendar <- as.Date(unlist(lapply(data_ita, "[[", "data")))
plot_calendar <- seq(calendar[1], calendar[1] + end - 1, 1)
#days <- 1:length(calendar)
#data <- data.frame(days = days, death = death)
#fit <- lm(log(data$death) ~ data$days)
#predict.exp <- exp(coef(fit)[1]) * exp(coef(fit)[2] * 1:end)

# italian prediction
out_italy <- predict.covid(plot_calendar, death, end_date = forecast_date, total_length = end, name = "Italy", verbose = T)
df <- as.data.frame(out_italy)

# useful text to plot
saturation_text <- paste("Saturation =", round(max(df$predict)), "+-", round(max(df$delta)))
predict_text <- paste("Tomorrow Total Forecast =", round(df$predict[length(death)+1])) # , "+-", round(df$delta[last_day+1]) )
maxdaily_text <- paste("Max Daily Deaths =", round(max(diff(df$predict))))
daysofmax_text <- paste("Day of Max =", plot_calendar[which.max(c(0,diff(df$predict)))])
delta_text <- paste("Tomorrow Delta Forecast =", round(diff(df$predict)[length(death)])) # , "+-", round(diff(df$delta))[length(death)])
first_day_out <- paste("First day below 100 deaths =", plot_calendar[c(0,diff(df$predict))<100 & plot_calendar>as.Date(now)][1])

# ggplot conversion of world plots
italy_daily <- list()
theplot[[1]] <- italy_daily[[1]] <- ggplot(df) +
  geom_point(mapping = aes(x = calendar, y = death, col = name)) +
  geom_line(mapping = aes(x = calendar, y = predict, col = name)) +
  labs(title = paste0("Italy Estimated COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  geom_ribbon(aes(x = calendar, ymin  = lower, ymax = upper, fill = name), alpha = .2, size = 0.1) +
  theme_light(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = kol) +
  scale_fill_manual(values = kol, guide = "none") +
  annotate("text", x = xdates[1], y = 47000, label = saturation_text, size = 5, hjust = 0) +
  annotate("text", x = xdates[1], y = 43000, label = predict_text, size = 5, hjust = 0) +
  coord_cartesian(ylim = c(0, 50000))

theplot[[2]] <- italy_daily[[2]] <- ggplot(df, aes(x = calendar)) +
  geom_point(df, mapping = aes(y = c(0, diff(death)), col = name)) +
  geom_line(df, mapping = aes(y = c(0, diff(predict)), col = name)) +
  geom_ribbon(aes(ymin = c(0, diff(lower)), ymax = c(0, diff(upper)), fill = name), alpha = .2, size = 0.1) +
  labs(title = paste0("Italy Estimated daily COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  theme_light(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = kol) +
  scale_fill_manual(values = kol, guide = "none") +
  annotate("text", x = xdates[1], y = 1400, label = maxdaily_text, size = 5, hjust = 0) +
  annotate("text", x = xdates[1], y = 1250, label = daysofmax_text, size = 5, hjust = 0) +
  annotate("text", x = xdates[1], y = 1100, label = delta_text, size = 5, hjust = 0) +
  annotate("text", x = xdates[1], y = 950,  label = first_day_out, size = 5, hjust = 0) +
  annotate("text", x = xdates[2], y = 1400, label = paste0("Last Updated: ", now), hjust = 1) +
  coord_cartesian(ylim = c(0, 1500))

ml <- arrangeGrob(grobs = italy_daily, ncol = 1, nrow = 2, plot = F)
ggsave(
  file = file.path(FIGDIR_ITA, paste0("Italy_COVID_prediction_day_", forecast_date, ".pdf")),
  ml, height = 10, width = 12
)

# create a R workspace for storing the forecast in time
if (forecast_mode == "today") {
  evo_df <- subset(evo_df, date != as.Date(now))
}
evo_df <- unique(rbind(evo_df, list(date = as.Date(forecast_date), country = "Italy", 
                                    predict = max(df$predict), lower = max(df$lower), upper = max(df$upper))))
save(evo_df, file=evo_file)


#######################
# REGION DATA ANALYSIS #
#######################
print("Regional Analysis...")

calendar_regions <- as.Date(unique(unlist(lapply(data_regioni, "[[", "data"))))
regions_plot_calendar <- seq(calendar_regions[1], calendar_regions[1] + end - 1, 1)
full_regions <- unlist(lapply(data_regioni, "[[", "denominazione_regione"))
death_regions <- unlist(lapply(data_regioni, "[[", "deceduti"))

# loop on countries, checks for regions, list handling
out_region <- list()
for (region in regions) {
  death <- death_regions[which(full_regions == region)]

  out_region[[region]] <- predict.covid(regions_plot_calendar, death, end_date = forecast_date, total_length = end, name = region, verbose = T)

  # print info
  print(paste(region, ":", rev(death)[1], "->", rev(diff(death))[1]))
}

# create a data frame 
df <- dataframing(out_region, names = regions)
#df <- rbindlist(out_region, fill = T)
#df$name <- factor(df$name, levels = regions)

# convergences (if error larger than value set to Inf)
#forecast <- unique(merge(aggregate(cbind(predict,delta) ~ name, df, max),df[,c("name", "validity")]))
#maxforecast <- unique(merge(aggregate(cbind(predict,delta) ~ name, df, function(x) max(diff(x))),df[,c("name", "validity")]))
#forecast <- aggregate(df[, c("predict", "delta")], by = list(df$name), max)
#maxforecast <- aggregate(df[, c("predict", "delta")], by = list(df$name), function(x) max(diff(x)))
#forecast[which(forecast[, 3] > forecast[, 2]), 2:3] <- NA
#maxforecast[which(maxforecast[, 3] > maxforecast[, 2]), 2:3] <- NA
#maxforecast[maxforecast$validity == 0, c("predict","delta")] <- NA
#forecast[forecast$validity == 0,c("predict","delta")] <- NA
#legend_text <- paste0(regions, ": ", round(forecast[, 2]), "+-", round(forecast[, 3]))
#legend_text2 <- paste0(regions, ": ", round(maxforecast[, 2]), "+-", round(maxforecast[, 3]))
#df$upper[df$validity==0] <- 0
#df$lower[df$validity==0] <- 0

# create legends
mm <- somelegend(df)

# ggplot conversion of world plots
region_daily <- list()
theplot[[3]] <- region_daily[[1]] <- ggplot(df) +
  geom_point(mapping = aes(x = calendar, y = death, col = name)) +
  geom_line(mapping = aes(x = calendar, y = predict, col = name, linetype=validity)) +
  geom_ribbon(aes(x = calendar, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1)  +
  scale_fill_manual(values = kol, guide = "none") +
  scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
  labs(title = paste0("Italian Regional Estimated COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  scale_color_manual(values = kol, labels = mm$forecast) +
  coord_cartesian(ylim = c(0, 15000))

theplot[[4]] <- region_daily[[2]] <- ggplot(df) +
  geom_point(df, mapping = aes(x = calendar, y = c(0, diff(death)), col = name)) +
  geom_line(df, mapping = aes(x = calendar, y = c(0, diff(predict)), linetype = validity, col = name)) +
  geom_ribbon(aes(x = calendar, ymin = c(0, diff(lower)), ymax = c(0,diff(upper)), fill = name), alpha = .2, size = 0.1)  +
  scale_fill_manual(values = kol, guide = "none") +
  scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
  labs(title = paste0("Italian Regions Estimated daily COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  scale_color_manual(values = kol, labels = mm$diff) +
  coord_cartesian(ylim = c(0, 600))

ml <- arrangeGrob(grobs = region_daily, ncol = 1, nrow = 2, plot = F)
ggsave(
  file = file.path(FIGDIR_REG, paste0("italian_regions_COVID_prediction_day_", forecast_date, ".pdf")),
  ml, height = 10, width = 12
)


ml <- arrangeGrob(grobs = theplot, ncol = 2, nrow = 4, plot = F)
ggsave(
  file = file.path(FIGDIR, paste0("covid-status-", forecast_date, ".svg")),
  plot = ml, height = 22, width = 22, system_fonts = list(sans = "Helvetica")
)


}


# forcily need to push to date
evo_df$date <- as.Date.numeric(evo_df$date, origin = "1970-01-01")
forecast_evo <- NULL
forecast_evo[[1]] <- ggplot(evo_df) +
  geom_point(mapping = aes(x = date, y = predict, col = country)) +
  geom_line(mapping = aes(x = date, y = predict, col = country)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = country), alpha = .2, size = 0.1)  +
  scale_fill_manual(values = kol, guide = "none") +
  labs(title = paste0("Time Evolution of the Forecasts"), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b", limits = as.Date(c("2020-03-15", "2020-04-15"))) +
  scale_color_manual(values = kol) +
  coord_cartesian(ylim = c(0, 75000))

ml <- arrangeGrob(grobs = forecast_evo, ncol = 1, nrow = 1, plot = F)
ggsave(
  file = file.path(FIGDIR, paste0("forecast_predict_evolution.svg")),
  ml, height = 5, width = 12
)

# forcily need to push to date
world_evo_df$date <- as.Date.numeric(world_evo_df$date, origin = "1970-01-01")
world_evo_df$country <- factor(world_evo_df$country, levels = countries)
forecast_evo <- NULL
forecast_evo[[1]] <- ggplot(world_evo_df) +
  geom_point(mapping = aes(x = date, y = predict, col = country)) +
  geom_line(mapping = aes(x = date, y = predict, col = country)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = country), alpha = .2, size = 0.1)  +
  scale_fill_manual(values = kol, guide = "none") +
  labs(title = paste0("Time Evolution of the Forecasts"), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b", limits = as.Date(c("2020-03-15", "2020-04-15"))) +
  scale_color_manual(values = kol) +
  coord_cartesian(ylim = c(0, 75000))

ml <- arrangeGrob(grobs = forecast_evo, ncol = 1, nrow = 1, plot = F)
ggsave(
  file = file.path(FIGDIR, paste0("forecast_predict_world_evolution.svg")),
  ml, height = 5, width = 12
)





#library("plotly")
#library("htmlwidgets")
#f <- subplot(ggplotly(theplot[[1]]), ggplotly(theplot[[2]]), ggplotly(theplot[[3]]), ggplotly(theplot[[4]]), 
#            #ggplotly(theplot[[5]]), ggplotly(theplot[[6]]), ggplotly(theplot[[7]]), ggplotly(theplot[[8]]),
#             nrows = 2, shareX = T, which_layout = 1)
#htmlwidgets::saveWidget(as_widget(f), file.path(FIGDIR, "plotly.html"), selfcontained=F)

italian_table <-  cbind(Calendar = out_italy$calendar, Italy = out_italy$predict, 
                        as.data.frame(lapply(out_region, "[[", "predict")))
write.csv(italian_table, file.path(COVID,"Cointreau/forecasts",paste0("italian-forecast-",forecast_date,".csv")))

