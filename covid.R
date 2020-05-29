# loading packages
library("rjson")
library("growthmodels")
library("minpack.lm")
library("data.table")
library("ggplot2")
library("gridExtra")
library("nlstools")
library("svglite")
library("scales")
library("wpp2019")

# load wordl population data
data(pop)

# directories declaration
# COVID <- "/Users/paolo/Desktop/covid"
COVID <- "/home/paolo/covid"

# need local fectch of John Hopkins and Italian Civil Protection data
DIR_ITA <- file.path(COVID, "COVID-ITALY/dati-json")
DIR_WORLD <- file.path(COVID, "COVID-WORLD/csse_covid_19_data/csse_covid_19_time_series")
DIR_ENGLAND <- file.path(COVID, "COVID-UK")

# for figures
FIGDIR <- "/work/users/paolo/figures/COVID-19"
FIGDIR_ITA <- file.path(FIGDIR, "forecast", "italy")
FIGDIR_WOR <- file.path(FIGDIR, "forecast", "world")
FIGDIR_REG <- file.path(FIGDIR, "forecast", "italian_regions")
FIGDIR_ENG <- file.path(FIGDIR, "forecast", "england")
for (DIR in c(FIGDIR_ITA, FIGDIR_REG, FIGDIR_WOR, FIGDIR_ENG)) {
  dir.create(DIR, recursive = T, showWarnings = FALSE)
}

# time evolution files
italy_evo_file <- file.path(FIGDIR, "daily_prediction_evolution.Rsave")
world_evo_file <- file.path(FIGDIR, "world_daily_prediction_evolution.Rsave")

# for updates
now <- Sys.time()

# forecast mode: run it on today or on the last 5 days
forecast_mode <- "today"
#forecast_mode <- "reforecast"

# select regions and countries
countries <- c("Italy", "Spain", "China", "France", "United Kingdom", 
               "Germany", "Brazil", "Netherlands", "US", "Belgium", "Sweden")
regions <- c("Lombardia", "Veneto", "Emilia-Romagna", "Piemonte", "Marche", "Liguria", "Toscana")
england_regions <- c("England", "East Of England", "London", "Midlands", "North East And Yorkshire",
  "North West", "South East", "South West")

# days of prediction since the first data
end <- 210

# graphical parameter
bsize <- 18
xdates <- as.Date(c("2020-01-20", "2020-07-01"))
lim_world_cdf <- c(0, 35000)
lim_world_pdf <- c(0, 1200)
lim_rel_world_cdf <- c(0, 600)
lim_rel_world_pdf <- c(0, 30)
lim_italy_cdf <- c(0, 40000)
lim_italy_pdf <- c(0, 1000)
lim_regio_cdf <- c(0, 20000)
lim_regio_pdf <- c(0, 500)
lim_eng_cdf <- c(0, 25000)
lim_eng_pdf <- c(0, 750)

# colors
kol <- c("darkgreen", "red", "orange", "blue", "brown", 
         "black", "lightgreen", "darkviolet", "aquamarine3", "pink", 
         "dodgerblue"
         )

# for text plotting
delta_italy_cdf <- diff(lim_italy_cdf) / 16
delta_italy_pdf <- diff(lim_italy_pdf) / 16

# option for forecast date
if (forecast_mode == "today") {

  # set forecast for today and load evolution files
  forecast_dates <- as.Date(now)
  load(italy_evo_file)
  load(world_evo_file)
} else if (forecast_mode == "reforecast") {

  # define loop on forecast dates, remove old evo file and declare evo arrays
  first_forecast_date <- as.Date("2020-03-01")
  forecast_dates <- seq(first_forecast_date, as.Date(now) - 1, by = 1)
  file.remove(italy_evo_file, world_evo_file)
  world_evo_df <- italy_evo_df <- data.frame(
    date = as.Date(character()), country = character(),
    predict = numeric(), upper = numeric(), lower = numeric()
  )

}

# load the R workspace
#if (file.exists(italy_evo_file)) {
  #load(italy_evo_file)
#} else {
  #italy_evo_df <- data.frame(
    #date = as.Date(character()), country = character(),
    #predict = numeric(), upper = numeric(), lower = numeric()
  #)
#}

# load the R workspace
#if (file.exists(world_evo_file)) {
  #load(world_evo_file)
#} else {
  #world_evo_df <- data.frame(
    #date = as.Date(character()), country = character(),
    #predict = numeric(), upper = numeric(), lower = numeric()
  #)
#}

#############
# FUNCTIONS #
#############

# graphical function
theme_calendar <- function(base_size = 18) {
  theme_light(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1), legend.position = c(.01, 0.99),
      legend.justification = c("left", "top"), legend.box.just = "right",
      legend.background = element_rect(fill = "transparent"), legend.title = element_blank()
    )
}

# moving average
ma <- function(x, n = 5) {
  avg <- na.omit(filter(x, rep(1 / n, n), sides = 2))
  d <- floor(n / 2)
  out <- c(x[1:d], avg, x[(length(x) - d + 1):length(x)])
  return(out)
}

# set up a dataframe from a list for a simpler plotting
dataframing <- function(input_list, names) {
  df <- rbindlist(input_list, fill = T)

  # factorize names and set boundaries to 0 when validity is 0
  df$name <- factor(df$name, levels = names)
  df$upper[df$validity == 0] <- 0
  df$lower[df$validity == 0] <- 0
  return(df)
}

# use wpop2019 to define country population
world_population <- function(country) {
  if (country == "US") { 
    country <- "United States of America"
  }
  population <- subset(pop, name == country)["2020"] / 1000
  return(as.numeric(population))
}

# create a couple of useful annotations
somelegend <- function(df) {

  # predict and delta for both the plots
  forecast <- unique(merge(aggregate(cbind(predict, delta, lower, upper) ~ name, df, max), df[, c("name", "validity", "population")]))
  maxforecast <- unique(merge(aggregate(cbind(predict, delta) ~ name, df, function(x) max(diff(x))), df[, c("name", "validity", "population")]))

  # order to follow factor order
  forecast <- forecast[order(forecast$name), ]
  maxforecast <- maxforecast[order(maxforecast$name), ]

  # set to NA when validity is negative
  maxforecast[maxforecast$validity == 0, c("predict", "delta")] <- NA
  forecast[forecast$validity == 0, c("predict", "delta", "lower", "upper")] <- NA

  # create legends
  legend_forecast <- paste0(forecast[, 1], ": ", round(as.numeric(forecast[, 2])), "+-", round(as.numeric(forecast[, 3])))
  legend_diff <- paste0(maxforecast[, 1], ": ", round(maxforecast[, 2]), "+-", round(maxforecast[, 3]))
  legend_ratio <- paste0(forecast[, 1], ": ", round(as.numeric(forecast[, 2])/forecast[, 7]), "+-", round(as.numeric(forecast[, 3])/forecast[, 7]))

  # return a list
  outlist <- list(forecast = legend_forecast, diff = legend_diff, values = forecast, ratio = legend_ratio)
  return(outlist)
}

# gompertz fitter
predict.covid <- function(calendar, death, name = "Italy", end_date = as.Date(now), total_length = 150, min_death = 5, verbose = F, moving_avg = T) {

  # Gompertz initialization parameters
  # They are taken from here, but as long as they converge they are fine
  # https://www.researchgate.net/post/Is_there_an_R_code_for_Gompertz_model
  # alpha <- 9526
  alpha <- 15000
  beta <- 9.1618
  # k <- 0.0028
  k <- 0.002

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
    death <- ma(death, 5)
  }

  # create dataframe
  data <- data.frame(days = days[death >= min_death], death = death[death >= min_death])
  if (verbose) {
    print(data)
  }

  # if there is not enough data, quite
  if (length(data$death) < 5) {
    print("No valid data, quitting")
    return(NULL)
  }


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

  # test with permutation
  # space <- 5
  # seq_alpha <- seq(lower.coef.gompertz[["alpha"]], upper.coef.gompertz[["alpha"]], length = space)
  # seq_beta <- seq(lower.coef.gompertz[["beta"]], upper.coef.gompertz[["beta"]], length = space)
  # permutation <- expand.grid(seq_alpha, seq_beta)
  # permutation.gompertz <- array(NA, dim=c(space*space, total_length))
  # for (k in 1:(space*space)) {
  #  permutation.gompertz[k,] <- growthmodels::gompertz(1:total_length,
  #                                                     alpha = permutation[k,1],
  #                                                     beta = permutation[k,2],
  #                                                     k = coef(nls.gompertz)[["k"]])
  # }
  # upper.gompertz <- apply(permutation.gompertz, 2, max)
  # lower.gompertz <- apply(permutation.gompertz, 2, min)


  # predict gompertz
  # print(coef(nls.gompertz))
  predict.gompertz <- growthmodels::gompertz(1:total_length,
    alpha = coef(nls.gompertz)[["alpha"]],
    beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]]
  )
  upper.gompertz <- growthmodels::gompertz(1:total_length,
    alpha = upper.coef.gompertz[["alpha"]],
    beta = upper.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]]
  )
  lower.gompertz <- growthmodels::gompertz(1:total_length,
    alpha = lower.coef.gompertz[["alpha"]],
    beta = lower.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]]
  )
  delta.gompertz <- abs(upper.gompertz - lower.gompertz) / 2

  # lower boundary cannot go below zero
  lower.gompertz[lower.gompertz < 0] <- 0

  # lagged
  lagged.death <- death[death > 100]

  # reliability of the prediction
  if (max(delta.gompertz) > max(predict.gompertz) / 2) {
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
    validity = factor(rep(validity, length(calendar))),
    population = rep(world_population(name), length(calendar))
  ) # , coef = coef(nls.gompertz))
}


# loop on the forecast dates
for (i in seq_along(forecast_dates)) {

  # which day perform
  forecast_date <- forecast_dates[i]
  print(forecast_date)

  # load files
  file_ita <- file.path(DIR_ITA, "dpc-covid19-ita-andamento-nazionale.json")
  file_region <- file.path(DIR_ITA, "dpc-covid19-ita-regioni.json")
  file_world <- file.path(DIR_WORLD, "time_series_covid19_deaths_global.csv")
  file_england <- file.path(DIR_ENGLAND, "covid-19-totals-england-regions.csv")
  data_ita <- fromJSON(file = file_ita)
  data_regioni <- fromJSON(file = file_region)
  data_world <- read.csv(file_world, check.names = F)
  data_england <- read.csv(file_england, check.names = F)

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

    print(country)
    # call covid prediction
    out_country[[country]] <- predict.covid(world_plot_calendar, death, end_date = forecast_date, total_length = end, name = country)

    # print info
    # print(paste(country, ":", rev(death)[1], "->", rev(diff(death))[1]))
  }

  # create a data frame
  df <- dataframing(out_country, names = countries)

  # create some legen
  mm <- somelegend(df)

  # status of deaths, lagged
  world_plot <- list()
  theplot[[9]] <- world_plot[[1]] <- ggplot(df, aes(x = days, y = lagged, col = name)) +
    geom_point() +
    geom_line() +
    labs(title = "World COVID-19 Deaths when exceeding 100 deaths", x = "# days since 100 deaths", y = "# of deaths") +
    theme_calendar() +
    scale_color_manual(values = kol) +
    coord_cartesian(xlim = c(0, 100), ylim = lim_world_cdf)

  theplot[[10]] <- world_plot[[2]] <- ggplot(df, aes(x = days, y = c(0, diff(lagged)), col = name)) +
    geom_point() +
    geom_line() +
    labs(title = "World COVID-19 Daily Deaths when exceeding 100 deaths", x = "# days  since 100 deaths", y = "# of deaths") +
    theme_calendar() +
    scale_color_manual(values = kol) +
    coord_cartesian(xlim = c(0, 100), ylim = lim_world_pdf)

  ml <- arrangeGrob(grobs = world_plot, ncol = 1, nrow = 2, plot = F)
  ggsave(
    filename = file.path(FIGDIR_WOR, paste0("status_world_", forecast_date, ".pdf")), plot = ml,
    width = 10, height = 10
  )

  # ggplot conversion of world plots
  world_daily <- list()
  theplot[[5]] <- world_daily[[1]] <- ggplot(df) +
    geom_point(mapping = aes(x = calendar, y = death, col = name)) +
    geom_line(mapping = aes(x = calendar, y = predict, col = name, linetype = validity)) +
    # geom_ribbon(aes(x = calendar, y = predict, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1) +
    # scale_fill_manual(values = kol, guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
    labs(title = paste0("World Estimated COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
    theme_calendar(base_size = bsize) +
    scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
    scale_color_manual(values = kol, labels = mm$forecast) +
    coord_cartesian(ylim = lim_world_cdf)

  theplot[[6]] <- world_daily[[2]] <- ggplot(df) +
    geom_point(df, mapping = aes(x = calendar, y = c(0, diff(death)), col = name)) +
    geom_line(df, mapping = aes(x = calendar, y = c(0, diff(predict)), col = name, linetype = validity)) +
    # geom_ribbon(aes(x = calendar, y = c(0,diff(predict)), ymin = c(0, diff(lower)), ymax = c(0,diff(upper)), fill = name), alpha = .2, size = 0.1)  +
    # scale_fill_manual(values = kol, guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
    labs(title = paste0("World Estimated daily COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
    theme_calendar(base_size = bsize) +
    scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
    scale_color_manual(values = kol, labels = mm$diff) +
    coord_cartesian(ylim = lim_world_pdf)

  theplot[[7]] <- ggplot(df) +
    geom_point(mapping = aes(x = calendar, y = death / population, col = name)) +
    geom_line(mapping = aes(x = calendar, y = predict / population, col = name, linetype = validity)) +
    # geom_ribbon(aes(x = calendar, y = predict, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1) +
    # scale_fill_manual(values = kol, guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
    labs(title = paste0("World Estimated COVID-19 deaths per million people at ", forecast_date), x = "", y = "# of deaths") +
    theme_calendar(base_size = bsize) +
    scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
    scale_color_manual(values = kol, labels = mm$ratio) +
    coord_cartesian(ylim = lim_rel_world_cdf)

  theplot[[8]] <- ggplot(df) +
    geom_point(df, mapping = aes(x = calendar, y = c(0, diff(death / population)), col = name)) +
    geom_line(df, mapping = aes(x = calendar, y = c(0, diff(predict / population)), col = name, linetype = validity)) +
    # geom_ribbon(aes(x = calendar, y = c(0,diff(predict)), ymin = c(0, diff(lower)), ymax = c(0,diff(upper)), fill = name), alpha = .2, size = 0.1)  +
    # scale_fill_manual(values = kol, guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
    labs(title = paste0("World Estimated daily COVID-19 deaths per million people at ", forecast_date), x = "", y = "# of deaths") +
    theme_calendar(base_size = bsize) +
    scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
    scale_color_manual(values = kol, labels = mm$values$name) +
    coord_cartesian(ylim = lim_rel_world_pdf)

  ml <- arrangeGrob(grobs = world_daily, ncol = 1, nrow = 2, plot = F)
  ggsave(
    file = file.path(FIGDIR_WOR, paste0("world_COVID_prediction_day_", forecast_date, ".pdf")),
    ml, height = 10, width = 12
  )


  # create a R workspace for storing the forecast in time
  world_evo_df <- unique(rbind(world_evo_df, list(
    date = as.Date(rep(forecast_date, length(mm$values$name))),
    country = mm$values$name, predict = mm$values$predict,
    lower = mm$values$lower, upper = mm$values$upper
  ),stringsAsFactors = FALSE))
  save(world_evo_df, file = world_evo_file)

  #######################
  # ITALY DATA ANALYSIS #
  #######################
  print("National Analysis...")
  death <- unlist(lapply(data_ita, "[[", "deceduti"))
  calendar <- as.Date(unlist(lapply(data_ita, "[[", "data")))
  plot_calendar <- seq(calendar[1], calendar[1] + end - 1, 1)

  # italian prediction
  out_italy <- predict.covid(plot_calendar, death, end_date = forecast_date, total_length = end, name = "Italy")
  df <- as.data.frame(out_italy)

  # useful text to plot
  saturation_text <- paste("Saturation =", round(max(df$predict)), "+-", round(max(df$delta)))
  predict_text <- paste("Tomorrow Total Forecast =", round(df$predict[length(death) + 1])) # , "+-", round(df$delta[last_day+1]) )
  maxdaily_text <- paste("Max Daily Deaths =", round(max(diff(df$predict))))
  daysofmax_text <- paste("Day of Max =", plot_calendar[which.max(c(0, diff(df$predict)))])
  delta_text <- paste("Tomorrow Delta Forecast =", round(diff(df$predict)[length(death)])) # , "+-", round(diff(df$delta))[length(death)])
  first_day_out <- paste("First day below 100 deaths =", plot_calendar[c(0, diff(df$predict)) < 100 & c(0,diff(c(0,diff(df$predict))))][1])

  # ggplot conversion of world plots
  italy_daily <- list()
  theplot[[1]] <- italy_daily[[1]] <- ggplot(df) +
    geom_point(mapping = aes(x = calendar, y = death, col = name)) +
    geom_line(mapping = aes(x = calendar, y = predict, col = name)) +
    labs(title = paste0("Italy Estimated COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
    geom_ribbon(aes(x = calendar, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1) +
    theme_light(base_size = bsize) +
    scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none", legend.title = element_blank()) +
    scale_color_manual(values = kol) +
    scale_fill_manual(values = kol, guide = "none") +
    annotate("text", x = xdates[1], y = lim_italy_cdf[2] - delta_italy_cdf, label = saturation_text, size = 5, hjust = 0) +
    annotate("text", x = xdates[1], y = lim_italy_cdf[2] - 2 * delta_italy_cdf, label = predict_text, size = 5, hjust = 0) +
    coord_cartesian(ylim = lim_italy_cdf)

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
    annotate("text", x = xdates[1], y = lim_italy_pdf[2] - 3 * delta_italy_pdf, label = maxdaily_text, size = 5, hjust = 0) +
    annotate("text", x = xdates[1], y = lim_italy_pdf[2] - 2 * delta_italy_pdf, label = daysofmax_text, size = 5, hjust = 0) +
    annotate("text", x = xdates[1], y = lim_italy_pdf[2] - 4 * delta_italy_pdf, label = delta_text, size = 5, hjust = 0) +
    annotate("text", x = xdates[1], y = lim_italy_pdf[2] - delta_italy_pdf, label = first_day_out, size = 5, hjust = 0) +
    annotate("text", x = xdates[2], y = lim_italy_pdf[2] - delta_italy_pdf, label = paste0("Last Updated: ", now), hjust = 1) +
    coord_cartesian(ylim = lim_italy_pdf)

  ml <- arrangeGrob(grobs = italy_daily, ncol = 1, nrow = 2, plot = F)
  ggsave(
    file = file.path(FIGDIR_ITA, paste0("Italy_COVID_prediction_day_", forecast_date, ".pdf")),
    ml, height = 10, width = 12
  )

  # create a R workspace for storing the forecast in time
  if (forecast_mode == "today") {
    italy_evo_df <- subset(italy_evo_df, date != as.Date(now))
  }
  italy_evo_df <- unique(rbind(italy_evo_df, list(
    date = as.Date(forecast_date), country = "Italy",
    predict = max(df$predict), lower = max(df$lower), upper = max(df$upper)
  )))
  save(italy_evo_df, file = italy_evo_file)

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

    print(region)
    out_region[[region]] <- predict.covid(regions_plot_calendar, death, end_date = forecast_date, total_length = end, name = region)

    # print info
    # print(paste(region, ":", rev(death)[1], "->", rev(diff(death))[1]))
  }

  # create a data frame
  df <- dataframing(out_region, names = regions)

  # create legends
  mm <- somelegend(df)

  region_daily <- list()
  theplot[[3]] <- region_daily[[1]] <- ggplot(df) +
    geom_point(mapping = aes(x = calendar, y = death, col = name)) +
    geom_line(mapping = aes(x = calendar, y = predict, col = name, linetype = validity)) +
    geom_ribbon(aes(x = calendar, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1) +
    scale_fill_manual(values = kol, guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
    labs(title = paste0("Italian Regional Estimated COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
    theme_calendar(base_size = bsize) +
    scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
    scale_color_manual(values = kol, labels = mm$forecast) +
    coord_cartesian(ylim = lim_regio_cdf)

  theplot[[4]] <- region_daily[[2]] <- ggplot(df) +
    geom_point(df, mapping = aes(x = calendar, y = c(0, diff(death)), col = name)) +
    geom_line(df, mapping = aes(x = calendar, y = c(0, diff(predict)), linetype = validity, col = name)) +
    geom_ribbon(aes(x = calendar, ymin = c(0, diff(lower)), ymax = c(0, diff(upper)), fill = name), alpha = .2, size = 0.1) +
    scale_fill_manual(values = kol, guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
    labs(title = paste0("Italian Regions Estimated daily COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
    theme_calendar(base_size = bsize) +
    scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
    scale_color_manual(values = kol, labels = mm$diff) +
    coord_cartesian(ylim = lim_regio_pdf)

  ml <- arrangeGrob(grobs = region_daily, ncol = 1, nrow = 2, plot = F)
  ggsave(
    file = file.path(FIGDIR_REG, paste0("italian_regions_COVID_prediction_day_", forecast_date, ".pdf")),
    ml, height = 10, width = 12
  )
  
  # great plot
  ml <- arrangeGrob(grobs = theplot, ncol = 2, nrow = 5, plot = F)
  ggsave(
    file = file.path(FIGDIR, paste0("covid-status-", forecast_date, ".svg")),
    plot = ml, height = 28, width = 22, system_fonts = list(sans = "Helvetica")
  )
}

#######################
# EVOLUTION ANALYSIS #
#######################

# forcily need to push to date
italy_evo_df$date <- as.Date(italy_evo_df$date, origin = "1970-01-01")
forecast_evo <- NULL
forecast_evo[[1]] <- ggplot(italy_evo_df) +
  geom_point(mapping = aes(x = date, y = predict, col = country)) +
  geom_line(mapping = aes(x = date, y = predict, col = country)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = country), alpha = .2, size = 0.1) +
  scale_fill_manual(values = kol, guide = "none") +
  labs(title = paste0("Time Evolution of the Forecasts"), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = c(as.Date("2020-03-15"), as.Date(now))) +
  scale_color_manual(values = kol) +
  coord_cartesian(ylim = c(0, 60000))

ml <- arrangeGrob(grobs = forecast_evo, ncol = 1, nrow = 1, plot = F)
ggsave(
  file = file.path(FIGDIR, paste0("forecast_predict_evolution.svg")),
  ml, height = 9, width = 12
)

breaks <- c(1000, 2000, 5000, 10000, 20000, 50000, 100000)
minor_breaks <- rep(1:9, 21) * (10^rep(-10:10, each = 9))

# forcily need to push to date
world_evo_df$date <- as.Date(world_evo_df$date, origin = "1970-01-01")
world_evo_df$country <- factor(world_evo_df$country, levels = countries)
forecast_evo <- NULL
forecast_evo[[1]] <- ggplot(world_evo_df) +
  geom_point(mapping = aes(x = date, y = predict, col = country)) +
  geom_line(mapping = aes(x = date, y = predict, col = country)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = country), alpha = .2, size = 0.1) +
  scale_fill_manual(values = kol, guide = "none") +
  labs(title = paste0("Time Evolution of the Forecasts (log)"), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = c(as.Date("2020-03-15"), as.Date(now))) +
  # scale_y_continuous(trans="log10",  breaks = c(2000, 5000, 10000, 20000, 50000, 100000, 150000) ,labels = comma) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, labels = comma) +
  annotation_logticks() +
  scale_color_manual(values = kol) +
  coord_cartesian(ylim = c(2000, 200000))


ml <- arrangeGrob(grobs = forecast_evo, ncol = 1, nrow = 1, plot = F)
ggsave(
  file = file.path(FIGDIR, paste0("forecast_predict_world_evolution.svg")),
  ml, height = 9, width = 12
)

# library("plotly")
# library("htmlwidgets")
# f <- subplot(ggplotly(theplot[[1]]), ggplotly(theplot[[2]]), ggplotly(theplot[[3]]), ggplotly(theplot[[4]]),
#            #ggplotly(theplot[[5]]), ggplotly(theplot[[6]]), ggplotly(theplot[[7]]), ggplotly(theplot[[8]]),
#             nrows = 2, shareX = T, which_layout = 1)
# htmlwidgets::saveWidget(as_widget(f), file.path(FIGDIR, "plotly.html"), selfcontained=F)

italian_table <- cbind(
  Calendar = out_italy$calendar, Italy = out_italy$predict,
  as.data.frame(lapply(out_region, "[[", "predict"))
)
write.csv(italian_table, file.path(COVID, "Cointreau/forecasts", paste0("italian-forecast-", forecast_date, ".csv")))


#######################
# ENGLAND DATA ANALYSIS #
#######################
print("England Analysis...")

# remove useless rows
good <- 5:length(data_england[, 1])
england_dates <- as.Date(data_england[good, 1])
england_plot_calendar <- seq(england_dates[1], england_dates[1] + end - 1, 1)

# loop on countries, checks for regions, list handling
out_england <- list()
for (region in england_regions) {
  death <- cumsum(as.numeric(as.character(data_england[good, which(data_england[1, ] == region)])))

  out_england[[region]] <- predict.covid(england_plot_calendar, death, end_date = forecast_date, total_length = end, name = region)

  # print info
  print(paste(region, ":", rev(death)[1], "->", rev(diff(death))[1]))
}

# create a data frame
df <- dataframing(out_england, names = england_regions)

# create legends
mm <- somelegend(df)


# ggplot conversion of world plots
england_daily <- list()
england_daily[[1]] <- ggplot(df) +
  geom_point(mapping = aes(x = calendar, y = death, col = name)) +
  geom_line(mapping = aes(x = calendar, y = predict, col = name, linetype = validity)) +
  geom_ribbon(aes(x = calendar, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1) +
  scale_fill_manual(values = kol, guide = "none") +
  scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
  labs(title = paste0("England Macro-Area Estimated COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  scale_color_manual(values = kol, labels = mm$forecast) +
  coord_cartesian(ylim = lim_eng_cdf)

england_daily[[2]] <- ggplot(df) +
  geom_point(df, mapping = aes(x = calendar, y = c(0, diff(death)), col = name)) +
  geom_line(df, mapping = aes(x = calendar, y = c(0, diff(predict)), linetype = validity, col = name)) +
  geom_ribbon(aes(x = calendar, ymin = c(0, diff(lower)), ymax = c(0, diff(upper)), fill = name), alpha = .2, size = 0.1) +
  scale_fill_manual(values = kol, guide = "none") +
  scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +
  labs(title = paste0("England Macro-Area Estimated daily COVID-19 deaths at ", forecast_date), x = "", y = "# of deaths") +
  theme_calendar(base_size = bsize) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b", limits = xdates) +
  scale_color_manual(values = kol, labels = mm$diff) +
  coord_cartesian(ylim = lim_eng_pdf)

ml <- arrangeGrob(grobs = england_daily, ncol = 1, nrow = 2, plot = F)
ggsave(
  file = file.path(FIGDIR_ENG, paste0("england_regions_COVID_prediction_day_", forecast_date, ".pdf")),
  ml, height = 10, width = 12
)
