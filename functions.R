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

  df$validity <- factor(df$validity, levels = c(1,0))
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
  legend_ratio <- paste0(forecast[, 1], ": ", round(as.numeric(forecast[, 2]) / forecast[, 7]), "+-", round(as.numeric(forecast[, 3]) / forecast[, 7]))

  # return a list
  outlist <- list(forecast = legend_forecast, diff = legend_diff, values = forecast, ratio = legend_ratio)
  return(outlist)
}

# gompertz fitter
predict.covid <- function(calendar, death, name = "Italy", start_date = starting_date, end_date = as.Date(now), total_length = 150, min_death = 5, verbose = F, moving_avg = T) {

  # Gompertz initialization parameters
  # They are taken from here, but as long as they converge they are fine
  # https://www.researchgate.net/post/Is_there_an_R_code_for_Gompertz_model
  # alpha <- 9526
  alpha <- 18000
  beta <- 9.1618
  # k <- 0.0028
  k <- 0.002

  # subset death up to the day of forecast
  if (end_date == as.Date(Sys.time())) {
    death <- death[which(calendar == start_date):length(death)]
  } else {
    death <- death[which(calendar == start_date):which(calendar == end_date)]
  }
  calendar <- calendar[which(calendar == start_date):length(calendar)]
  total_length <- length(calendar)

  # normalize by wave
  death <- death - min(death)

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
  predict.gompertz <- growthmodels::gompertz(1:total_length,
    alpha = coef(nls.gompertz)[["alpha"]],
    beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]]
  )


  boundaries <- "random"

  if (boundaries == "confint") {
    # upper and lower bound coefficinets
    confidence <- confint2(nls.gompertz)
    upper.coef.gompertz <- confidence[, "97.5 %"]
    lower.coef.gompertz <- confidence[, "2.5 %"]
    upper.gompertz <- growthmodels::gompertz(1:total_length,
      alpha = upper.coef.gompertz[["alpha"]],
      beta = upper.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]]
    )
    lower.gompertz <- growthmodels::gompertz(1:total_length,
      alpha = lower.coef.gompertz[["alpha"]],
      beta = lower.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]]
    )
    #print(max(upper.gompertz))
    #print(max(lower.gompertz))
  } else if (boundaries == "random") {
    # test with random noise in data + confint
    # sampling 100 times with random noise in order to include error in source data
    # random noise is Gaussian with zero mean and standard deviation equal to standard
    # deviation of daily mortality
    space <- 100
    random.gompertz.low <- random.gompertz.high <- array(NA, dim = c(space, total_length))
    #sdnoise <- sd(diff(data$death))
    sdnoise <- 5
    for (i in 1:space) {
      #noise <- round(rnorm(length(data$death), mean = 0, sd = sdnoise))
      noise <- sapply(1:length(data$death), function(l) rnorm(1, 0, max(sdnoise,c(0,diff(data$death))[l])*0.2))

      data$newdeath <- data$death + noise
      nls.gompertz <- minpack.lm::nlsLM(data$newdeath ~ alpha * exp(-beta * exp(-k * data$days)),
        data = data, start = list(alpha = alpha, beta = beta, k = k), control = list(maxiter = 500)
      )
      confidence <- confint2(nls.gompertz)
      upper.coef.gompertz <- confidence[, "97.5 %"]
      lower.coef.gompertz <- confidence[, "2.5 %"]

      random.gompertz.high[i, ] <- growthmodels::gompertz(1:total_length,
        alpha = upper.coef.gompertz[["alpha"]],
        beta = upper.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]]
      )
      random.gompertz.low[i, ] <- growthmodels::gompertz(1:total_length,
        alpha = lower.coef.gompertz[["alpha"]],
        beta = lower.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]]
      )
    }
    upper.gompertz <- apply(random.gompertz.high, 2, max, na.rm=T)
    lower.gompertz <- apply(random.gompertz.low, 2, min, na.rm=T)
    #print(max(upper.gompertz))
    #print(max(lower.gompertz))
  }

  # delta boundaries
  delta.gompertz <- abs(upper.gompertz - lower.gompertz) / 2

  # lower boundary cannot go below zero
  lower.gompertz[lower.gompertz < 0] <- 0

  # lagged
  lagged.death <- death[death > 100]

  # reliability of the prediction
  #print(max(delta.gompertz))
  #print(max(predict.gompertz))
        
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
