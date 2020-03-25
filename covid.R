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
kol <- c("darkgreen", "red", "orange", "blue", "black", "green", "darkviolet", "pink", "aquamarine3")

# directories declaration
COVID <- "/Users/paolo/Desktop/covid"

# need local fectch of John Hopkins and Italian Civil Protection data
DIR_ITA <- file.path(COVID, "COVID-19/dati-json")
DIR_WORLD <- file.path(COVID, "COVID-WORLD/csse_covid_19_data/csse_covid_19_time_series")

# for figures
FIGDIR <- file.path(COVID, "prediction")
FIGDIR2 <- file.path(COVID, "world_prediction")
FIGDIR3 <- file.path(COVID, "italian_regions")
dir.create(FIGDIR3, FIGDIR, FIGDIR2)

# for updates
now <- Sys.time()

# setup
countries <- c("Italy", "Spain", "China", "France", "United Kingdom", "Iran", "Korea, South", "Japan", "US")
regions <- c("Lombardia", "Veneto", "Emilia Romagna", "Piemonte", "Marche", "Liguria")

# days of prediction since the first data
end <- 150

# gompertz fitter
predict.covid <- function(calendar, death, name = "Italy", total_length = 150, min_death = 5, verbose = F) {

  # Gompertz initialization parameters
  alpha <- 9526
  beta <- 9.1618
  k <- 0.0028

  # days
  days <- 1:length(death)

  # remove missing values
  days <- days[!is.na(death)]
  death <- death[!is.na(death)]

  # create dataframe
  data <- data.frame(days = days[death > min_death], death = death[death > min_death])

  # gompertz fit
  nls.gompertz <- minpack.lm::nlsLM(data$death ~ alpha * exp(-beta * exp(-k * data$days)),
    data = data,
    start = list(alpha = alpha, beta = beta, k = k), control = list(maxiter = 500)
  )

  # upper and lower bound coefficinets
  #upper.coef.gompertz <- coef(nls.gompertz) + 2 * summary(nls.gompertz)$parameter[, 2]
  #lower.coef.gompertz <- coef(nls.gompertz) - 2 * summary(nls.gompertz)$parameter[, 2]
  confidence <- confint2(nls.gompertz)
  #confidence <- confint(nls.gompertz)
  upper.coef.gompertz <- confidence[,"97.5 %"]
  lower.coef.gompertz <- confidence[,"2.5 %"]

  # predict gompertz
  predict.gompertz <- growthmodels::gompertz(1:end, alpha = coef(nls.gompertz)[["alpha"]], beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]])
  upper.gompertz <- growthmodels::gompertz(1:end, alpha = upper.coef.gompertz[["alpha"]], beta = upper.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]])
  lower.gompertz <- growthmodels::gompertz(1:end, alpha = lower.coef.gompertz[["alpha"]], beta = lower.coef.gompertz[["beta"]], k = coef(nls.gompertz)[["k"]])
  delta.gompertz <- (upper.gompertz - lower.gompertz) / 2
  
  # lower boundary cannot go below zero
  lower.gompertz[lower.gompertz<0] <- 0


  # lagged
  lagged.death <- death[death > 100]

  # outlist
  out <- list(
    calendar = calendar, days = 1:length(calendar), death = c(death, rep(NA, length(calendar) - length(death))),
    lagged = c(lagged.death, rep(NA, length(calendar) - length(lagged.death))), 
    predict = predict.gompertz, upper = upper.gompertz, lower = lower.gompertz, 
    delta = delta.gompertz, name = rep(name, length(calendar))
  ) # , coef = coef(nls.gompertz))
}

# running.mean
ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}

# Gompertz initialization parameters
alpha <- 9526
beta <- 9.1618
k <- 0.0028

# load files
file_ita <- file.path(DIR_ITA, "dpc-covid19-ita-andamento-nazionale.json")
file_region <- file.path(DIR_ITA, "dpc-covid19-ita-regioni.json")
#file_world <- file.path(DIR_WORLD, "time_series_19-covid-Deaths.csv")
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
  if (country == "Hubei") {
    death <- unlist(data_world[data_world[, 1] == country, ])[good]
  } else if (country == "France" | country == "United Kingdom") {
    death <- unlist(data_world[data_world[, 1] == "" & data_world[, 2] == country, ])[good]
  } else if (country == "China" | country == "US") {
    death <- unlist(apply(data_world[data_world[, 2] == country, 4 + 1:length(dates)], 2, sum))
  } else {
    death <- unlist(data_world[data_world[, 2] == country, ])[good]
  }

  # call covid prediction
  out_country[[country]] <- predict.covid(world_plot_calendar, death, name = country, verbose = T)

  # print info
  print(paste(country, ":", rev(death)[1], "->", rev(diff(death))[1]))
}

# create a data frame
df <- rbindlist(out_country, fill = T)
df$name <- factor(df$name, levels = countries)

# status of deaths, lagged
world_plot <- list()
theplot[[7]] <- world_plot[[1]] <- ggplot(df,  aes(x = days, y = lagged, col = name)) +
  geom_point() + geom_line() +
  labs(title = "World COVID-19 Deaths when exceeding 100 counts", x = "# days", y = "# of deaths") +
  labs(col = "Countries") +
  theme_light(base_size = 15) +
  scale_color_manual(values = kol) +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,10000)) +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = c(.01, 0.99),
    legend.justification = c("left", "top"), legend.box.just = "right",
    legend.background = element_rect(fill = "transparent"))

theplot[[8]] <- world_plot[[2]] <- ggplot(df,  aes(x = days, y = c(0,diff(lagged)), col = name)) +
  geom_point() + geom_line() +
  labs(title = "World COVID-19 Daily Deaths when exceeding 100 counts", x = "# days", y = "# of deaths") +
  labs(col = "Countries") +
  theme_light(base_size = 15) +
  scale_color_manual(values = kol) +
  coord_cartesian(xlim = c(0, 60), ylim = c(0,1000)) +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = c(.01, 0.99),
    legend.justification = c("left", "top"), legend.box.just = "right",
    legend.background = element_rect(fill = "transparent"))

ml <- arrangeGrob(grobs = world_plot, ncol = 1, nrow = 2, plot = F)
ggsave(
  filename = file.path(FIGDIR2, paste0("status_world_", dates[length(dates)], ".pdf")), plot = ml,
  width = 10, height = 10
)

# convergences (if error larger than value set to Inf)
forecast <- aggregate(df[,c("predict","delta")], by = list(df$name), max)
forecast[which(forecast[,3]>forecast[,2]),2:3] <- NA
#forecast[which(is.na(forecast[,2])),2] <- NA
legend_text <- paste0(countries, ": ", round(forecast[,2]), "+-", round(forecast[,3]))

#my_theme    <- theme(axis.text=element_text(size=20),
#                     axis.title=element_text(size=14,face="bold"))

# ggplot conversion of world plots
world_daily <- list()
theplot[[5]] <- world_daily[[1]] <- ggplot(df) +    
  geom_point(mapping = aes(x = calendar, y = death, col = name)) +
  geom_line(mapping = aes(x = calendar, y = predict, col = name)) +
  labs(title = paste0("World Estimated COVID-19 deaths at ", dates[length(dates)]), x = "# days", y = "# of deaths") +
  labs(col = "") +
  theme_light(base_size = 15) +
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") + 
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = c(.01, 0.99),
    legend.justification = c("left", "top"), legend.box.just = "right",
    legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = kol, labels = legend_text) +
  #annotate("text", x=as.Date(calendar[50]), y = seq(20000, by= -1000, length.out=length(countries)), label = paste0(countries, ": ", round(forecast[,2]), "+-", round(forecast[,3]))) +
  coord_cartesian(ylim = c(0, 20000))

theplot[[6]] <- world_daily[[2]] <- ggplot(df) +
  geom_point(df, mapping = aes(x = calendar, y = c(0,diff(death)), col = name)) +
  geom_line(df, mapping = aes(x = calendar, y = c(0,diff(predict)), col = name)) +
  labs(title = paste0("World Estimated daily COVID-19 deaths at ", dates[length(dates)]), x = "# days", y = "# of deaths") +
  labs(col = "") +
  theme_light(base_size = 15) +
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +      
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = c(.01, 0.99),
    legend.justification = c("left", "top"), legend.box.just = "right",
    legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = kol) +
  annotate("text", x = world_plot_calendar[120], y = 900, label = paste0("Last Updated: ", now)) +
  coord_cartesian(ylim = c(0, 1000))

ml <- arrangeGrob(grobs = world_daily, ncol = 1, nrow = 2, plot = F)
ggsave(file = file.path(FIGDIR2, paste0("world_COVID_prediction_day_", dates[length(dates)], ".pdf")), 
ml, height = 10, width = 12)


#######################
# ITALY DATA ANALYSIS #
#######################
print("National Analysis...")
full_death <- unlist(lapply(data_ita, "[[", 10))
full_calendar <- as.Date(unlist(lapply(data_ita, "[[", 1)))
plot_calendar <- seq(full_calendar[1], full_calendar[1] + end - 1, 1)

for (last_day in 20:length(full_death)) {
  days <- 1:last_day
  death <- full_death[days]
  calendar <- full_calendar[days]
  data <- data.frame(days = days, death = death)

  fit <- lm(log(data$death) ~ data$days)
  predict.exp <- exp(coef(fit)[1]) * exp(coef(fit)[2] * 1:end)
  out_italy <- predict.covid(plot_calendar, death, name = "Italy", verbose = T)
  df <- as.data.frame(out_italy)
  saturation_text <- paste0("Saturation", "= ", round(max(df$predict)), "+-", round(max(df$delta)))
  predict_text <- paste("Tomorrow Total Forecast = ", round(df$predict[last_day+1]))#, "+-", round(df$delta[last_day+1]) )
  maxdaily_text <- paste0("Max Daily Deaths = ", round(max(diff(df$predict)))) 
  daysofmax_text <-  paste("Days of Max = ", plot_calendar[which.max(diff(df$predict))])
  delta_text <- paste("Tomorrow Delta Forecast = ", round(diff(df$predict)[length(death)]))#, "+-", round(diff(df$delta))[length(death)])

  #  text(first_date - 2, 1400, paste("Max Daily Deaths = ", round(max(diff(predict.gompertz)))), pos = 4, cex = 1.5)
#  text(first_date - 2, 1300, paste("Days of Max = ", plot_calendar[which.max(diff(predict.gompertz))]), pos = 4, cex = 1.5)
#  text(first_date - 2, 1200, paste("Tomorrow Delta Forecast = ", round(predict.gompertz[length(death) + 1] - predict.gompertz[length(death)])), pos = 4, cex = 1.5)


  # ggplot conversion of world plots
  italy_daily <- list()
theplot[[1]] <-  italy_daily[[1]] <- ggplot(df) +
  geom_point(mapping = aes(x = calendar, y = death, col = name)) +
  geom_line(mapping = aes(x = calendar, y = predict, col = name)) +
  labs(title = paste0("Italy Estimated COVID-19 deaths at ", full_calendar[last_day]), x = "# days", y = "# of deaths") +
  geom_ribbon(aes(x = calendar, y = predict, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1)  +
  labs(col = "") +
  theme_light(base_size = 15) +
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "none") + 
  scale_color_manual(values = kol, labels = legend_text) +
  annotate("text", x=plot_calendar[20], y = 47000, label = saturation_text) +
  annotate("text", x=plot_calendar[20], y = 44000, label = predict_text) +
  coord_cartesian(ylim = c(0, 50000))

theplot[[2]] <-  italy_daily[[2]] <- ggplot(df) +
  geom_point(df, mapping = aes(x = calendar, y = c(0,diff(death)), col = name)) +
  geom_line(df, mapping = aes(x = calendar, y = c(0,diff(predict)), col = name)) +
  geom_ribbon(aes(x = calendar, y = c(0,diff(predict)), ymin = c(0, diff(lower)), ymax = c(0,diff(upper)), fill = name), alpha = .2, size = 0.1)  +
  labs(title = paste0("Italy Estimated daily COVID-19 deaths at ", full_calendar[last_day]), x = "# days", y = "# of deaths") +
  labs(col = "Countries") +
  theme_light(base_size = 15) +
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "none") + 
  scale_color_manual(values = kol) +
  annotate("text", x=plot_calendar[20], y = 1400, label = maxdaily_text) +
  annotate("text", x=plot_calendar[20], y = 1300, label = daysofmax_text) +
  annotate("text", x=plot_calendar[20], y = 1200, label = delta_text) +
  coord_cartesian(ylim = c(0, 1500))

  ml <- arrangeGrob(grobs = italy_daily, ncol = 1, nrow = 2, plot = F)
  ggsave(file = file.path(FIGDIR, paste0("Italy_COVID_prediction_day_", full_calendar[last_day], ".pdf")),
    ml, height = 10, width = 12)

}


#  pdf(
#    file = file.path(FIGDIR, paste0("COVID_prediction_day", last_day, ".pdf")),
#    width = 16, height = 12, onefile = T, bg = "white", family = "Helvetica"
#  )
#  par(mfrow = c(2, 1))
#  plot(plot_calendar[1:length(days)], death,
#    pch = 18, xlim = c(first_date, max(plot_calendar)), ylim = c(0, 50000),
#    xlab = "Days since 24 February", ylab = "# of deaths", main = "Italy Estimated COVID-19 deaths",
#    cex.main = 3, cex.lab = 2, cex.axis = 2, cex = 1.5
#  )
#  grid()
#  polygon(x = c(plot_calendar, rev(plot_calendar)), y = c(upper.gompertz, rev(lower.gompertz)), col = adjustcolor("cyan", alpha = 0.2))
#  points(plot_calendar, predict.exp, col = "darkred", type = "l", lwd = 2)
#  points(plot_calendar, upper.gompertz, col = "darkblue", type = "l", lwd = 1, lty = 3)
#  points(plot_calendar, lower.gompertz, col = "darkblue", type = "l", lwd = 1, lty = 3)
#  # estimation <- c(predict.gompertz)
#  points(plot_calendar, predict.gompertz, col = "darkblue", type = "l", lwd = 2)
#  if (n.gompertz > 5) {
#    points(plot_calendar, worst.gompertz, col = "darkgreen", type = "l", lwd = 1, lty = 1)
#    points(plot_calendar, best.gompertz, col = "darkgreen", type = "l", lwd = 1, lty = 1)
#  }
#  text(first_date - 2, 48000, paste("Convergence = ", round(max(predict.gompertz)), "+-", round(max(delta.gompertz))), pos = 4, cex = 1.5)
#  text(first_date - 2, 44000, paste("Tomorrow Total Forecast = ", round(predict.gompertz[length(death) + 1])), pos = 4, cex = 1.5)
#  text(plot_calendar[90], 5000, paste("Prediction issued at day", length(death), "i.e.", calendar[length(death)]))
#  text(plot_calendar[90], 8000, paste0("Alpha = ", round(coef(nls.gompertz)["alpha"], 2), "; Beta = ", round(coef(nls.gompertz)["beta"], 2)))
#  legend("topright", c("Exponential Fit", "Gompertz Fit", "Gompertz +- 2STD coefficients fit", "Best/Worse Gompertz Fit in last 5 days"), col = c("darkred", "darkblue", "darkblue", "darkgreen"), lty = c(1, 1, 3, 1), lwd = 2)
#
#  diff_calendar <- plot_calendar[-1]
#  plot(plot_calendar[2:length(days)], diff(death),
#    pch = 18, xlim = c(first_date, max(plot_calendar)), ylim = c(0, 1500),
#    xlab = "Days since 24 February", ylab = "# of daily deaths", main = "Italy daily estimated COVID-19 deaths",
#    cex.main = 3, cex.lab = 2, cex.axis = 2, cex = 1.5
#  )

#  text(first_date - 2, 1400, paste("Max Daily Deaths = ", round(max(diff(predict.gompertz)))), pos = 4, cex = 1.5)
#  text(first_date - 2, 1300, paste("Days of Max = ", plot_calendar[which.max(diff(predict.gompertz))]), pos = 4, cex = 1.5)
#  text(first_date - 2, 1200, paste("Tomorrow Delta Forecast = ", round(predict.gompertz[length(death) + 1] - predict.gompertz[length(death)])), pos = 4, cex = 1.5)

#  grid()



#  dev.off()
#}

#######################
# REGION DATA ANALYSIS #
#######################
print("Regional Analysis...")

calendar_regions <- as.Date(unique(unlist(lapply(data_regioni, "[[", 1))))
regions_plot_calendar <- seq(calendar_regions[1], calendar_regions[1] + end - 1, 1)
full_regions <- unlist(lapply(data_regioni, "[[", 4))
death_regions <- unlist(lapply(data_regioni, "[[", 14))

# loop on countries, checks for regions, list handling
out_region <- list()
for (region in regions) {

  death <- death_regions[which(full_regions == region)]

  out_region[[region]] <- predict.covid(regions_plot_calendar, death, name = region, verbose = T)

  # print info
  print(paste(region, ":", rev(death)[1], "->", rev(diff(death))[1]))
}

# create a data frame
df <- rbindlist(out_region, fill = T)
df$name <- factor(df$name, levels = regions)

# convergences (if error larger than value set to Inf)
forecast <- aggregate(df[,c("predict","delta")], by = list(df$name), max)
forecast[which(forecast[,3]>forecast[,2]),2:3] <- NA
legend_text <- paste0(regions, ": ", round(forecast[,2]), "+-", round(forecast[,3]))

# ggplot conversion of world plots
region_daily <- list()
theplot[[3]] <- region_daily[[1]] <- ggplot(df) +   
  geom_point(mapping = aes(x = calendar, y = death, col = name)) +
  geom_line(mapping = aes(x = calendar, y = predict, col = name)) +
  #geom_ribbon(aes(x = calendar, y = predict, ymin = lower, ymax = upper, fill = name), alpha = .2, size = 0.1)  + 
  scale_fill_manual(values = kol, guide = "none") +
  labs(title = paste0("Italian Regional Estimated COVID-19 deaths at ", dates[length(dates)]), x = "# days", y = "# of deaths") +
  labs(col = "") +
  theme_light(base_size = 15) +
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = c(.01, 0.99),
    legend.justification = c("left", "top"), legend.box.just = "right",
    legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = kol, labels = legend_text) +
  #annotate("text", x=as.Date(calendar[50]), y = seq(20000, by= -1000, length.out=length(countries)), label = paste0(countries, ": ", round(forecast[,2]), "+-", round(forecast[,3]))) +
  coord_cartesian(ylim = c(0, 15000))

theplot[[4]] <- region_daily[[2]] <- ggplot(df) +
  geom_point(df, mapping = aes(x = calendar, y = c(0,diff(death)), col = name)) +
  geom_line(df, mapping = aes(x = calendar, y = c(0,diff(predict)), col = name)) +
  #geom_ribbon(aes(x = calendar, y = c(0,diff(predict)), ymin = c(0, diff(lower)), ymax = c(0,diff(upper)), fill = name), alpha = .2, size = 0.1)  + 
  scale_fill_manual(values = kol, guide = "none") +
  labs(title = paste0("Italian Regions Estimated daily COVID-19 deaths at ", dates[length(dates)]), x = "# days", y = "# of deaths") +
  labs(col = "") +
  theme_light(base_size = 15) +
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +     
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = c(.01, 0.99),
    legend.justification = c("left", "top"), legend.box.just = "right", 
    legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = kol) +
  #annotate("text", x = world_plot_calendar[120], y = 900, label = paste0("Last Updated: ", now)) +
  coord_cartesian(ylim = c(0, 500))

ml <- arrangeGrob(grobs = region_daily, ncol = 1, nrow = 2, plot = F)
ggsave(file = file.path(FIGDIR3, paste0("italian_regions_COVID_prediction_day_", dates[length(dates)], ".pdf")),
ml, height = 10, width = 12)


#for (region in regions) {
#  death <- death_regions[which(full_regions == region)]
#  days <- 1:length(calendar_regions)
#  calendar <- full_calendar[days]
#  data <- data.frame(days = days, death = death)

#  nls.gompertz <- minpack.lm::nlsLM(data$death ~ alpha * exp(-beta * exp(-k * data$days)), data = data, start = list(alpha = alpha, beta = beta, k = k), control = list(maxiter = 500))
#
#  upper.coef.gompertz <- coef(nls.gompertz) + summary(nls.gompertz)$parameter[, 2]
#  lower.coef.gompertz <- coef(nls.gompertz) - summary(nls.gompertz)$parameter[, 2]
#
#  growth.gompertz <- growthmodels::gompertz(data$days, alpha = coef(nls.gompertz)[["alpha"]], beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]])
#  predict.gompertz <- growthmodels::gompertz(1:end, alpha = coef(nls.gompertz)[["alpha"]], beta = coef(nls.gompertz)[["beta"]], k = coef(nls.gompertz)[["k"]])
#  upper.gompertz <- growthmodels::gompertz(1:end, alpha = upper.coef.gompertz[["alpha"]], beta = upper.coef.gompertz[["beta"]], k = upper.coef.gompertz[["k"]])
#  lower.gompertz <- growthmodels::gompertz(1:end, alpha = lower.coef.gompertz[["alpha"]], beta = lower.coef.gompertz[["beta"]], k = lower.coef.gompertz[["k"]])
#  delta.gompertz <- (upper.gompertz - lower.gompertz) / 2
#  assign(paste0("predict_", region), predict.gompertz)
#  assign(paste0("upper_", region), upper.gompertz)
#  assign(paste0("lower_", region), lower.gompertz)
#  assign(paste0("death_", region), death)
#  assign(paste0("delta_", region), delta.gompertz)
#}

#pdf(
#  file = file.path(FIGDIR3, paste0("italian_regions_COVID_prediction_day", length(days), ".pdf")),
#  width = 16, height = 12, onefile = T, bg = "white", family = "Helvetica"
#)
#par(mfrow = c(2, 1))
#plot(plot_calendar[1:length(days)], death,
#  type = "n", xlim = c(min(dates), max(world_plot_calendar)), ylim = c(0, 15000),
#  xlab = "Days", ylab = "# of deaths", main = "Estimated COVID-19 deaths",
#  cex.main = 3, cex.lab = 2, cex.axis = 2, cex = 1.5
#)
#grid()
#toplot <- NULL
#for (region in regions) {
#  # polygon(x = c(plot_calendar, rev(plot_calendar)), y = c(get(paste0("upper_",region)), rev(get(paste0("lower_",region)))),
#  #       col = adjustcolor(kol[which(region==regions)], alpha = 0.1))
#  points(plot_calendar, get(paste0("predict_", region)), type = "l", col = kol[which(region == regions)], lwd = 2)
#  points(plot_calendar[1:length(days)], get(paste0("death_", region)), pch = 19, col = kol[which(region == regions)])
#  toplot <- c(toplot, paste0(region, ": ", round(max(get(paste0("predict_", region)))), "+-", round(max(get(paste0("delta_", region))))))
#}
#legend("topleft", toplot, col = kol, lwd = 2, cex = 1.2)
#
#plot(plot_calendar[2:length(days)], diff(death),
#  type = "n", xlim = c(min(dates), max(world_plot_calendar)), ylim = c(0, 500),
#  xlab = "Days", ylab = "# daily of deaths", main = "Daily estimated COVID-19 deaths",
#  cex.main = 3, cex.lab = 2, cex.axis = 2, cex = 1.5
#)
#grid()
#for (region in regions) {
#  # polygon(x = c(plot_calendar, rev(plot_calendar)), y = c(get(paste0("upper_",region)), rev(get(paste0("lower_",region)))),
#  #       col = adjustcolor(kol[which(region==regions)], alpha = 0.1))
#  points(plot_calendar[-1], diff(get(paste0("predict_", region))), type = "l", col = kol[which(region == regions)], lwd = 2)
#  points(plot_calendar[2:length(days)], diff(get(paste0("death_", region))), pch = 19, col = kol[which(region == regions)])
#}
#legend("topleft", regions, col = kol, lwd = 2, cex = 1.2)
#dev.off()



 ml <- arrangeGrob(grobs = theplot, ncol = 2, nrow = 4, plot = F)
  ggsave(file = file.path(FIGDIR, "theplot.svg"), plot = ml, height = 17, width = 26)
