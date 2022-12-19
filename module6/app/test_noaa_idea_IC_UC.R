library(ncdf4)
library(ggplot2)

scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind

siteID <- "BARC"
fpath <- "data/NOAAGEFS_1hr/BARC/"
fc_date <- "2020-09-25"
fc_date <- list.files(fpath)[1]
fpath2 <- file.path(fpath, fc_date[1], "00")
fils <- list.files(fpath2, full.names = TRUE)
fils <- fils[-c(grep("ens00", fils))]
fid <- nc_open(file.path(fils[1]))
vars <- fid$var # Extract variable names for selection
fc_vars <- names(vars)[c(1)] # 5
membs <- 1 #length(fils)
ncdf4::nc_close(fid)


# fpath <- file.path("data", "NOAAGEFS_1hr", siteID)

out <- lapply(fc_date, function(dat) {
  idx <- which(fc_date == dat)

  fpath2 <- file.path(fpath, dat, "00")
  fils <- list.files(fpath2)
  fils <- fils[-c(grep("ens00", fils))][1]

  for( i in seq_len(length(fils))) {

    fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID, dat,
                                    "00", fils[i]))
    tim = ncvar_get(fid, "time")
    tunits = ncatt_get(fid, "time")
    lnam = tunits$long_name
    tustr <- strsplit(tunits$units, " ")
    step = tustr[[1]][1]
    tdstr <- strsplit(unlist(tustr)[3], "-")
    tmonth <- as.integer(unlist(tdstr)[2])
    tday <- as.integer(unlist(tdstr)[3])
    tyear <- as.integer(unlist(tdstr)[1])
    tdstr <- strsplit(unlist(tustr)[4], ":")
    thour <- as.integer(unlist(tdstr)[1])
    tmin <- as.integer(unlist(tdstr)[2])
    origin <- as.POSIXct(paste0(tyear, "-", tmonth,
                                "-", tday, " ", thour, ":", tmin),
                         format = "%Y-%m-%d %H:%M", tz = "UTC")
    if (step == "hours") {
      tim <- tim * 60 * 60
    }
    if (step == "minutes") {
      tim <- tim * 60
    }
    time = as.POSIXct(tim, origin = origin, tz = "UTC")
    var_list <- lapply(fc_vars, function(x) {
      if(x == "air_temperature") {
        data.frame(time = time, value = (ncdf4::ncvar_get(fid, x) -  273.15))
      } else {
        data.frame(time = time, value = (ncdf4::ncvar_get(fid, x)))
      }
    })


    ncdf4::nc_close(fid)
    names(var_list) <- fc_vars

    mlt1 <- reshape::melt(var_list, id.vars = "time")
    mlt1 <- mlt1[, c("time", "L1", "value")]

    # df <- get_vari(file.path("data", fils[i]), input$fc_var, print = F)
    cnam <- paste0("ens", formatC(i, width = 2, format = "d", flag = "0"))
    if(i == 1) {
      df2 <- mlt1
      colnames(df2)[3] <- cnam
    } else {
      df2 <- merge(df2, mlt1, by = c(1,2))
      colnames(df2)[ncol(df2)] <- cnam
    }

  }
  return(df2)
})

names(out) <- fc_date
out$`2020-09-25`$L1 <- "Air temperature"

mlt <- reshape::melt(out[[1]], id.vars = c("time", "L1"))
idx1 <- which(mlt$time == mlt$time[1])
idx2 <- which(mlt$time == (mlt$time[1] + lubridate::days(6) + lubridate::hours(12)))

dat <- mlt[idx1, ]
dat$time2 <- dat$time + rnorm(nrow(dat), mean = 0, sd = 6000)

dat2 <- rbind(mlt[idx2, ])
dat2$time2 <- dat2$time + rnorm(nrow(dat2), mean = 0, sd = 6000)

cur_df <- data.frame(time = dat$time2, value = dat$value, variable = dat$variable, xend = dat2$time2, yend = dat2$value)

st <- data.frame(time = c(mlt$time[1], (mlt$time[1] + lubridate::days(1))),
                  mean = c(mean(dat$value, na.rm = TRUE), mean(dat2$value, na.rm = TRUE)),
                  sd = c(sd(dat$value, na.rm = TRUE), sd(dat2$value, na.rm = TRUE)),
                 null = rep(mean(dat$value, na.rm = TRUE), 2),
                 L1 = dat$L1[1])
xlims <- c(mlt$time[1], (mlt$time[1] + lubridate::days(6) + lubridate::hours(12)))
ylims <- range(mlt$value, mlt$value, na.rm = TRUE)
ylab <- "Temperature (\u00B0C)"
xlab <- "Time"


ggplot() +
  geom_point(data = st, aes(time, null, color = "NULL"), size = 2) + # Mean
  geom_line(data = st, aes(time, null, color = "NULL"), size = 1) + # Mean
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  ylab(ylab) +
  xlab(xlab) +
  facet_wrap(~L1, scales = "free_y", ncol = 1) +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  theme_minimal(base_size = 18)

st <- data.frame(time = c(mlt$time[1], (mlt$time[1] + lubridate::days(6) + lubridate::hours(12))),
                 mean = c(mean(dat$value, na.rm = TRUE), mean(dat2$value, na.rm = TRUE)),
                 sd = c(sd(dat$value, na.rm = TRUE), sd(dat2$value, na.rm = TRUE)),
                 null = rep(mean(dat$value, na.rm = TRUE), 2),
                 L1 = dat$L1[1])

# Plot of curved lines from start to finish
ggplot() +
  geom_point(data = dat, aes(time2, value)) + # Day 1
  geom_point(data = dat2, aes(time2, value)) + # Day 7
  geom_point(data = st, aes(time, mean), color = "red", size = 5) + # Mean
  geom_curve(data = cur_df, aes(time, value, group = variable, xend = xend, yend = yend),
             curvature = -0.2) + # Connecting curves
  geom_errorbar(data = st, aes(time, ymin = mean - sd, ymax = mean + sd),
                color = "red", width = 12000, size = 2) + # Error bars - Std. Dev
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  stat_ellipse(data = dat, aes(time2, value), na.rm = TRUE, inherit.aes = FALSE, type = "norm") +
  stat_ellipse(data = dat2, aes(time2, value), na.rm = TRUE, inherit.aes = FALSE) +
  facet_wrap(~L1, scales = "free_y", ncol = 1) +
  ylab(ylab) +
  xlab(xlab) +
  # coord_cartesian(xlim = c(mlt$time[1], (mlt$time[1] + lubridate::days(6) + lubridate::hours(12)))) +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  theme_minimal(base_size = 18)

# Plot of time series of Air temperature forecast
ggplot() +
  geom_point(data = dat, aes(time2, value)) + # Day 1
  # geom_point(data = dat2, aes(time2, value)) + # Day 7
  geom_point(data = st, aes(time, mean), color = "red", size = 5) + # Mean
  geom_line(data = mlt[mlt$time <= xlims[2], ], aes(time, value, group = variable)) +
  geom_errorbar(data = st, aes(time, ymin = mean - sd, ymax = mean + sd),
                color = "red", width = 12000, size = 2) + # Error bars - Std. Dev
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  stat_ellipse(data = dat, aes(time2, value), na.rm = TRUE, inherit.aes = FALSE) +
  stat_ellipse(data = dat2, aes(time2, value), na.rm = TRUE, inherit.aes = FALSE) +
  facet_wrap(~L1, scales = "free_y", ncol = 1) +
  ylab(ylab) +
  xlab(xlab) +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  theme_minimal(base_size = 18)


# end
