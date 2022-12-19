# Create np inputs

create_np_inputs <- function(time, PAR = NULL, swr = NULL, temp = NULL) {

  # year <- lubridate::year(time[1])
  ndays <- round(as.numeric(difftime(time, time[1], units = "days")))

  # PAR calculations ----
  if(is.null(PAR) & !is.null(swr)) {
    PAR <- LakeMetabolizer::sw.to.par.base(swr)
  }

  if(is.null(PAR) & is.null(swr)) {
    PAR <- 0.5 * (540 + 440 * sin(2 * pi * ndays/365-1.4))
  }

  if(length(temp) == 2) {
    TEMP <- (temp[1] + temp[2] * sin(2*pi*ndays/365-1.4))
  } else {
    TEMP <- temp
  }


  out = data.frame(Day = ndays, PAR = PAR, TEMP = TEMP)


  #Read to a .csv file
  # write.csv(out, file.path("data", out_file), row.names = FALSE)
  return(out)

}

# END
