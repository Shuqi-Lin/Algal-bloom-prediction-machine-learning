
# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# Slides for slickR
model_slides <- list.files("www/model_slides", full.names = TRUE)

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
met_pars <- read.csv("data/met_params.csv", fileEncoding = "UTF-8-BOM")

# a table container with complex header
sketch1 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 2, 'Slope (m)'),
      th(colspan = 2, 'Intercept (b)')
    ),
    tr(
      lapply(rep(c('Mean', 'Std. Dev.'), 2), th)
    )
  )
))

sketch2 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 2, 'Slope (m)'),
      th(colspan = 2, 'Intercept (b)'),
      th(colspan = 2, '')
    ),
    tr(
      lapply(rep(c('Estimate', 'Std. Error'), 2), th),
      th("R-squared"),
      th("N")
    )
  )
))


shinyServer(function(input, output, session) {

  # Slickr model output
  output$slides <- renderSlickR({
    slickR(recap_slides) + settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 7000)
  })

  # Slickr Process UC slides
  output$proc_uc_slides <- renderSlickR({
    slickR(proc_uc_slides) + settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 7000)
  })

  # Slickr Parameter UC slides
  output$param_uc_slides <- renderSlickR({
    slickR(param_uc_slides) + settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 7000)
  })

  # Slickr Initial conditions UC slides
  output$ic_uc_slides <- renderSlickR({
    slickR(ic_uc_slides) + settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 7000)
  })

  # Slickr Driver UC slides
  output$driver_uc_slides <- renderSlickR({
    slickR(driver_uc_slides) + settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 7000)
  })

  # NEON Sites datatable ----
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  )

  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveValues(lab = NULL)

  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')


  # Select NEON DT rows ----
  neon_chla <- reactiveValues(df = NULL)
  airt1_fc <- reactiveValues(df = NULL)
  observeEvent(input$table01_rows_selected, {
    row_selected = neon_sites[input$table01_rows_selected, ]
    siteID$lab <- neon_sites$siteID[input$table01_rows_selected]
    coords <- st_coordinates(row_selected)
    colnames(coords) <- c("long", "lat")
    row_selected = cbind(row_selected, coords)
    proxy <- leafletProxy('neonmap')
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$uid),
                        lng=row_selected$long,
                        lat=row_selected$lat,
                        icon = my_icon)

    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        addMarkers(data = prev_row(),
                   layerId = as.character(prev_row()$uid))
    }
    # set new value to reactiveVal
    prev_row(row_selected)

    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID$lab, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.POSIXct(chla[, 1], tz = "UTC")
    }
    neon_chla$df <- chla

    # Load one airt fc
    fpath <- file.path("data", "NOAAGEFS_1hr", siteID$lab)
    fc_date <- list.files(fpath)[1]
    fpath2 <- file.path(fpath, fc_date[1], "00")
    fils <- list.files(fpath2, full.names = TRUE)
    fils <- fils[-c(grep("ens00", fils))]
    fid <- ncdf4::nc_open(file.path(fils[1]))
    vars <- fid$var # Extract variable names for selection
    fc_vars <- names(vars)[c(1)] # Extract air temp
    membs <- 1 #length(fils)
    ncdf4::nc_close(fid)

    out <- lapply(fc_date, function(dat) {
      idx <- which(fc_date == dat)

      fpath2 <- file.path(fpath, dat, "00")
      fils <- list.files(fpath2)
      fils <- fils[-c(grep("ens00", fils))]
      fils <- fils[1]

      for( i in seq_len(length(fils))) {

        fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID$lab, dat,
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
        cnam <- paste0("mem", formatC(i, width = 2, format = "d", flag = "0"))
        # if(i == 1) {
          df2 <- mlt1
          colnames(df2)[3] <- cnam
        df3 <- data.frame(Date = as.character(as.Date(df2$time)),
                          value = df2$mem01)
        # df2$Date <- as.character(as.Date(df2$time))
        df4 <- plyr::ddply(df3, c("Date"), function(x) data.frame(value = mean(x[, 2], na.rm = TRUE)))
        df4$Date <- as.Date(df4$Date)
        df4 <- df4[df4$Date <= "2020-10-02", ]

      }
      return(df4)
    })
    airt1_fc$df <- out[[1]]
  })

  # Neon map ----
  output$neonmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = neon_sites,
                 layerId = ~uid, clusterOptions = markerClusterOptions(),
                 label = ~locationDescription, icon = ~neonIcons[type])

  })

  # Download phenocam ----
  pheno_file <- reactiveValues(img = NULL)
  observeEvent(input$view_webcam, {

    req(!is.null(siteID$lab))
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Accessing and downloading phenocam image",
                 detail = "This may take a while. This window will disappear
                     when it is downloaded.", value = 0.5)

    p <- input$neonmap_marker_click
    idx <- which(neon_sites_df$siteID == siteID$lab)
    url <- neon_sites_df$pheno_url[idx]
    pheno_file$img <<- download_phenocam(url)
    progress$set(value = 1)
  })

  # Show phenocam image ----
  output$pheno <- renderImage({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table.")
    )
    validate(
      need(!is.null(pheno_file$img), "Click 'View latest photo' to download the image.")
    )
    list(src = pheno_file$img,
         alt = "Image failed to render. Please click 'Save plot' again.",
         height = 320,
         width = 350)
  }, deleteFile = FALSE)

  observeEvent(input$view_webcam, {
    output$prompt1 <- renderText({
      "Hover your cursor above the image to enlarge."
    })
  })

  # Download html ----
  observeEvent(input$table01_rows_selected, {
    p <- input$neonmap_marker_click  # typo was on this line
    sid <- neon_sites$siteID[input$table01_rows_selected]
    idx <- which(neon_sites_df$siteID == sid)
    # output$site_name <- neon_sites$description[idx]
    output$site_html <- renderUI({
      return(get_html(site_id = neon_sites_df$siteID[idx]))
    })
  })
  #** Create hyperlink ----
  observeEvent(input$table01_rows_selected, {
    sid <- neon_sites$siteID[input$table01_rows_selected]
    url <- paste0("https://www.neonscience.org/field-sites/field-sites-map/", sid)

    output$site_link <- renderUI({
      tags$a(href = url, "Click here for more site info", target = "_blank")
    })
  })
  #** Create prompt ----
  persist_df <- reactiveValues(df = NULL)
  observeEvent(input$table01_rows_selected, {
    output$prompt2 <- renderText({
      "Click on the link below to find out more information about your site."
    })

    ref <- "Air temperature"
    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", "neon", paste0(siteID$lab, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(ref, " is not available at this site."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp


    ref2 <- "Surface water temperature"
    y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
    y_file <- file.path("data", "neon", paste0(siteID$lab, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(ref2, " is not available at this site."))
    )
    yvar <- read.csv(y_file)
    yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
    yvar$Date <- as.Date(yvar[, 1])
    if(ref2 == "Surface water temperature") {
      yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
    }
    yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp

    df <- merge(xvar, yvar, by = "Date")

    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps.")
    )
    df$month <- lubridate::month(df$Date)
    df <- df[(df$month %in% 5:10), 1:3]
    colnames(df)[-1] <- c("airt", "wtemp")
    airt_swt$df <- df
    df$Mod <- c(NA, df$wtemp[-nrow(df)])
    persist_df$df <- df


    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp, airt = airt_swt$df$airt)

    wtemp_fc_data$hist <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
    wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date > as.Date("2020-09-25")] <- NA
    wtemp_fc_data$hist$airt[wtemp_fc_data$hist$Date > as.Date("2020-09-25")] <- NA
    wtemp_fc_data$fut <- dat[dat$Date > as.Date("2020-09-25") & dat$Date <= "2020-10-02", ]
    wtemp_fc_data$fut$airt[wtemp_fc_data$fut$Date > as.Date("2020-09-25")] <- NA


  })

  # Read in site data ----
  neon_DT <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )

    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", "neon", paste0(siteID$lab, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    df <- read.csv(file)
    df[, 1] <- as.POSIXct(df[, 1], tz = "UTC")
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var

    if(input$view_var == "Surface water temperature") {
      df <- df[df[, 2] == min(df[, 2], na.rm = TRUE), c(1, 3)] # subset to surface temperature
    }

    sel <- tryCatch(df[(selected$sel$pointNumber+1), , drop=FALSE] , error=function(e){NULL})


    return(list(data = df, sel = sel))
  })

  # NEON variable description table ----
  output$var_desc <- renderDT({
    var_desc <- neon_vars[!duplicated(neon_vars$Short_name), c("Short_name", "description")]
    colnames(var_desc) <- c("Name", "Description")
    datatable(var_desc, rownames = FALSE, options = list(pageLength = 4))
  })

  # Site data datatable ----
  output$neon_datatable <- DT::renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    df <- neon_DT()$data
    df[, -1] <- signif(df[, -1], 4)
    df[, 1] <- format(df[, 1], format = "%Y-%m-%d")
    names(df)[ncol(df)] <- read_var
    return(df)
  })

  # Variable description ----
  output$txt_out <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    out_txt <- neon_vars$description[which(neon_vars$Short_name == input$view_var)][1]
    return(out_txt)
  })

  # Site data plot ----
  output$var_plot <- renderPlotly({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", "neon", paste0(siteID$lab, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    obj <- neon_DT()$sel

    p <- ggplot() +
      geom_point(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2]), color = "black") +
      ylab(paste0(input$view_var, " (", units, ")")) +
      xlab("Time") +
      theme_bw(base_size = 12)

    if(!is.null(obj)) {
      p <- p +
        geom_point(data = obj, aes_string(names(obj)[1], names(obj)[2]), color = cols[2])

    }
    return(ggplotly(p, dynamicTicks = TRUE, source = "A"))
  })

  # Slickr model output
  output$slck_model <- renderSlickR({
    slickR(model_slides)
  })

  # Stats 101 ----
  # Load airt & SWT
  airt_swt <- reactiveValues(df = NULL, sub = NULL, sel = NULL)
  observeEvent(input$plot_airt_swt2, { # view_var

    req(!is.null(airt_swt$df))
    # Date slider
    # airt_swt$sub <- airt_swt$df[airt_swt$df$Date >= input$date1[1] & airt_swt$df$Date <= input$date1[2], ]

    if(input$samp_freq == "Month") {
      idx_dates <- seq.Date(airt_swt$df$Date[1], to = airt_swt$df$Date[nrow(airt_swt$df)], by = "1 month")
    } else if(input$samp_freq == "Week") {
      idx_dates <- seq.Date(airt_swt$df$Date[1], to = airt_swt$df$Date[nrow(airt_swt$df)], by = "1 week")
    } else if(input$samp_freq == "Fortnight") {
      idx_dates <- seq.Date(airt_swt$df$Date[1], to = airt_swt$df$Date[nrow(airt_swt$df)], by = "2 week")
    } else if(input$samp_freq == "Day") {
      idx_dates <- seq.Date(airt_swt$df$Date[1], to = airt_swt$df$Date[nrow(airt_swt$df)], by = "1 day")
    }

    airt_swt$sub <- airt_swt$df[airt_swt$df$Date %in% idx_dates, ]
  })

  observe({
    airt_swt$sel <- tryCatch(airt_swt$df[(selected$sel$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
  })

  # Plot air temperature vs. surface water temperature
  output$airt_swt_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$df),
           message = "Click 'Plot'")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )
    df <- airt_swt$df
    df$airt[is.na(df$wtemp)] <- NA
    df$wtemp[is.na(df$airt)] <- NA
    p <- ggplot() +
      geom_line(data = df, aes(Date, airt, color = "Air temperature")) +
      geom_line(data = df, aes(Date, wtemp, color = "Water temperature")) +
      scale_color_manual(values = cols[5:6]) +
    # geom_point(data = airt_swt$df, aes(airt, wtemp), color = "black") +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_bw(base_size = 18)
    return(p)
  })

  output$date_slider1 <- renderUI({
    req(!is.null(airt_swt$df))
    df <- na.exclude(airt_swt$df)
    sliderInput("date1", "Date", min = min(df$Date), max = max(df$Date), step = 1, value = c(min(df$Date), min(df$Date) + 100))
  })

  # Reset plot when adjusting dates
  # observeEvent(input$date1, {
  #   airt_swt$sub <- NULL
  #   lm_fit$fit <- NULL
  #   lm_fit$plt_orig <- TRUE
  #   lm_fit$eqn <- NULL
  # })

  # Reset plot when adjusting dates
  observeEvent(input$samp_freq, {
    airt_swt$sub <- NULL
    lm_fit$fit <- NULL
    lm_fit$plt_orig <- TRUE
    lm_fit$eqn <- NULL
  })

  # Add dashed lines to build reg plot
  reg_line <- reactiveValues(m = 1, b = 0)
  sav_lines <- reactiveValues(m = 1, b = 0)
  observeEvent(input$draw_line, {
    reg_line$m <- input$m
    reg_line$b <- input$b
  })

  # Data table to store 10 lines
  lr_pars <- reactiveValues(dt = data.frame(m_est = rep(NA, 4), m_se = rep(NA, 4),
                                            b_est = rep(NA, 4), b_se = rep(NA, 4),
                                            r2 = rep(NA, 4), N = rep(NA, 4), rmse = rep(NA, 4)))
  # output$lr_DT <- renderDT(lr_pars$dt, selection = "single",
  #                          options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
  #                                         columnDefs = list(list(width = '10%', targets = "_all"))
  #                          ), colnames = c("m (Est.)", "m (SE)", "b (Est.)", "b (SE)", "R-squared", "%"),
  #                          rownames = FALSE, # c("25%", "50%", "75%", "100%"),
  #                          # container = sketch2,
  #                          server = FALSE, escape = FALSE)
  lr_eqn <- reactiveValues(dt = data.frame(eqn = rep(NA, 4),
                                            r2 = rep(NA, 4), N = rep(NA, 4)))
  output$lr_DT <- renderDT(lr_eqn$dt[, c(1, 3)], selection = "none",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                           ), colnames = c("Model", "N"),
                           rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                           # container = sketch2,
                           server = FALSE, escape = FALSE)
  output$lr_DT2 <- renderDT(lr_pars$dt[, c("m_est", "m_se", "b_est", "b_se", "rmse", "N")], selection = "single",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                           ),
                           # colnames = c("m", "b", "R-squared", "N"),
                           colnames = c("m", "m (Std. Dev.)", "b", "b (Std. Dev.)", "RMSE", "N"),
                           rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                           # container = sketch2,
                           server = FALSE, escape = FALSE)

  output$r2_tab <- renderDT(data.frame(lr_pars$dt$rmse), selection = "none",
                               options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                              columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                               ), colnames = c("RMSE"),
                               rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                               # container = sketch2,
                               server = FALSE, escape = FALSE)

  # Add red saved lines to plot & DT
  # observeEvent(input$save_line, {
  #   req(!is.null(airt_swt$sub))
  #   req(!is.na(input$m))
  #   req(!is.na(input$b))
  #   if(input$save_line == 1) {
  #     sav_lines$m <- input$m
  #     sav_lines$b <- input$b
  #   } else {
  #     sav_lines$m <- c(sav_lines$m, input$m)
  #     sav_lines$b <- c(sav_lines$b, input$b)
  #   }
  #   mod <- (input$m * airt_swt$sub$airt) + input$b
  #   df <- data.frame(obs = airt_swt$sub$wtemp, mod = mod)
  #   df <- na.exclude(df)
  #   # mean_err <- round(mean(mod - airt_swt$sub$wtemp, na.rm = TRUE), 2)
  #   mean_err <- round(cor(df$obs, df$mod), 2)
  #   if(!is.null(input$lr_DT_rows_selected)) {
  #     lr_pars$dt$m[input$lr_DT_rows_selected] <- input$m
  #     lr_pars$dt$b[input$lr_DT_rows_selected] <- input$b
  #     lr_pars$dt$start[input$lr_DT_rows_selected] <- as.character(input$date1[1])
  #     lr_pars$dt$stop[input$lr_DT_rows_selected] <- as.character(input$date1[2])
  #     lr_pars$dt$mean_err[input$lr_DT_rows_selected] <- mean_err
  #     lr_pars$dt$label[input$lr_DT_rows_selected] <- nrow(na.exclude(airt_swt$sub))
  #   } else {
  #     idx <- which(is.na(lr_pars$dt$m))[1]
  #     lr_pars$dt$m[idx] <- input$m
  #     lr_pars$dt$b[idx] <- input$b
  #     lr_pars$dt$start[idx] <- as.character(input$date1[1])
  #     lr_pars$dt$stop[idx] <- as.character(input$date1[2])
  #     lr_pars$dt$mean_err[idx] <- mean_err
  #     lr_pars$dt$label[idx] <- nrow(na.exclude(airt_swt$sub))
  #   }
  # })

  # Plot with regression lines
  output$airt_swt_plot_lines <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    df <- na.exclude(airt_swt$sub)
    validate(
      need(nrow(df) > 0,
           message = "No points in selected dates. Please adjust the dates.")
    )

    mx <- max(df$airt, df$wtemp, na.rm = TRUE) + 2
    mn <- min(df$airt, df$wtemp, na.rm = TRUE) - 2

    sgmnt <- data.frame(x = mn, xend = mx, y = mn, yend = mx)

    p <- ggplot() +
      # geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_segment(data = sgmnt, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      coord_cartesian(xlim = c(-5, 30), ylim = c(-5, 30)) +
      theme_bw(base_size = 12)

    pars <- na.exclude(lr_pars$dt)

    if(length(lm_fit$df_lst) > 0) {
      # mlt <- reshape::melt(lm_fit$df_lst, id.vars = c("Date", "wtemp", "airt", "Percentage"))
      # mlt$Percentage <- factor(paste0(mlt$Percentage, "%"))
      # lvls <- levels(mlt$Percentage)
      # ordr <- order(as.numeric(gsub("%", "", as.character(lvls))))
      # # Reorder levels in ascending order
      # mlt$Percentage <- factor(mlt$Percentage, levels = levels(mlt$Percentage)[ordr])
      # mlt <- reshape::melt(lm_fit$df_lst, id.vars = c("Date", "Frequency", "N"))
      mlt <- do.call(rbind, lm_fit$df_lst)
      mlt$Frequency <- factor(mlt$Frequency, levels = samp_freq)
      # levels(mlt$Frequency) <- samp_freq

      p <- p +
        geom_point(data = mlt, aes(airt, wtemp, color = Frequency)) +
        geom_smooth(data = mlt, aes(airt, wtemp, color = Frequency), method = "lm", formula = "y ~ x",
                    se = FALSE) +
        scale_color_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4])) +
        labs(color = "Frequency")
    }

    if(lm_fit$plt_orig) {
      p <- p +
        geom_point(data = df, aes(airt, wtemp), color = "black")
    }

    gp <- ggplotly(p, dynamicTicks = TRUE) %>%
      layout(xaxis = list(range = c(0, 10)),
             yaxis = list(range = c(10, 15)))

    return(gp)
  })

  # Plot water temp ts with model
  output$lm_ts_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$df),
           message = "Click 'Plot'")
    )
    # validate(
    #   need(input$plot_airt_swt > 0,
    #        message = "Click 'Plot'")
    # )
    df <- airt_swt$df
    df$airt[is.na(df$wtemp)] <- NA
    df$wtemp[is.na(df$airt)] <- NA
    df <- df[df$Date > "2020-01-01", ]

    pars <- na.exclude(lr_pars$dt)
    freq_idx <- which(!is.na(lr_pars$dt[, 1]))

    if(nrow(pars) > 0) {
      mod <- lapply(1:nrow(pars), function(x) {
        df2 <- data.frame(Date = df$Date,
                   Model = pars$m_est[x] * df$airt + pars$b_est[x],
                   Frequency = samp_freq[freq_idx[x]])
        df2$rmse <- round(sqrt(mean((df2$Model - df$wtemp)^2, na.rm = TRUE)), 2)
        return(df2)
      })
      # names(mod) <- pars$label
      # mlt <- reshape::melt(mod, id.vars = c("Date", "Model", "Percentage"))
      mlt <- do.call(rbind, mod)
      # mlt <- mlt[mlt$Date > "2020-01-01", ]
      mlt$Frequency <- factor(mlt$Frequency, levels = samp_freq)

      for(freq in samp_freq) {
        sub <- mlt[mlt$Frequency == freq, ]
        fidx <- which(samp_freq == freq)
        if(nrow(sub) > 0) {
          lr_pars$dt$rmse[fidx] <- sub$rmse[1]
          if(freq == "Daily") {
            mod_selec_tab$dt$rmse[3] <- sub$rmse[1]
          }
        }
      }

      # mlt$Percentage <- factor(paste0(mlt$Percentage, "%"))
      # mlt$Percentage <- factor(as.character(mlt$N))
      # lvls <- levels(mlt$Percentage)
      # ordr <- order(as.numeric(gsub("%", "", as.character(lvls))))
      # # Reorder levels in ascending order
      # mlt$Percentage <- factor(mlt$Percentage, levels = levels(mlt$Percentage)[ordr])
    }

    p <- ggplot() +
      # geom_hline(yintercept = 0) +
      # geom_point(data = df, aes(Date, wtemp, color = "Observed")) +
      geom_point(data = df, aes(Date, wtemp), color = "black") +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_bw(base_size = 12)

    if(nrow(pars) > 0) {
      p <- p +
        # geom_line(data = mlt, aes(Date, Model, color = Percentage)) +
        geom_line(data = mlt, aes(Date, Model, color = Frequency)) +
        scale_color_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4])) +
        labs(color = "Frequency")
    }

    # return(p)
    return(ggplotly(p, dynamicTicks = TRUE))
  })

  # Add lm fit
  lm_fit <- reactiveValues(fit = NULL, df_lst = list(), plt_orig = TRUE, eqn = NULL)
  observeEvent(input$add_lm, {
    req(!is.null(airt_swt$sub))
    tot_rows <- nrow(na.exclude(airt_swt$df))
    df <- airt_swt$sub
    df <- na.exclude(df)
    colnames(df)[2:3] <- c("airt", "wtemp")
    fit <- lm(wtemp ~ airt, data = df)
    out <- summary(fit)

    # if(!is.null(input$lr_DT_rows_selected)) {
    #   lr_pars$dt$m_est[input$lr_DT_rows_selected] <- round(out$coefficients[2, 1], 2)
    #   lr_pars$dt$b_est[input$lr_DT_rows_selected] <- round(out$coefficients[1, 1], 2)
    #   lr_pars$dt$m_se[input$lr_DT_rows_selected] <- round(out$coefficients[2, 2], 2)
    #   lr_pars$dt$b_se[input$lr_DT_rows_selected] <- round(out$coefficients[1, 2], 2)
    #   lr_pars$dt$r2[input$lr_DT_rows_selected] <- round(out$r.squared, 2)
    #   lr_pars$dt$Percentage[input$lr_DT_rows_selected] <- round((100 * nrow(df) / tot_rows))
    #
    #   lr_eqn$dt$eqn[input$lr_DT_rows_selected] <- paste0("$$wtemp_{t} =  ", round(out$coefficients[2, 1], 2), "\\times airt_{t} + ", round(out$coefficients[1, 1], 2), " $$")
    #   lr_eqn$dt$r2[input$lr_DT_rows_selected] <- round(out$r.squared, 2)
    #   lr_eqn$dt$Percentage[input$lr_DT_rows_selected] <- round((100 * nrow(df) / tot_rows))
    # } else {
    # Old version with ability to re-fit models
      # idx <- which(is.na(lr_pars$dt$m_est))[1]
      idx <- which(samp_freq2 %in% input$samp_freq)
      lr_pars$dt$m_est[idx] <- round(out$coefficients[2, 1], 2)
      lr_pars$dt$b_est[idx] <- round(out$coefficients[1, 1], 1)
      lr_pars$dt$m_se[idx] <- round(out$coefficients[2, 2], 2)
      lr_pars$dt$b_se[idx] <- round(out$coefficients[1, 2], 1)
      lr_pars$dt$r2[idx] <- round(out$r.squared, 2)
      # lr_pars$dt$Percentage[idx] <- round((100 * nrow(df) / tot_rows))
      lr_pars$dt$N[idx] <- nrow(df)
      lr_pars$dt$rmse[idx] <- 0

      lr_eqn$dt$eqn[idx] <- paste0("$$wtemp_{t+1} =  ", round(out$coefficients[2, 1], 2), "\\times atemp_{t} + ", round(out$coefficients[1, 1], 2), " $$")
      lr_eqn$dt$r2[idx] <- round(out$r.squared, 2)
      # lr_eqn$dt$Percentage[idx] <- round((100 * nrow(df) / tot_rows))
      lr_eqn$dt$N[idx] <- nrow(df)
      # }

      df$N <- nrow(df)
      df$Frequency <- samp_freq[idx]

      lm_fit$df_lst[[idx]] <- df

    # df$Percentage <- round((100 * nrow(df) / tot_rows))
    # if(!is.null(input$lr_DT_rows_selected)) {
    #   lm_fit$df_lst[[input$lr_DT_rows_selected]] <- df
    # } else {
    #   idx <- length(lm_fit$df_lst) + 1
    #   lm_fit$df_lst[[idx]] <- df
    # }

    lm_fit$plt_orig <- FALSE

    lm_fit$fit <- fit

    lm_fit$eqn <- paste0("$$wtemp_{t} =  ", round(out$coefficients[2, 1], 2), "\\times atemp_{t} + ", round(out$coefficients[1, 1], 2), " $$")

    # For model selection table
    if(input$samp_freq == "Day") {
      mod_selec_tab$dt$eqn[3] <- paste0("$$wtemp_{t+1} =  ", round(out$coefficients[2, 1], 2), " \\times atemp_{t+1} + ", round(out$coefficients[1, 1], 2), " $$")
      mod_selec_tab$dt$r2[3] <- round(out$r.squared, 2)
    }


  })

  # observe({
  #   req(!is.null(airt_swt$sub))
  #   fit <- lm(wtemp ~ airt, data = df)
  #   out <- summary(fit)
  #   mod_selec_tab$dt$eqn[1] <- paste0("$$wtemp_{t} =  ", round(out$coefficients[2, 1], 2), "*atemp_{t} + ", round(out$coefficients[1, 1], 2), " $$")
  #   mod_selec_tab$dt$r2 <- round(out$r.squared, 2)
  # })

  output$lm_mod <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    validate(
      need(!is.null(lm_fit$eqn),
           message = "Click 'Get model parameters'.")
    )
    withMathJax(
      tags$p(lm_fit$eqn)
    )
  })


  output$lm_out <- renderPrint({
    validate(
      need(!is.null(lm_fit$fit), "Click 'Get model parameters'")
    )
    summary(lm_fit$fit)
  })

  # Calculate statistics from the lines drawn
  linr_stats <- reactiveValues(dt = data.frame("m_mn" = 0,
                                               "m_sd" = 0,
                                               "b_mn" = 0,
                                               "b_sd" = 0))

  # Disable button if no row selected
  observe({
    if(!is.null(input$lr_DT2_rows_selected)) {
      shinyjs::enable("calc_stats")
    } else {
      shinyjs::disable("calc_stats")
    }
  })

  observeEvent(input$lr_DT2_rows_selected, {
    req(sum(!is.na(lr_pars$dt$m_est)) > 0)
    req(!is.na(lr_pars$dt$m_est[input$lr_DT2_rows_selected]))

    mb_samples$df <- NULL

    df <- data.frame("m_mn" = lr_pars$dt$m_est[input$lr_DT2_rows_selected],
                     "m_sd" = lr_pars$dt$m_se[input$lr_DT2_rows_selected],
                     "b_mn" = lr_pars$dt$b_est[input$lr_DT2_rows_selected],
                     "b_sd" = lr_pars$dt$b_se[input$lr_DT2_rows_selected])
    updateSliderInput(session, "m_std", value = df[1, 2], max = round(max(1, (df[1, 2] + 0.5)), 2))
    updateSliderInput(session, "b_std", value = df[1, 4], max = round(max(1, (df[1, 4] + 0.5)), 2))
    linr_stats$dt <- signif(df, 3)
  })

  output$lr_stats <- renderDT(linr_stats$dt, selection = "none",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                              ),
                              colnames = c("Mean (m)", "Std. Dev (m)", "Mean (b)", "Std. Dev (b)"),
                              rownames = FALSE, container = sketch1,
                              server = FALSE, escape = FALSE)

  #** Generate distribution plots ----
  lr_dist_plot <- reactiveValues(lst = as.list(rep(NA, 5)))
  observeEvent(input$gen_lr_dist_plot, {

    req(!is.nan(input$m_std))

    df <- data.frame(m = rnorm(500, mean = linr_stats$dt[1, 1], sd = input$m_std),
                     b = rnorm(500, mean = linr_stats$dt[1, 3], sd = input$b_std))
    if(!is.null(input$lr_DT2_rows_selected)) {
      if(input$lr_DT2_rows_selected != "") {
        df$Frequency <- samp_freq[input$lr_DT2_rows_selected]
        lr_dist_plot$lst[[input$lr_DT2_rows_selected]] <- df
      }
    } else {
      df$Frequency <- "User input"
      lr_dist_plot$lst[[5]] <- df
    }
    linr_stats$dt$b_sd <- input$b_std
    linr_stats$dt$m_sd <- input$m_std
  })

  # Reset samples
  observeEvent(input$b_std, {
    mb_samples$df <- NULL
  })
  observeEvent(input$m_std, {
    mb_samples$df <- NULL
  })
  observeEvent(input$n_samp, {
    mb_samples$df <- NULL
  })

  output$lr_param_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$gen_lr_dist_plot > 0, "Click 'Generate plot!'")
    )
    validate(
      need(any(!is.na(lr_dist_plot$lst)), "Click 'Generate plot!'")
    )

    lst <- lr_dist_plot$lst[!is.na(lr_dist_plot$lst)]

    mlt <- do.call(rbind, lst)

    y_max_m <- lapply(lst, function(x) {
      dens_m <- density(x$m)
      max(dens_m$y)
    })

    ylims_m <- c(0, max(c(6, max(unlist(y_max_m), na.rm = TRUE))))

    y_max_b <- lapply(lst, function(x) {
      dens_b <- density(x$b)
      max(dens_b$y)
    })

    ylims_b <- c(0, max(c(1.5, max(unlist(y_max_b), na.rm = TRUE))))

    # df <- data.frame(par = "Slope (m)", value = lr_dist_plot$m)

    xlims_m <- c(min(0, mlt$m), max(2, mlt$m))
    xlims_b <- c(min(-2.5, mlt$b), max(10, mlt$b))

    # scales_y <- list()

    p1 <- ggplot() +
      geom_density(data = mlt, aes(m, fill = Frequency), color = NA, alpha = 0.3) +
      coord_cartesian(xlim = xlims_m, ylim = ylims_m) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Slope (m)") +
      scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4], "User input" = cols[5])) +
      theme_bw(base_size = 22)

    p2 <- ggplot() +
      geom_density(data = mlt, aes(b, fill = Frequency), color = NA, alpha = 0.3) +
      coord_cartesian(xlim = xlims_b, ylim = ylims_b) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Intercept (b)") +
      scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4], "User input" = cols[5])) +
      theme_bw(base_size = 22)

    g <- ggpubr::ggarrange(p1, p2, nrow = 1, align = "h", common.legend = TRUE, legend = "bottom")
    return(g)
  })

  # Sample m and b for plotting
  mb_samples <- reactiveValues(df = NULL)
  lm_dist <- reactiveValues(df = NULL)


  observeEvent(input$gen_lin_mods, {
    req(!is.null(input$n_samp))
    req(!is.na(linr_stats$dt$b_sd))
    # req(!is.null(input$lr_DT2_rows_selected))
    # req(input$lr_DT2_rows_selected != "")
    # req(!is.null(lr_dist_plot$m))
    # req(!is.null(lr_dist_plot$b))
    updateRadioButtons(inputId = "plot_type1", selected = "Line")

    if(!is.null(input$lr_DT2_rows_selected)) {
      if(input$lr_DT2_rows_selected != "") {
        idx <- sample(1:nrow(lr_dist_plot$lst[[input$lr_DT2_rows_selected]]), input$n_samp)
        mb_samples$df <- signif(data.frame("m" = lr_dist_plot$lst[[input$lr_DT2_rows_selected]]$m[idx],
                                           "b" = lr_dist_plot$lst[[input$lr_DT2_rows_selected]]$b[idx]), 3)
      }
    } else {
      idx <- sample(1:nrow(lr_dist_plot$lst[[5]]), input$n_samp)
      mb_samples$df <- signif(data.frame("m" = lr_dist_plot$lst[[5]]$m[idx],
                                         "b" = lr_dist_plot$lst[[5]]$b[idx]), 3)
    }
    # Create summary data frame
    x = c(-5, 35) # seq(-5, 35, 0.1)
    y = apply(mb_samples$df, 1, function(y) y[1]* x + y[2])

    lm_dist$df <- data.frame(x = x,
                             p5 = apply(y, 1, function(x) quantile(x, 0.05)),
                             p125 = apply(y, 1, function(x) quantile(x, 0.125)),
                             p875 = apply(y, 1, function(x) quantile(x, 0.875)),
                             p95 = apply(y, 1, function(x) quantile(x, 0.95)),
                             mean = apply(y, 1, function(x) mean(x)),
                             median = apply(y, 1, function(x) median(x)))
  })

  output$mb_samps <- renderDT(mb_samples$df, selection = "none",
                              options = list(searching = FALSE, paging = TRUE, ordering= FALSE,
                                             # dom = "t",
                                             autoWidth = TRUE,
                                             pageLength = 5
                              ),
                              colnames = c("Slope (m)", "Intercept (b)"),
                              rownames = FALSE,
                              server = FALSE, escape = FALSE)



  # Add linear models to plot ----
  output$add_lin_mods <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )

    p <- ggplot() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(data = airt_swt$df, aes(airt, wtemp), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      coord_cartesian(xlim = c(-5, 30), ylim = c(-5, 30)) +
      theme_bw(base_size = 16)

    if(!is.null(mb_samples$df) & input$plot_type1 == "Line") {
      p <- p +
        geom_abline(slope = mb_samples$df$m, intercept = mb_samples$df$b, color = "gray", linetype = "solid")
    }
    if(input$plot_type1 == "Distribution") {
      p <- p +
        geom_ribbon(data = lm_dist$df, aes(x, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3) +
        # geom_ribbon(data = lm_dist$df, aes(x, ymin = p125, ymax = p875, fill = "75%"), alpha = 0.3) +
        geom_line(data = lm_dist$df, aes(x, median, color = "Median")) +
        scale_fill_manual(values = l.cols)
    }
    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  #** Investigate model error ----
  output$mod_err_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_dist$df), "Click 'Add lines' in the plot above")
    )



    p <- ggplot(data = airt_swt$df, aes(airt, wtemp)) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      coord_cartesian(xlim = c(-5, 35), ylim = c(-5, 35)) +
      theme_bw(base_size = 16)

    p <- p +
      geom_line(data = lm_dist$df, aes(x, mean, color = "Linear regression")) +
      scale_color_manual(values = cols[2])

    return(p)
  })

  click_df <- reactiveValues(df = NULL)
  observe({
    req(!is.null(lm_dist$df))
    pnts <- nearPoints(airt_swt$df, input$mod_err_plot_click)[, -1]
    # class(pnts)
    brsh <- brushedPoints(airt_swt$df, brush = input$mod_err_plot_brush)[, -1]
    df <- rbind(pnts, brsh)
    df <- round(df, 1)
    # idx <- which(lm_dist$df$x %in% df$airt)
    df$`Modelled Water Temp.` <- linr_stats$dt[1, 1] * df$airt + linr_stats$dt[1, 3]
    colnames(df)[1:2] <- c("Air Temp.", "Observed Water Temp.")
    df$Error <- df[, 3] - df[, 2]
    click_df$df <- df
    mean_err$val <- NULL
  })


  output$click_dt <- renderDT(click_df$df, selection = "none",
                            colnames = c("Air Temp.", "Obs. Water Temp.", "Mod. Water Temp.", "Mod - Obs"),
                            options = list(pageLength = 5),
                              rownames = FALSE,
                              server = FALSE, escape = FALSE)

  #** Calculate mean error ----
  mean_err <- reactiveValues(val = NULL)
  observeEvent(input$calc_err, {
    req(!is.null(click_df$df))
    mean_err$val <- mean(click_df$df$Error, na.rm = TRUE)
  })

  output$mean_err <- renderText({
    validate(
      need(!is.null(click_df$df), "Please select points on the plot.")
    )
    validate(
      need(nrow(click_df$df) > 0, "No points in the current selection. Please try again.")
    )
    validate(
      need(!is.null(mean_err$val), "Click calculate.")
    )

    paste0("The mean model error is ", round(mean_err$val, 1), " \u00B0C.")
  })

  #** Calculate error w/ UC ----
  output$mod_err_uc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_dist$df), "Click 'Add lines' above")
    )

    p <- ggplot() +
      geom_ribbon(data = lm_dist$df, aes(x, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3) +
      # geom_ribbon(data = lm_dist$df, aes(x, ymin = p125, ymax = p875, fill = "75%"), alpha = 0.3) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(data = airt_swt$df, aes(airt, wtemp), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      # coord_cartesian(xlim = c(-5, 35), ylim = c(-5, 35)) +
      theme_bw(base_size = 16)

    p <- p +
      geom_line(data = lm_dist$df, aes(x, mean, color = "Linear regression")) +
      scale_color_manual(values = cols[7]) +
      scale_fill_manual(values = l.cols)
    if(nrow(airt_swt$sel) != 0) {
      p <- p +
        geom_point(data = airt_swt$sel, aes(airt, wtemp), color = cols[2])
    }

    gp <- ggplotly(p, dynamicTicks = TRUE, source = "ci_sel")
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  selected <- reactiveValues(sel = NULL)

  observeEvent(input$clear_sel1, {
    airt_swt$sel <- NULL
    selected$sel <- NULL
  })

  #selected
  observe({
    # suppress warnings
    storeWarn<- getOption("warn")
    options(warn = -1)
    selected$sel <- event_data(event = "plotly_selected", source = "ci_sel")

    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({
      options(warn = storeWarn)
    }) ,ms = 100)
  })

  output$sel_points <- renderText({
    n <- 0
    if(!is.null(airt_swt$sel)) {
      n <- nrow(airt_swt$sel)
    }
    paste0("Number of selected points: ", n)
  })

  output$total_points <- renderText({
    validate(
      need(!is.null(airt_swt$df), "Need SOMETHING?")
    )
    n_pnts = nrow(na.exclude(airt_swt$df))
    paste0("Total number of points: ", n_pnts)
  })

  pct_msg <- reactiveValues(txt = NULL)
  observeEvent(input$calc_pct, {
    n_pnts = nrow(na.exclude(airt_swt$df))
    insid <- n_pnts - (input$points_above + input$points_below)
    pct_msg$txt <- paste0("Percentage points inside the CI: ", round((insid * 100 / n_pnts), 2), "%")
  })

  output$pct_inside <- renderText({
    validate(
      need(!is.null(pct_msg$txt), "Click calculate")
    )
    pct_msg$txt
  })

  #* Persistence model ----
  # output$date_persist <- renderUI({
  #   validate(
  #     need(!is.null(persist_df$df), "Please select a site in Objective 1.")
  #   )
  #   df <- persist_df$df[!is.na(persist_df$df$wtemp), ]
  #   # df <- persist_df$df
  #   sliderInput("persist_date", label = "Date", min = min(df$Date), max = max(df$Date),value = c(df$Date[1], df$Date[3]), step = 1)
  # })

  output$persist_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )

    df <- persist_df$df
    df <- df[df$Date > "2020-01-01", ]
    # df <- df[df$Date >= input$persist_date[1] & df$Date <= input$persist_date[2], ]
    # validate(
    #   need(nrow(df) > 0, "No data between those dates. Adjust the Date range.")
    # )

    p <- ggplot() +
      geom_point(data = df, aes(Date, wtemp, color = "Obs")) +
      ylab("Water temperature (\u00B0C)") +
      xlab("Time") +
      scale_color_manual(values = c("Mod" = cols[1], "Obs" = "black")) +
      theme_bw(base_size = 16)

    if(input$plot_persist > 0) {
      p <- p +
        geom_line(data = df, aes(Date, Mod, color = "Mod"))
    }

    ggplotly(p, dynamicTick = TRUE)
  })

  output$persist_r2 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_persist > 0, "Click 'Plot'.")
    )

    df <- persist_df$df
    # df <- df[df$Date >= input$persist_date[1] & df$Date <= input$persist_date[2], ]
    df <- na.exclude(df)
    # validate(
    #   need(nrow(df) > 0, "No data between those dates. Adjust the Date range.")
    # )

    fit <- lm(wtemp ~ Mod, data = df)
    out <- summary(fit)

    r2 <- round(out$r.squared, 2) # round(cor(df$wtemp, df$Mod), 2)
    rmse <- round(sqrt(mean((df$Mod - df$wtemp)^2, na.rm = TRUE)), 2)
    mod_selec_tab$dt$eqn[1] <- "$$wtemp_{t+1} = wtemp_{t}$$"
    mod_selec_tab$dt$r2[1] <- r2
    mod_selec_tab$dt$rmse[1] <- rmse
    withMathJax(
      tags$p(paste0("$$RMSE = ", rmse, "\u00B0C$$"))
    )
  })

  # Build a forecast model ----

  # observe({
  #   if((all(input$mult_lin_reg_vars %in% c("Water temperature days ahead", "Mean water temperature")) & input$mean_t == 1 & input$lag_t == 1 & length(input$mult_lin_reg_vars) == 2)) {
  #     shinyjs::disable("fit_mlr")
  #   } else if((all(input$mult_lin_reg_vars %in% c("Air temperature days ahead", "Mean air temperature")) & input$mean_t == 1 & input$lag_t == 1 & length(input$mult_lin_reg_vars) == 2)) {
  #     shinyjs::disable("fit_mlr")
  #   } else if(length(input$mult_lin_reg_vars) == 0) {
  #     shinyjs::disable("fit_mlr")
  #   } else {
  #     shinyjs::enable("fit_mlr")
  #   }
  # })

  output$mult_lin_reg_eqn <- renderUI({
    validate(
      need(!is.null(input$mult_lin_reg_vars),
           message = "Please select predictors.")
    )
    # validate(
    #   need(!(all(input$mult_lin_reg_vars %in% c("Water temperature days ahead", "Mean water temperature")) & input$mean_t == 1 & input$lag_t == 1 & length(input$mult_lin_reg_vars) == 2),
    #        message = "Water temperature mean of 1 and days ahead of 1 are the same and can not be included in the model.\nAdd another predictor or adjust the days ahead/mean")
    # )
    # validate(
    #   need(!(all(input$mult_lin_reg_vars %in% c("Air temperature days ahead", "Mean air temperature")) & input$mean_t == 1 & input$lag_t == 1 & length(input$mult_lin_reg_vars) == 2),
    #        message = "Air temperature mean of 1 and days ahead of 1 are the same and can not be included in the model.\nAdd another predictor or adjust the days ahead/mean")
    # )
    mean_terms <- NULL
    lag_terms <- NULL

    if(any(grepl("tomorrow", input$mult_lin_reg_vars))) {
      idx <- grep("tomorrow", input$mult_lin_reg_vars)
      idx2 <- which(lin_reg_vars$Name %in% input$mult_lin_reg_vars[idx])
      lag_terms <- paste0("\\beta_{%s} \\times  ", lin_reg_vars$latex[idx2], collapse = "\\\\ \\ &+ ")
    }
    if(any(grepl("today", input$mult_lin_reg_vars))) {
      idx <- grep("today", input$mult_lin_reg_vars)
      idx2 <- which(lin_reg_vars$Name %in% input$mult_lin_reg_vars[idx])
      mean_terms <- paste0("\\beta_{%s} \\times  ", lin_reg_vars$latex[idx2], collapse = "\\\\ \\ &+ ")
    }

    formula <- paste0("$$\\begin{align} \\ wtemp_{t+1} &=  ", paste0(c(lag_terms, mean_terms), collapse = "\\\\ \\ &+ "), "+ \\beta_{%s} \\end{align} $$")

    text <- do.call(sprintf, as.list(c(formula, 1:(length(input$mult_lin_reg_vars)+1))))

    withMathJax(
      tags$p(text)
    )
  })

  mlr_fit <- reactiveValues(lst = as.list(rep(NA, 2)))
  mlr_pred <- reactiveValues(lst = as.list(rep(NA, 2)))

  mlr_params <- reactiveValues(df = data.frame(beta1 = rep(NA,2),
                                               beta2 = rep(NA,2),
                                               beta3 = rep(NA,2),
                                               beta1_se = rep(NA,2),
                                               beta2_se = rep(NA,2),
                                               beta3_se = rep(NA,2)))

  observeEvent(input$fit_mlr, {
    req(!is.null(input$mult_lin_reg_vars))

    if(length(input$mult_lin_reg_vars) == 2) {
      inp_row <- 2
    } else if(input$mult_lin_reg_vars == "Water temperature (today)") {
      inp_row <- 1
    } else {
      showModal(modalDialog(
        title = "Uh oh!",
        "You must select predictors according to the instructions to the left.", easyClose = TRUE
      ))
      req(input$mult_lin_reg_vars != "Air temperature (tomorrow)")
    }

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)
    dat$wtemp_yday[-c(1:1)] <- dat$wtemp[-c(nrow(dat))]
    dat$airt_yday[-c(1:1)] <- dat$airt[-c(nrow(dat))]

    idx <- which(lin_reg_vars$Name %in% input$mult_lin_reg_vars)

    dat <- dat[, c("Date", "wtemp", lin_reg_vars$var[idx])]

    # Subset to training data
    # train <- dat[dat$Date >= input$train_date[1] & dat$Date <= input$train_date[2], ]

    eval(parse(text = paste0("fit <- lm(wtemp ~ ", paste0(lin_reg_vars$var[idx], collapse = " + "), ", data = dat)")))

    mlr_out$txt <- summary(fit)
    out <- summary(fit)
    if(nrow(out$coefficients) == 2) {
      mlr_params$df$beta1[inp_row] <- out$coefficients[2, 1]
      mlr_params$df$beta1_se[inp_row] <- out$coefficients[2, 2]
      mlr_params$df$beta2[inp_row] <- out$coefficients[1, 1]
      mlr_params$df$beta2_se[inp_row] <- out$coefficients[1, 2]
    } else if(nrow(out$coefficients) == 3) {
      mlr_params$df$beta1[inp_row] <- out$coefficients[2, 1]
      mlr_params$df$beta1_se[inp_row] <- out$coefficients[2, 2]
      mlr_params$df$beta2[inp_row] <- out$coefficients[3, 1]
      mlr_params$df$beta2_se[inp_row] <- out$coefficients[3, 2]
      mlr_params$df$beta3[inp_row] <- out$coefficients[1, 1]
      mlr_params$df$beta3_se[inp_row] <- out$coefficients[1, 2]
    }

    coeffs <- round(fit$coefficients, 2)
    if(coeffs[1] >= 0) {
      b <- paste0("+", coeffs[1])
    } else {
      b <- coeffs[1]
    }
    # coeffs <- round(coeffs[c(2:(length(coeffs)), 1)], 2)
    mod <- predict(fit, dat)
    df2 <- data.frame(obs = dat$wtemp, mod = mod, diff = mod - dat$wtemp)
    df2 <- na.exclude(df2)
    r2 <- round(mlr_out$txt$r.squared, 2) #round(cor(df2$obs, df2$mod), 2)
    err <- mean(mod - dat$wtemp, na.rm = TRUE)
    rmse <- round(sqrt(mean((mod - dat$wtemp)^2, na.rm = TRUE)), 2)

    # Latex eqn
    mean_terms <- NULL
    lag_terms <- NULL


    if(any(grepl("tomorrow", input$mult_lin_reg_vars))) {
      idx <- grep("tomorrow", input$mult_lin_reg_vars)
      idx2 <- which(lin_reg_vars$Name %in% input$mult_lin_reg_vars[idx])
      lag_terms <- paste0("%s \\times  ", lin_reg_vars$latex[idx2], collapse = "+ ")
    }
    if(any(grepl("today", input$mult_lin_reg_vars))) {
      idx <- grep("today", input$mult_lin_reg_vars)
      idx2 <- which(lin_reg_vars$Name %in% input$mult_lin_reg_vars[idx])
      mean_terms <- paste0("%s \\times  ", lin_reg_vars$latex[idx2], collapse = "+ ")
    }

    formula <- paste0("$$wtemp_{t+1} =  ", paste0(c(lag_terms, mean_terms), collapse = "+ "), b, " $$")

    text <- do.call(sprintf, as.list(c(formula, coeffs[-1])))

    # idx <- which(is.na(lr_pars$dt$m))[1]
    mlr$dt$Equation[inp_row] <- text # paste0("$$ wtemp =  ", paste0(coeffs[-1], " \\times  ", lin_reg_vars$latex[idx], collapse = " + "), b, " $$")
    mlr$dt$lag[inp_row] <- 1
    # mlr$dt$mean_day[inp_row] <- input$mean_t
    mlr$dt$mean_err[inp_row] <- rmse
    mlr_pred$lst[[inp_row]] <- data.frame(Date = dat$Date,
                                          Model = mod)

    if(inp_row == 1) {
      mod_selec_tab$dt$eqn[2] <- text
      mod_selec_tab$dt$r2[2] <- r2
      mod_selec_tab$dt$rmse[2] <- rmse
      mod_selec_tab$dt$lag[2] <- 1
    }
    if(inp_row == 2) {
      mod_selec_tab$dt$eqn[4] <- text
      mod_selec_tab$dt$r2[4] <- r2
      mod_selec_tab$dt$rmse[4] <- rmse
      mod_selec_tab$dt$lag[4] <- 1
    }

    mlr$eqn <- text
  })

  mlr_out <- reactiveValues(txt = NULL, invis = NULL)

  output$mlr_out <- renderPrint({
    validate(
      need(!is.null(input$mult_lin_reg_vars),
           message = "Please select predictors.")
    )
    validate(
      need(!is.null(mlr_out$txt), "Click 'Fit model'")
    )
    mlr_out$txt
  })

  output$mlr_invis <- renderPrint({
    validate(
      need(!is.null(mlr_out$invis), "Click 'Fit model'")
    )
    mlr_out$invis
  })

  output$mlr_mod <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$mult_lin_reg_vars),
           message = "Please select predictors.")
    )
    validate(
      need(!is.null(mlr$eqn),
           message = "Click 'Fit model'.")
    )
    withMathJax(
      tags$p(mlr$eqn)
    )
  })

  output$sel_mod <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab_rows_selected != "",
           message = "Select a model in the table.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[input$mod_selec_tab_rows_selected])
    )
  })

  output$sel_mod1a <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab1a_rows_selected != "",
           message = "")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[input$mod_selec_tab1a_rows_selected])
    )
  })

  output$sel_mod2 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab2_rows_selected != "",
           message = "")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[input$mod_selec_tab2_rows_selected])
    )
  })

  # Parameter UC
  output$sel_mod3b <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab3_rows_selected != "",
           message = "")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[input$mod_selec_tab3_rows_selected])
    )
  })


  # IC Forecast
  output$sel_mod4 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab4_rows_selected != "",
           message = "")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[input$mod_selec_tab4_rows_selected])
    )
  })

  # Driver forecast UC
  output$sel_mod5 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab5_rows_selected != "",
           message = "")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[input$mod_selec_tab5_rows_selected])
    )
  })

  observeEvent(input$mult_lin_reg_vars, {
    mlr_out$txt <- NULL
    mlr$eqn <- NULL
  })

  # observeEvent(input$train_date, {
  #   mlr$eqn <- NULL
  # })
  # observeEvent(input$test_date, {
  #   mlr$eqn <- NULL
  # })



  mlr <- reactiveValues(dt = data.frame(Equation = rep(NA, 2),
                                        lag = rep(NA, 2),
                                        # mean_day = rep(NA, 5),
                                        mean_err = rep(NA, 2)),
                        eqn = NULL
                        )

  output$mlr_dt <- renderDT(mlr$dt[, c(1, 3)], selection = "none",
                            options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                           columnDefs = list(list(width = '10%', targets = "_all")),
                                           scrollX = TRUE #,
    #                                        drawcallback = JS("function( settings ) {
    #     // MathJax.Hub.Config({
    #     //    tex2jax: {inlineMath: [['$','$'],['\\(','\\)']]}
    #     // });
    #
    #     MathJax.Hub.Queue(['Typeset',MathJax.Hub]);
    # }")
                            ), colnames = c("Model", "RMSE (\u00B0C)"), rownames = mod_names[c(2, 4)],

                            server = FALSE, escape = FALSE)

  # Date slider for training & test data
  # output$date_train <- renderUI({
  #   req(!is.null(airt_swt$df))
  #   bump <- round(nrow(airt_swt$df)/2)
  #   sliderInput("train_date", "Training Dates", min = min(airt_swt$df$Date), max = max(airt_swt$df$Date), step = 1, value = c(min(airt_swt$df$Date), airt_swt$df$Date[bump]))
  # })
  #
  # output$date_test <- renderUI({
  #   req(!is.null(airt_swt$df))
  #   bump <- round(nrow(airt_swt$df)/2)
  #   sliderInput("test_date", "Testing Dates", min = min(airt_swt$df$Date), max = max(airt_swt$df$Date), step = 1, value = c(airt_swt$df$Date[bump+1], max(airt_swt$df$Date)))
  # })

  #** Plot water temp ts with MLR model----
  output$mlr_ts_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$df),
           message = "Click 'Plot'")
    )

    df <- airt_swt$df
    df$airt[is.na(df$wtemp)] <- NA
    df$wtemp[is.na(df$airt)] <- NA

    df <- df[df$Date > "2020-01-01", ]



    p <- ggplot() +
      geom_point(data = df, aes(Date, wtemp), color = "black") +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      # facet_wrap(~per, scales = "free_x") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      theme_bw(base_size = 12)

    sub_lst <- mlr_pred$lst[!is.na(mlr_pred$lst)]

    if(length(sub_lst) > 0) {

      # names(sub_lst) <- 1:5
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      mlt$Label <- as.character(mlt$Label)

      mlt$Label[mlt$Label == 1] <- "Wtemp"
      mlt$Label[mlt$Label == 2] <- "Both"

      mlt$Label <- factor(mlt$Label, levels = c("Wtemp", "Both"))

      mlt <- mlt[mlt$Date > "2020-01-01", ]

      p <- p +
        geom_line(data = mlt, aes(Date, value, color = Label))
    }

    return(ggplotly(p, dynamicTicks = TRUE))
  })

  # Activity B ----
  #* Run wtemp forecasts - Model Uncertainty ----

  wtemp_fc_data <- reactiveValues(lst = as.list(rep(NA, 4)), hist = NULL, fut = NULL)
  observeEvent(input$load_driv1, {
    req(input$table01_rows_selected != "")
    req(input$mod_selec_tab_rows_selected != "")
    idx <- input$mod_selec_tab_rows_selected

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)

    dat$wtemp_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]
    dat$airt_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$airt[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]

    lag_date <- (as.Date(fc_date) + mod_selec_tab$dt$lag[idx])
    mn_date <- (as.Date(fc_date) + 1)

    dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
    dat$wtemp[dat$Date > fc_date] <- NA
    dat$forecast <- NA
    dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
    dat$airt[dat$Date > fc_date] <- NA
    dat$wtemp_yday[dat$Date > lag_date] <- NA
    dat$airt_yday[dat$Date > mn_date] <- NA

    df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
    df <- merge(dat, df, by = "Date", all.y = TRUE)
    wtemp_fc_data$lst[[idx]] <- df
  })

  wtemp_fc_data1a <- reactiveValues(lst = as.list(rep(NA, 4)), hist = NULL, fut = NULL)


  # Process Uncertainty
  wtemp_fc_data2 <- reactiveValues(lst = as.list(rep(NA, 4)), hist = NULL, fut = NULL)
  # observeEvent(input$load_driv2, {
  #
  # })

  # IC Uncertainty
  wtemp_fc_data4 <- reactiveValues(lst = as.list(rep(NA, 4)), hist = NULL, fut = NULL)
  observeEvent(input$load_driv4, {

  })

  # Driver Uncertainty
  wtemp_fc_data5 <- reactiveValues(lst = NULL)
  # observeEvent(input$load_driv5, {
  #
  # })

  mod_selec_tab <- reactiveValues(dt = data.frame(eqn = rep(NA, 4),
                                                  mean = rep(0, 4),
                                                  lag = rep(0, 4),
                                                  r2 = rep(NA, 4),
                                                  rmse = rep(NA, 4)))


  output$mod_selec_tab <- renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$load_mods > 0,
           message = "Please click 'Load models'.")
    )
    mod_selec_tab$dt[, c(1, 4)]
  }, selection = "single",
                                   options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t", autoWidth = TRUE,
                                                  columnDefs = list(list(width = '10%', targets = "_all")),
                                                  scrollX = TRUE),
                                   colnames = c("Model", "R-squared"), rownames = mod_names,
                                   server = FALSE, escape = FALSE)

  # For forecast with weather forecast
  output$mod_selec_tab1a <- renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$load_mods > 0,
           message = "Please click 'Load models'.")
    )
    mod_selec_tab$dt[, c(1, 5)]
  }, selection = "single",
  options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '10%', targets = "_all")),
                 scrollX = TRUE),
  colnames = c("Model", "RMSE (\u00B0C)"), rownames = mod_names,
  server = FALSE, escape = FALSE)

  # For forecast with Process Uncertainty
  output$mod_selec_tab2 <- renderDT({
    dt <- mod_selec_tab$dt[, c(1, 4)]
    idx <- which(!is.na(dt$eqn))
    eqn <- gsub("[$$]+", "", dt$eqn[idx])
    dt$eqn[idx] <- paste0("$$", eqn, " + W_{t}$$")
    dt

  }, selection = "single",
  options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '10%', targets = "_all")),
                 scrollX = TRUE),
  colnames = c("Model", "R-squared"), rownames = mod_names,
  server = FALSE, escape = FALSE)

  #  Parameter Uncertainty
  output$mod_selec_tab3 <- renderDT({
    mod_selec_tab$dt[, c(1, 4)]
  }, selection = "single",
  options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '10%', targets = "_all")),
                 scrollX = TRUE),
  colnames = c("Model", "R-squared"), rownames = mod_names,
  server = FALSE, escape = FALSE)

  # For IC Uncertainty
  output$mod_selec_tab4 <- renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    mod_selec_tab$dt[, c(1, 4)]
  }, selection = "single",
  options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '10%', targets = "_all")),
                 scrollX = TRUE),
  colnames = c("Model", "R-squared"), rownames = mod_names,
  server = FALSE, escape = FALSE)

  # For Driver Uncertainty
  output$mod_selec_tab5 <- renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    mod_selec_tab$dt[, c(1, 4)]
  }, selection = "single",
  options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '10%', targets = "_all")),
                 scrollX = TRUE),
  colnames = c("Model", "R-squared"), rownames = mod_names,
  server = FALSE, escape = FALSE)

  wtemp_fc_out1 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  observe({
    if(is.null(input$mod_selec_tab_rows_selected)) {
      shinyjs::disable("run_wtemp_fc1")
    } else if(is.na(wtemp_fc_data$lst[[input$mod_selec_tab_rows_selected]])) {
      shinyjs::disable("run_wtemp_fc1")
    } else {
      shinyjs::enable("run_wtemp_fc1")
    }
  })

  wtemp_fc_out1a <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  observe({
    if(is.null(input$mod_selec_tab1a_rows_selected)) {
      shinyjs::disable("run_wtemp_fc1a")
    } else {
      shinyjs::enable("run_wtemp_fc1a")
    }
  })

  wtemp_fc_out2 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  observe({
    if(is.null(input$mod_selec_tab2_rows_selected)) {
      shinyjs::disable("run_wtemp_fc2")
    } else {
      shinyjs::enable("run_wtemp_fc2")
    }
  })

  wtemp_fc_out3b <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  observe({
    if(is.null(input$mod_selec_tab3_rows_selected)) {
      shinyjs::disable("run_wtemp_fc3b")
    } else {
      shinyjs::enable("run_wtemp_fc3b")
    }
  })


  # Driver UC
  wtemp_fc_out5 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  observe({
    if(is.null(input$mod_selec_tab5_rows_selected)) {
      shinyjs::disable("run_wtemp_fc5")
    } else {
      shinyjs::enable("run_wtemp_fc5")
    }
  })

  output$txt_fc_out <- renderText({
    validate(
      need(!is.null(input$mod_selec_tab_rows_selected), "Select a model in the table.")
    )
    validate(
      need(!is.na(wtemp_fc_out1$lst[[input$mod_selec_tab_rows_selected]]), "Click 'Run forecast'")
    )
    "Forecast complete!"
  })

  output$txt_fc_out1a <- renderText({
    validate(
      need(!is.null(input$mod_selec_tab1a_rows_selected), "Select a model in the table.")
    )
    validate(
      need(!is.na(wtemp_fc_out1a$lst[[input$mod_selec_tab1a_rows_selected]]), "Click 'Run forecast'")
    )
    "Forecast complete!"
  })

  # Driver forecast
  output$txt_fc_out2 <- renderText({
    validate(
      need(!is.null(input$mod_selec_tab2_rows_selected), "Select a model in the table.")
    )
    validate(
      need(!is.na(wtemp_fc_out2$mlt[[input$mod_selec_tab2_rows_selected]]), "Click 'Run forecast'")
    )
    "Forecast complete!"
  })

  # Parameter UC Forecast
  output$txt_fc_out3b <- renderText({
    validate(
      need(input$mod_selec_tab3_rows_selected != "", "Select a model in the table.")
    )
    validate(
      need(!is.null(input$mod_selec_tab3_rows_selected), "Select a model in the table.")
    )
    validate(
      need(!is.null(input$mod_selec_tab3_rows_selected), "Select a model in the table.")
    )
    if(input$mod_selec_tab3_rows_selected != 2) {
      validate(
        need(!is.na(param_dist3b$dist[[input$mod_selec_tab3_rows_selected]]), "Click 'Generate parameters'.")
      )
    }
    validate(
      need(!is.na(wtemp_fc_out3b$mlt[[input$mod_selec_tab3_rows_selected]]), "Click 'Run forecast'")
    )
    "Forecast complete!"
  })

  # Initial Conditions Forecast
  output$txt_fc_out4 <- renderText({
    validate(
      need(!is.null(input$mod_selec_tab4_rows_selected), "Select a model in the table.")
    )
    validate(
      need(!is.na(wtemp_fc_out4$mlt[[input$mod_selec_tab4_rows_selected]]), "Click 'Run forecast'")
    )
    "Forecast complete!"
  })

  # Driver UC Forecast
  output$txt_fc_out5 <- renderText({
    validate(
      need(!is.null(input$mod_selec_tab5_rows_selected), "Select a model in the table.")
    )
    validate(
      need(!is.na(wtemp_fc_out5$mlt[[input$mod_selec_tab5_rows_selected]]), "Click 'Run forecast'")
    )
    "Forecast complete!"
  })

  observeEvent(input$run_wtemp_fc1, {
    req(input$mod_selec_tab_rows_selected != "")

    df <- wtemp_fc_data$lst[[input$mod_selec_tab_rows_selected]]
    fc_days <- which(df$Date >= fc_date)
    if(input$mod_selec_tab_rows_selected == 1) {
      showModal(modalDialog(
        title = "Uh oh!",
        "This model requires air temperature at the same time step of water temperature to predict water temperature. Currently, we do not have forecasted air temperature so we cannot use this model to predict future water temperature.", easyClose = TRUE
      ))
    } else if(input$mod_selec_tab_rows_selected == 2) {
      for(i in fc_days[-1]) {
        df$forecast[i] <- df$forecast[i-1]
      }
    } else if(input$mod_selec_tab_rows_selected == 3) {
      coeffs <- c(mlr_params$df$beta1[1], mlr_params$df$beta2[1])
      for(i in fc_days[-1]) {
        df$forecast[i] <- df$forecast[i-1] * coeffs[1] + coeffs[2]
      }
    } else if(input$mod_selec_tab_rows_selected == 4) {
      coeffs <- c(mlr_params$df$beta1[2], mlr_params$df$beta2[2], mlr_params$df$beta3[2])
      for(i in fc_days[-1]) {
        df$forecast[i] <- df$airt[i] * coeffs[1] + df$forecast[i-1] * coeffs[2] + coeffs[3]
      }
    }
    wtemp_fc_out1$lst[[input$mod_selec_tab_rows_selected]] <- df[, c("Date", "forecast")]
  })

  # Run forecast WITH forecasted air temperature
  observeEvent(input$run_wtemp_fc1a, {

    req(input$table01_rows_selected != "")
    req(input$mod_selec_tab1a_rows_selected != "")
    idx <- input$mod_selec_tab1a_rows_selected

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)

    dat$wtemp_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]
    dat$airt_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$airt[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]

    lag_date <- (as.Date(fc_date) + mod_selec_tab$dt$lag[idx])
    mn_date <- (as.Date(fc_date) + 1)

    dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
    dat$wtemp[dat$Date > fc_date] <- NA
    dat$forecast <- NA
    dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
    dat$airt[dat$Date > fc_date] <- airt1_fc$df$value[2:8]
    dat$wtemp_yday[dat$Date > lag_date] <- NA
    dat$airt_yday[dat$Date > mn_date] <- NA

    df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
    df <- merge(dat, df, by = "Date", all.y = TRUE)
    wtemp_fc_data1a$lst[[idx]] <- df

    # Run model

    df <- wtemp_fc_data1a$lst[[input$mod_selec_tab1a_rows_selected]]
    fc_days <- which(df$Date >= fc_date)
    if(input$mod_selec_tab1a_rows_selected == 3) {
      for(i in fc_days[-1]) {
        df$forecast[i] <- df$airt[i] * lr_pars$dt$m_est[4] + lr_pars$dt$b_est[4]
      }
    } else if(input$mod_selec_tab1a_rows_selected == 1) {
      for(i in fc_days[-1]) {
        df$forecast[i] <- df$forecast[i-1]
      }
    } else if(input$mod_selec_tab1a_rows_selected == 2) {
      coeffs <- c(mlr_params$df$beta1[1], mlr_params$df$beta2[1])
      for(i in fc_days[-1]) {
        df$forecast[i] <- df$forecast[i-1] * coeffs[1] + coeffs[2]
      }
    } else if(input$mod_selec_tab1a_rows_selected == 4) {
      coeffs <- c(mlr_params$df$beta1[2], mlr_params$df$beta2[2], mlr_params$df$beta3[2])
      for(i in fc_days[-1]) {
        df$forecast[i] <- df$airt[i] * coeffs[1] + df$forecast[i-1] * coeffs[2] + coeffs[3]
      }
    }
    wtemp_fc_out1a$lst[[input$mod_selec_tab1a_rows_selected]] <- df[, c("Date", "forecast")]
  })




  output$wtemp_fc1 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )

    if(any(!is.na(wtemp_fc_out1$lst))) {
      sub_lst <- wtemp_fc_out1$lst[!is.null(wtemp_fc_out1$lst)]
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      mlt$Label <- as.character(mlt$Label)
    }

    p <- ggplot() +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      # coord_cartesian(xlim = c(as.Date("2020-09-22"), as.Date("2020"))) +
      theme_bw(base_size = 12)

    if(any(!is.na(wtemp_fc_out1$lst))) {
      p <- p +
        geom_line(data = mlt, aes(Date, value, color = Label))
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6]))

    return(ggplotly(p, dynamicTicks = TRUE))
  })

  output$wtemp_fc1a <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )

    if(any(!is.na(wtemp_fc_out1a$lst))) {
      sub_lst <- wtemp_fc_out1a$lst[!is.null(wtemp_fc_out1a$lst)]
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      for(num in 1:4) {
        if(num %in% mlt$Label) {
          mlt$Label[mlt$Label == num] <- mod_names[num]
        }
      }
      mlt$Label <- factor(mlt$Label, levels = mod_names)
    }

    p <- ggplot() +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      # coord_cartesian(xlim = c(as.Date("2020-09-22"), as.Date("2020"))) +
      theme_bw(base_size = 12)

    if(any(!is.na(wtemp_fc_out1a$lst))) {
      p <- p +
        geom_line(data = mlt, aes(Date, value, color = Label))
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6]))

    return(ggplotly(p, dynamicTicks = TRUE))
  })

  #* Model Process Uncertainty ----
  wtemp_fc_out2 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)))

  #** Run Forecast - Process UC ----
  # Run forecast WITH forecasted & PROCESS UC air temperature
  observeEvent(input$run_wtemp_fc2, {

    req(input$table01_rows_selected != "")
    req(input$mod_selec_tab2_rows_selected != "")
    idx <- input$mod_selec_tab2_rows_selected

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)

    dat$wtemp_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]
    dat$airt_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$airt[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]

    lag_date <- (as.Date(fc_date) + mod_selec_tab$dt$lag[idx])
    mn_date <- (as.Date(fc_date) + 1)

    dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
    dat$wtemp[dat$Date > fc_date] <- NA
    dat$forecast <- NA
    dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
    dat$airt[dat$Date > fc_date] <- airt1_fc$df$value[2:8]
    dat$wtemp_yday[dat$Date > lag_date] <- NA
    dat$airt_yday[dat$Date > mn_date] <- NA

    df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
    df <- merge(dat, df, by = "Date", all.y = TRUE)
    wtemp_fc_data2$lst[[idx]] <- df

    # Run forecast

    Wt <- 0.2 # Process Uncertainty Noise Std Dev.

    df <- wtemp_fc_data2$lst[[input$mod_selec_tab2_rows_selected]]

    mat <- matrix(NA, 8, input$n_mem2)
    mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
    df <- df[(df$Date >= fc_date), ]
    idx <- input$mod_selec_tab2_rows_selected
    for(mem in 2:nrow(mat)) {

      if(idx == 3) {
        mat[mem, ] <- df$airt[mem] * lr_pars$dt$m_est[4] + lr_pars$dt$b_est[4] + rnorm(input$n_mem2, 0, Wt)
      } else if(idx == 1) {
        mat[mem, ] <- mat[mem-1, ] + rnorm(input$n_mem2, 0, Wt)
      } else if(idx == 2) {
        coeffs <- c(mlr_params$df$beta1[1], mlr_params$df$beta2[1])
        mat[mem, ] <- mat[mem-1, ] * coeffs[1] + coeffs[2] + rnorm(input$n_mem2, 0, Wt)
      } else if(idx == 4) {
        coeffs <- c(mlr_params$df$beta1[2], mlr_params$df$beta2[2], mlr_params$df$beta3[2])
        mat[mem, ] <- df$airt[mem] * coeffs[1] + mat[mem-1, ] * coeffs[2] + coeffs[3] + rnorm(input$n_mem2, 0, Wt)
      }
    }

    # Calculate distributions
    dat <- apply(mat, 1, function(x){
      quantile(x, c(0.05, 0.5, 0.95))
    })
    dat <- as.data.frame(t(dat))
    colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
    dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    dat$Level <- as.character(idx)
    wtemp_fc_out2$dist[[idx]] <- dat

    df2 <- as.data.frame(mat)
    df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    mlt <- reshape::melt(df2, id.vars = "Date")
    mlt$Level <- as.character(idx)
    wtemp_fc_out2$mlt[[idx]] <- mlt

    wtemp_fc_out2$lst[[idx]] <- df[, c("Date", "forecast")]
  })

  #** Plot - Forecast Process UC ----
  output$wtemp_fc2 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out2$mlt)),
           message = "Click 'Run forecast'.")
    )

    p <- ggplot() +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    if(input$plot_type2 == "Line") {
      if(any(!is.na(wtemp_fc_out2$mlt))) {
        sub_lst <- wtemp_fc_out2$mlt[!is.null(wtemp_fc_out2$mlt)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Level, group = variable), alpha = 0.6)
      }
    } else if(input$plot_type2 == "Distribution") {
      if(any(!is.na(wtemp_fc_out2$dist))) {
        sub_lst <- wtemp_fc_out2$dist[!is.null(wtemp_fc_out2$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level))
      }
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                    "Atemp" = l.cols[3], "Both" = l.cols[4]))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)
  })





  #* Parameter Uncertainty ----
  observe({
    if(input$view_at_fc < 1) {
      shinyjs::disable("run_wtemp_fc3a")
    } else {
      shinyjs::enable("run_wtemp_fc3a")
      }
    })

  output$airt1_fc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$view_at_fc,
           message = "Please click 'View forecast")
    )

    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp. - Observed")) +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_line(data = airt1_fc$df, aes(Date, value, color = "Air temp. - Forecast")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    # if(!is.null(wtemp_fc3a$df)) {
    #   p <- p +
    #     geom_line(data = wtemp_fc3a$df, aes(Date, model, color = "Water temp. - Forecast"))
    # }

    return(ggplotly(p, dynamicTicks = TRUE))
  })

  output$lr_mod_eqn <- renderUI({
    validate(
      need(!is.na(mod_selec_tab$dt$eqn[1]),
           message = "Please complete Objective X - Linear Regression")
    )

    eqn <- gsub("[$$]+", "", mod_selec_tab$dt$eqn[1])
    eqn <- paste0("$$", eqn, " ; r^2 = ", mod_selec_tab$dt$r2[1], "$$")
    withMathJax(
      tags$p(eqn)
    )
  })

  # Run one lr wtemp forecast
  wtemp_fc3a <- reactiveValues(df = NULL)
  # observeEvent(input$run_wtemp_fc3a, {
  #   # NEEDS CHECKS
  #
  #   df <- airt1_fc$df
  #   colnames(df)[2] <- "airt"
  #   mod <- predict(lm_fit$fit, df)
  #   dat <- data.frame(Date = df$Date, model = mod)
  #   dat$model[1] <- wtemp_fc_data$hist$wtemp[which(wtemp_fc_data$hist$Date == fc_date)]
  #   wtemp_fc3a$df <- dat
  # })

  output$param_fcast3b <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$gen_lr_dist_plot > 0, "Complete Objective 4!")
    )
    validate(
      need(!is.na(lr_dist_plot$lst[[4]]), "Click 'Generate plot!' in Objective 4.")
    )

    lst <- lr_dist_plot$lst[!is.na(lr_dist_plot$lst)]

    mlt <- do.call(rbind, lst)

    # Just Daily
    # lst <- lr_dist_plot$lst[4]
    #
    # mlt <- lr_dist_plot$lst[[4]]

    y_max_m <- lapply(lst, function(x) {
      dens_m <- density(x$m)
      max(dens_m$y)
    })

    ylims_m <- c(0, max(c(6, max(unlist(y_max_m), na.rm = TRUE))))

    y_max_b <- lapply(lst, function(x) {
      dens_b <- density(x$b)
      max(dens_b$y)
    })

    ylims_b <- c(0, max(c(1.5, max(unlist(y_max_b), na.rm = TRUE))))

    xlims_m <- c(min(0, mlt$m), max(2, mlt$m))
    xlims_b <- c(min(-2.5, mlt$b), max(10, mlt$b))

    # scales_y <- list()

    p1 <- ggplot() +
      geom_density(data = mlt, aes(m, fill = Frequency), color = NA, alpha = 0.3) +
      coord_cartesian(xlim = xlims_m, ylim = ylims_m) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Slope (m)") +
      scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4])) +
      theme_bw(base_size = 16)

    p2 <- ggplot() +
      geom_density(data = mlt, aes(b, fill = Frequency), color = NA, alpha = 0.3) +
      coord_cartesian(xlim = xlims_b, ylim = ylims_b) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Intercept (b)") +
      scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4])) +
      theme_bw(base_size = 16)

    g <- ggpubr::ggarrange(p1, p2, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")
    return(g)

  })

  #** Generate parameter distributions ----
  param_dist3b <- reactiveValues(dist = as.list(rep(NA, 4)))
  observeEvent(input$mod_selec_tab3_rows_selected, {
    if(input$mod_selec_tab3_rows_selected == 1) {
      param_dist3b$dist[[1]] <- "None"
    }
  })
  observeEvent(input$gen_params3b, {
    req(input$mod_selec_tab3_rows_selected != "")

    idx <- input$mod_selec_tab3_rows_selected

    if(idx == 3) {
      req(!is.na(lr_pars$dt$m_est[4]))
      df <- data.frame(m = rnorm(5000, lr_pars$dt$m_est[4], lr_pars$dt$m_se[4]),
                       b = rnorm(5000, lr_pars$dt$b_est[4], lr_pars$dt$b_se[4]))
    } else if(idx == 1) {
      df <- "None"
    } else if(idx == 2) {
      req(!is.na(mlr_params$df$beta1[1]))
      df <- data.frame(m = rnorm(5000, mlr_params$df$beta1[1], mlr_params$df$beta1_se[1]),
                       b = rnorm(5000, mlr_params$df$beta2[1], mlr_params$df$beta2_se[1]))
    } else if(idx == 4) {
      req(!is.na(mlr_params$df$beta1[2]))
      df <- data.frame(beta1 = rnorm(5000, mlr_params$df$beta1[2], mlr_params$df$beta1_se[2]),
                       beta2 = rnorm(5000, mlr_params$df$beta2[2], mlr_params$df$beta2_se[2]),
                       beta3 = rnorm(5000, mlr_params$df$beta3[2], mlr_params$df$beta3_se[2]))
    }
    param_dist3b$dist[[idx]] <- df
  })

  output$param_dist3b <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab3_rows_selected != "", "Please select a model in the table.")
    )
    idx <- input$mod_selec_tab3_rows_selected

    if(idx == 1) {
      validate(
        need(param_dist3b$dist[[idx]] != "None", "Uh oh! Looks like the model you selected has no parameters, but you can still generate a forecast with it below.")
      )
    }

    validate(
      need(!is.na(param_dist3b$dist[[idx]]), "Click 'Generate parameters'.")
    )

    mlt <- reshape::melt(param_dist3b$dist[[idx]])

    ggplot(mlt) +
      geom_density(aes(value), fill = l.cols[idx], alpha = 0.5) +
      facet_wrap(~variable, nrow = 1, scales = "free_x") +
      # scale_fill_manual(values = l.cols[idx]) +
      theme_bw(base_size = 16)

  })

  #** Run Parameter UC ensemble ----
  wtemp_fc3b <- reactiveValues(mlt = NULL, dist = NULL)
  observeEvent(input$run_wtemp_fc3b, {
    # NEEDS CHECKS

    req(input$mod_selec_tab3_rows_selected != "")
    idx <- input$mod_selec_tab3_rows_selected

    req(!is.na(param_dist3b$dist[[idx]]))

    pars <- param_dist3b$dist[[idx]]
    if(idx != 1) {
      pars <- pars[sample(1:nrow(pars), size = 100), ]
    }

    df <- airt1_fc$df
    colnames(df)[2] <- "airt"

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp)

    mat <- matrix(NA, 8, 100)
    mat[1, ] <- dat$wtemp[which(dat$Date == fc_date)]
    df <- df[(df$Date >= fc_date), ]
    for(mem in 2:nrow(mat)) {

      if(idx == 3) {
        mat[mem, ] <- df$airt[mem] * pars$m + pars$b
      } else if(idx == 1) {
        mat[mem, ] <- mat[mem-1, ]
      } else if(idx == 2) {
        mat[mem, ] <- mat[mem-1, ] * pars$m + pars$b
      } else if(idx == 4) {
        mat[mem, ] <- df$airt[mem] * pars$beta1 + mat[mem-1, ] * pars$beta2 + pars$beta3
      }
    }

    # Calculate distributions
    dat <- apply(mat, 1, function(x){
      quantile(x, c(0.05, 0.5, 0.95))
    })
    dat <- as.data.frame(t(dat))
    colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
    dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    dat$Level <- as.character(idx)
    wtemp_fc_out3b$dist[[idx]] <- dat

    df2 <- as.data.frame(mat)
    df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    mlt <- reshape::melt(df2, id.vars = "Date")
    mlt$Level <- as.character(idx)
    wtemp_fc_out3b$mlt[[idx]] <- mlt

    # wtemp_fc_out3b$lst[[idx]] <- df[, c("Date", "forecast")]
  })

  #** Plot - Parameter UC ----
  output$wtemp_fc3b <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$mod_selec_tab3_rows_selected != "",
           message = "Please select a model in the table.")
    )

    idx <- input$mod_selec_tab3_rows_selected

    if(idx != 1) {
      validate(
        need(!is.na(param_dist3b$dist[[idx]]), "Click 'Generate parameters'.")
      )
    }

    validate(
      need(any(!is.na(wtemp_fc_out3b$mlt)),
           message = "Click 'Run forecast'.")
    )

    # if(any(!is.na(wtemp_fc_out3b$lst))) {
    #   sub_lst <- wtemp_fc_out3b$lst[!is.na(wtemp_fc_out3b$lst)]
    #   mlt <- reshape::melt(sub_lst, id.vars = "Date")
    #   print(head(mlt))
    #   colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
    #   mlt$Label <- as.character(mlt$Label)
    # }

    p <- ggplot() +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    if(input$plot_type3b == "Line") {
      if(any(!is.na(wtemp_fc_out3b$mlt))) {
        sub_lst <- wtemp_fc_out3b$mlt[!is.null(wtemp_fc_out3b$mlt)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Level, group = variable), alpha = 0.6)
      }
    } else if(input$plot_type3b == "Distribution") {
      if(any(!is.na(wtemp_fc_out3b$dist))) {
        sub_lst <- wtemp_fc_out3b$dist[!is.null(wtemp_fc_out3b$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level))
      }
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4]))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)
  })

  #* Initial Condition Uncertainty ----
  ic_dist <- reactiveValues(df = NULL)

  #** Generate IC distribution ----
  observeEvent(input$gen_ic, {
    req(input$table01_rows_selected != "")
    req(!is.null(wtemp_fc_data$hist))
    mn_wtemp <- wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date == fc_date]
    ic_dist$df <- data.frame(value = rnorm(1000, mn_wtemp, input$ic_uc))
  })

  #** Plot - IC distribution ----
  output$ic_uc_plot <- renderPlot({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(ic_dist$df), "Click 'Generate distribution")
    )

    df <- data.frame(x = wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date == fc_date],
                     label = "Observed")

    xlims <- c(df$x -1.5, df$x + 1.5)
    ylims <- c(0,7)

    p <- ggplot() +
      # geom_vline(data = df, aes(xintercept = x, color = label)) +
      geom_vline(xintercept = df$x) +
      geom_density(data = ic_dist$df, aes(value), fill = l.cols[2], alpha = 0.3) +
      xlab("Temperature (\u00B0C)") +
      ylab("Density") +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_bw(base_size = 18)

    return(p)

    # ggplotly(p, dynamicTicks = TRUE) %>%
    #   layout(xaxis = list(range = xlims), yaxis = list(range = ylims))

  })

  #** Disable button - IC ----
  wtemp_fc_out4 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  observe({
    if(is.null(input$mod_selec_tab4_rows_selected)) {
      shinyjs::disable("run_wtemp_fc4")
    } else {
      shinyjs::enable("run_wtemp_fc4")
    }
  })

  #** Run Forecast - IC UC ----
  wtemp_fc4 <- reactiveValues(mlt = NULL, dist = NULL)
  # Run forecast WITH forecasted & IC UC air temperature
  observeEvent(input$run_wtemp_fc4, {
    req(input$table01_rows_selected != "")
    req(input$mod_selec_tab4_rows_selected != "")
    idx <- input$mod_selec_tab4_rows_selected

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)

    dat$wtemp_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]
    dat$airt_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$airt[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]

    lag_date <- (as.Date(fc_date) + mod_selec_tab$dt$lag[idx])
    mn_date <- (as.Date(fc_date) + 1)


    dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
    dat$wtemp[dat$Date > fc_date] <- NA
    dat$forecast <- NA
    dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
    dat$airt[dat$Date > fc_date] <- airt1_fc$df$value[2:8]
    dat$wtemp_yday[dat$Date > lag_date] <- NA
    dat$airt_yday[dat$Date > mn_date] <- NA

    df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
    df <- merge(dat, df, by = "Date", all.y = TRUE)
    wtemp_fc_data4$lst[[idx]] <- df

    # Running Forecast

    df <- wtemp_fc_data4$lst[[input$mod_selec_tab4_rows_selected]]

    mat <- matrix(NA, 8, 100)
    mat[1, ] <- rnorm(100, df$wtemp[which(df$Date == fc_date)], sd = input$ic_uc)
    df <- df[(df$Date >= fc_date), ]
    idx <- input$mod_selec_tab4_rows_selected
    for(mem in 2:nrow(mat)) {
      if(idx == 3) {
        mat[mem, ] <- df$airt[mem] * lr_pars$dt$m_est[4] + lr_pars$dt$b_est[4]
      } else if(idx == 1) {
        mat[mem, ] <- mat[mem-1, ]
      } else if(idx == 2) {
        coeffs <- c(mlr_params$df$beta1[1], mlr_params$df$beta2[1])
        mat[mem, ] <- mat[mem-1, ] * coeffs[1] + coeffs[2]
      } else if(idx == 4) {
        coeffs <- c(mlr_params$df$beta1[2], mlr_params$df$beta2[2], mlr_params$df$beta3[2])
        mat[mem, ] <- df$airt[mem] * coeffs[1] + mat[mem-1, ] * coeffs[2] + coeffs[3]
      }
    }

    # Calculate distributions
    dat <- apply(mat, 1, function(x){
      quantile(x, c(0.05, 0.5, 0.95))
    })
    dat <- as.data.frame(t(dat))
    colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
    dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    dat$Level <- as.character(idx)
    wtemp_fc_out4$dist[[idx]] <- dat

    df2 <- as.data.frame(mat)
    df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    mlt <- reshape::melt(df2, id.vars = "Date")
    mlt$Level <- as.character(idx)
    wtemp_fc_out4$mlt[[idx]] <- mlt

    wtemp_fc_out4$lst[[idx]] <- df[, c("Date", "forecast")]
  })

  # Recent obs timeseries
  output$ic_obs_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    df <- wtemp_fc_data$hist[2:5, ]

    p <- ggplot()

    if(!is.null(ic_dist$df)) {
      quants <- quantile(ic_dist$df$value, c(0.25, 0.75))

      err_bar <- data.frame(x = as.Date(fc_date), ymin = quants[1], ymax = quants[2])
      p <- p +
        geom_errorbar(data = err_bar, aes(x, ymin = ymin, ymax = ymax, width = 0.5), )
    }

    p <- p +
      geom_point(data = df, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      xlab("Date") +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      theme_bw(base_size = 12) +
      theme(legend.position = "none")



    return(ggplotly(p, dynamicTicks = TRUE))
  })

  #** Plot - IC Forecast ----
  # Plot IC UC
  output$wtemp_fc4 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out4$mlt)),
           message = "Click 'Run forecast'.")
    )

    p <- ggplot() +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    if(input$plot_type4 == "Line") {
      if(any(!is.na(wtemp_fc_out4$mlt))) {
        sub_lst <- wtemp_fc_out4$mlt[!is.null(wtemp_fc_out4$mlt)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Level, group = variable), alpha = 0.6)
      }
    } else if(input$plot_type4 == "Distribution") {
      if(any(!is.na(wtemp_fc_out4$dist))) {
        sub_lst <- wtemp_fc_out4$dist[!is.null(wtemp_fc_out4$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level))
      }
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4]))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  #* Driver Uncertainty ----
  #** Plot - FC NOAA Air temperature ----
  output$airt_fc5 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt), "Please click 'Load forecast'")
    )

    mlt <- noaa_df$airt

    mlt$Date <- as.Date(mlt$time)
    mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
    mlt <- mlt[mlt$Date <= "2020-10-02", ]
    mlt$time <- as.POSIXct(mlt$Date)
    fut_offset <- lubridate::days(6) #+ lubridate::hours(19)

    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      # geom_line(data = airt1_fc$df, aes(Date, value, color = "Air temp. - Forecast")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    if(input$plot_type5 == "Line") {

      mlt <- mlt[mlt$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]
      p <- p +
        geom_line(data = mlt, aes(Date, value, group = variable), color = "gray", alpha = 0.6)
    } else if(input$plot_type5 == "Distribution") {
      validate(
        need(input$noaa_n_mems > 2, "Need more than 2 members to create a distribution.")
      )

      wid <- tidyr::pivot_wider(mlt, c(Date, L1), names_from = variable, values_from = value)
      wid <- wid[, 1:(input$noaa_n_mems + 2)]
      df <- apply(wid[, -c(1, 2)], 1, function(x){
        quantile(x, c(0.05, 0.5, 0.875, 0.95))
      })
      df <- as.data.frame(t(df))
      colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
      df$Date <- wid$Date
      p <- p +
        geom_ribbon(data = df, aes(Date, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3)+
        # geom_ribbon(data = df, aes(Date, ymin = p12.5, ymax = p87.5, fill = "75%"), alpha = 0.3)+
        geom_line(data = df, aes(Date, p50, color = "Median"))
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4]))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)
  })

  #** Run forecast - Driver UC ----
  wtemp_fc5 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)))
  # Run forecast WITH forecasted & Driver UC air temperature
  observeEvent(input$run_wtemp_fc5, {

    req(input$table01_rows_selected != "")

    mlt <- noaa_df$airt
    mlt$Date <- as.Date(mlt$time)
    mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
    mlt <- mlt[mlt$Date <= "2020-10-02", ]

    wid <- tidyr::pivot_wider(mlt, c(Date, L1), names_from = variable, values_from = value)
    wid <- as.data.frame(wid)


    idx <- input$mod_selec_tab5_rows_selected

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)

    dat$wtemp_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]
    dat$airt_yday[-c(1:mod_selec_tab$dt$lag[idx])] <- dat$airt[-c((nrow(dat)+1-mod_selec_tab$dt$lag[idx]):nrow(dat))]

    lag_date <- (as.Date(fc_date) + mod_selec_tab$dt$lag[idx])
    mn_date <- (as.Date(fc_date) + 1)

    df <- wtemp_fc_data5$lst[[1]]

    mat <- matrix(NA, 8, input$noaa_n_mems)
    mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
    df <- df[(df$Date >= fc_date), ]
    idx <- input$mod_selec_tab5_rows_selected
    driv_mat <- sapply(1:input$noaa_n_mems, function(x) wtemp_fc_data5$lst[[x]]$airt[wtemp_fc_data5$lst[[x]]$Date >= fc_date] )
    for(mem in 2:nrow(mat)) {
      if(idx == 1) {
        mat[mem, ] <- mat[mem-1, ]
      } else if(idx == 2) {
        coeffs <- c(mlr_params$df$beta1[1], mlr_params$df$beta2[1])
        mat[mem, ] <- mat[mem-1, ] * coeffs[1] + coeffs[2]
      } else if(idx == 3) {
        mat[mem, ] <- driv_mat[mem, ] * lr_pars$dt$m_est[4] + lr_pars$dt$b_est[4]
      } else if(idx == 4) {
        coeffs <- c(mlr_params$df$beta1[2], mlr_params$df$beta2[2], mlr_params$df$beta3[2])
        mat[mem, ] <- driv_mat[mem, ] * coeffs[1] + mat[mem-1, ] * coeffs[2] + coeffs[3]
      }
    }


    # Calculate distributions
    dat <- apply(mat, 1, function(x){
      quantile(x, c(0.05, 0.5, 0.95))
    })
    dat <- as.data.frame(t(dat))
    colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
    dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    dat$Level <- as.character(idx)
    wtemp_fc_out5$dist[[idx]] <- dat

    df2 <- as.data.frame(mat)
    df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
    mlt <- reshape::melt(df2, id.vars = "Date")
    mlt$Level <- as.character(idx)
    wtemp_fc_out5$mlt[[idx]] <- mlt

    wtemp_fc_out5$lst[[idx]] <- df[, c("Date", "forecast")]
  })

  #** Plot - Forecast Driver UC ----
  output$wtemp_fc5 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out5$mlt)),
           message = "Click 'Run forecast'.")
    )

    if(any(!is.na(wtemp_fc_out5$lst))) {
      sub_lst <- wtemp_fc_out5$lst[!is.na(wtemp_fc_out5$lst)]
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      mlt$Label <- as.character(mlt$Label)
    }

    p <- ggplot() +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    if(input$plot_type5 == "Line") {
      if(any(!is.na(wtemp_fc_out5$mlt))) {
        sub_lst <- wtemp_fc_out5$mlt[!is.null(wtemp_fc_out5$mlt)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Level, group = variable), alpha = 0.6)
      }
    } else if(input$plot_type5 == "Distribution") {
      if(any(!is.na(wtemp_fc_out5$dist))) {
        sub_lst <- wtemp_fc_out5$dist[!is.null(wtemp_fc_out5$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level))
      }
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4]))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  #* Summary Plots ----
  #** Process Uncertainty Summary ----
  output$proc_uc_summ <- renderPlot({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out2$mlt)),
           message = "Click 'Run forecast' in Objective 6.")
    )

    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp."), size = 2) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 22)

    if(any(!is.na(wtemp_fc_out2$dist))) {
      sub_lst <- wtemp_fc_out2$dist[!is.null(wtemp_fc_out2$dist)]
      mlt <- do.call(rbind, sub_lst)
      mlt <- na.exclude(mlt)
      for(num in 1:4) {
        if(num %in% mlt$Level) {
          mlt$Level[mlt$Level == num] <- mod_names[num]
        }
      }
      mlt$Level <- factor(mlt$Level, levels = mod_names)

      p <- p +
        geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.5) +
        geom_line(data = mlt, aes(Date, p50, color = Level))
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      guides(color = "none") +
      coord_cartesian(xlim = c(as.Date(fc_date)-1, as.Date(fc_date)+7)) +
      labs(fill = "Model")
    return(p)
  })

  #** Parameter Uncertainty Summary ----
  output$param_uc_summ <- renderPlot({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out3b$mlt)),
           message = "Click 'Run forecast' in Objective 7.")
    )

    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 22)

    if(any(!is.na(wtemp_fc_out3b$dist))) {
      sub_lst <- wtemp_fc_out3b$dist[!is.null(wtemp_fc_out3b$dist)]
      mlt <- do.call(rbind, sub_lst)
      mlt <- na.exclude(mlt)
      for(num in 1:4) {
        if(num %in% mlt$Level) {
          mlt$Level[mlt$Level == num] <- mod_names[num]
        }
      }
      mlt$Level <- factor(mlt$Level, levels = mod_names)

      p <- p +
        geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.5) +
        geom_line(data = mlt, aes(Date, p50, color = Level))
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      guides(color = "none") +
      coord_cartesian(xlim = c(as.Date(fc_date)-1, as.Date(fc_date)+7)) +
      labs(fill = "Model")
    return(p)
  })

  #** Initial Conditions Uncertainty Summary ----
  output$ic_uc_summ <- renderPlot({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out4$mlt)),
           message = "Click 'Run forecast' in Objective 8.")
    )

    if(any(!is.na(wtemp_fc_out4$lst))) {
      sub_lst <- wtemp_fc_out4$lst[!is.na(wtemp_fc_out4$lst)]
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      mlt$Label <- as.character(mlt$Label)
    }

    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 22)

    if(any(!is.na(wtemp_fc_out4$dist))) {
      sub_lst <- wtemp_fc_out4$dist[!is.null(wtemp_fc_out4$dist)]
      mlt <- do.call(rbind, sub_lst)
      mlt <- na.exclude(mlt)
      for(num in 1:4) {
        if(num %in% mlt$Level) {
          mlt$Level[mlt$Level == num] <- mod_names[num]
        }
      }
      mlt$Level <- factor(mlt$Level, levels = mod_names)

      p <- p +
        geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.5) +
        geom_line(data = mlt, aes(Date, p50, color = Level))
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      guides(color = "none") +
      coord_cartesian(xlim = c(as.Date(fc_date)-1, as.Date(fc_date)+7)) +
      labs(fill = "Model")
    return(p)
  })

  #** Driver Uncertainty Summary ----
  output$driver_uc_summ <- renderPlot({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out5$mlt)),
           message = "Click 'Run forecast' in Objective 9.")
    )

    if(any(!is.na(wtemp_fc_out5$lst))) {
      sub_lst <- wtemp_fc_out5$lst[!is.na(wtemp_fc_out5$lst)]
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      mlt$Label <- as.character(mlt$Label)
    }

    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 22)

    if(any(!is.na(wtemp_fc_out5$dist))) {
      sub_lst <- wtemp_fc_out5$dist[!is.null(wtemp_fc_out5$dist)]
      mlt <- do.call(rbind, sub_lst)
      mlt <- na.exclude(mlt)
      for(num in 1:4) {
        if(num %in% mlt$Level) {
          mlt$Level[mlt$Level == num] <- mod_names[num]
        }
      }
      mlt$Level <- factor(mlt$Level, levels = mod_names)

      p <- p +
        geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.5) +
        geom_line(data = mlt, aes(Date, p50, color = Level))
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      guides(color = "none") +
      coord_cartesian(xlim = c(as.Date(fc_date)-1, as.Date(fc_date)+7)) +
      labs(fill = "Model")
    return(p)
  })


  # Activity C ----

  # Activity C - model
  output$modA_txt <- renderText({
    validate(
      need(input$mod_selec_tot_fc != "", "")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "")
    )

    if(input$mod_selec_tot_fc[1] == "Pers") {
      txt <- "We will use the persistence model. This model predicts that tomorrow's water temperature will be the same as today."
    } else if (input$mod_selec_tot_fc[1] == "Wtemp") {
      txt <- "We will use the water temperature linear regression model. This model uses today's water temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[1] == "Atemp") {
      txt <- "We will use the air temperature linear regression model. This model uses tomorrow's air temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[1] == "Both") {
      txt <- "We will use the multiple linear regression model with air and water temperature. This model uses today's water temperature and tomorrow's air temperature as explanatory variables in a multiple linear regression."
    }
    return(txt)
  })

  output$modB_txt <- renderText({
    validate(
      need(input$mod_selec_tot_fc != "", "")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "")
    )

    if(input$mod_selec_tot_fc[2] == "Pers") {
      txt <- "We will use the persistence model. This model predicts that tomorrow's water temperature will be the same as today."
    } else if (input$mod_selec_tot_fc[2] == "Wtemp") {
      txt <- "We will use the water temperature linear regression model. This model uses today's water temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[2] == "Atemp") {
      txt <- "We will use the air temperature linear regression model. This model uses tomorrow's air temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[2] == "Both") {
      txt <- "We will use the multiple linear regression model with air and water temperature. This model uses today's water temperature and tomorrow's air temperature as explanatory variables in a multiple linear regression."
    }
    return(txt)
  })

  output$modA_eqn <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )

    if(input$mod_selec_tot_fc[1] == "Pers") {
      eqn <- mod_selec_tab$dt[1, 1]
    } else if (input$mod_selec_tot_fc[1] == "Wtemp") {
      eqn <- mod_selec_tab$dt[2, 1]
    } else if (input$mod_selec_tot_fc[1] == "Atemp") {
      eqn <- mod_selec_tab$dt[3, 1]
    } else if (input$mod_selec_tot_fc[1] == "Both") {
      eqn <- mod_selec_tab$dt[4, 1]
    }
    eqn <- gsub("[$$]+", "", eqn)
    eqn <- paste0("$$", eqn, " + W_{t}$$")

    withMathJax(
      div(eqn)
    )
  })

  output$modB_eqn <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )

    if(input$mod_selec_tot_fc[2] == "Pers") {
      eqn <- mod_selec_tab$dt[1, 1]
    } else if (input$mod_selec_tot_fc[2] == "Wtemp") {
      eqn <- mod_selec_tab$dt[2, 1]
    } else if (input$mod_selec_tot_fc[2] == "Atemp") {
      eqn <- mod_selec_tab$dt[3, 1]
    } else if (input$mod_selec_tot_fc[2] == "Both") {
      eqn <- mod_selec_tab$dt[4, 1]
    }

    eqn <- gsub("[$$]+", "", eqn)
    eqn <- paste0("$$", eqn, " + W_{t}$$")

    withMathJax(
      div(eqn)
    )
  })

  output$mod4_eqn <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.na(mlr$dt$Equation[2]),
           message = "Complete Objective 5")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )


    eqn <- gsub("[$$]+", "", mlr$dt$Equation[2])
    eqn <- paste0("$$", eqn, " + W_{t}$$")

    withMathJax(
      div(eqn)
    )
  })

  # Uncertainty Partitioning ----
  # reset plots when models are changed
  observeEvent(input$mod_selec_tot_fc, {
    tot_fc_dataA$mlt <- NULL
    tot_fc_dataA$dist <- NULL
    tot_fc_dataA$mat <- NULL
    tot_fc_dataA$lab <- NULL

    tot_fc_dataB$mlt <- NULL
    tot_fc_dataB$dist <- NULL
    tot_fc_dataB$mat <- NULL
    tot_fc_dataB$lab <- NULL

    quantfcA$df <- NULL
  })

  #** Plot - Total Forecast Uncertainty A ----
  output$tot_fc_uncertA <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )

    idx <- which(mod_names == input$mod_selec_tot_fc[1])
    sel_col <- cols[idx]

    if(idx != 1) {
      validate(
        need(!is.na(param_dist3b$dist[[idx]]), "Need to generate parameters in Objective 7.")
      )
    }
    validate(
      need(!is.null(noaa_df$airt), "Please click 'Load forecast' in Objective 9.")
    )
    validate(
      need(!is.null(tot_fc_dataA$dist), "Click 'Run forecast'")
    )

    dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]


    p <- ggplot() +
      # geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp.")) +
      geom_point(data = dat, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    if(input$plot_type_totA == "Line") {
      if(!is.null(tot_fc_dataA$mlt)) {

        mlt <- tot_fc_dataA$mlt

        p <- p +
          geom_line(data = mlt, aes(Date, value, group = variable), color = sel_col, alpha = 0.6)
      }
    } else if(input$plot_type_totA == "Distribution") {
      if(!is.null(tot_fc_dataA$dist)) {
        mlt <- tot_fc_dataA$dist

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95), fill = sel_col, alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50), color = sel_col)
      }
    }

    # if(input$add_obs1) {
    #   p <- p +
    #     geom_point(data = wtemp_fc_data$fut, aes(Date, wtemp, color = "F Water temp."))
    # }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d")

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  tot_fc_dataA <- reactiveValues(mlt = NULL, dist = NULL, mat = NULL, lab = NULL)
  observeEvent(input$run_tot_fcA, {

    req(input$table01_rows_selected != "")
    req(length(input$mod_selec_tot_fc) == 2)

    idx <- which(mod_names == input$mod_selec_tot_fc[1])

    req(!is.na(param_dist3b$dist[[idx]]))
    req(!is.null(wtemp_fc_data5$lst))

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Forecasting water temperature"),
                 detail = "This may take a while. This window will disappear
                     when it is finished loading.", value = 0.05)

    df <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp)
    tot_fc_dataA$lab <- input$mod_selec_tot_fc[1]

    for(fc_uncertA in uc_sources) {

      pidx <- which(uc_sources == fc_uncertA)
      mat <- matrix(NA, 8, input$tot_fc_mem)

      if("Driver" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        driv_mat <- sapply(1:input$noaa_n_mems, function(x) wtemp_fc_data5$lst[[x]]$airt[wtemp_fc_data5$lst[[x]]$Date >= fc_date])
        tmes <- ceiling(input$tot_fc_mem / input$noaa_n_mems)
        M <- do.call(cbind, replicate(tmes, driv_mat, simplify = FALSE))
        driv_mat <- M[, 1:input$tot_fc_mem]
      } else {
        driv_mat <- sapply(1, function(x) wtemp_fc_data5$lst[[x]]$airt[wtemp_fc_data5$lst[[x]]$Date >= fc_date])
      }
      if("Parameter" %in% fc_uncertA | "Total" %in% fc_uncertA) {

        idx <- sample(1:5000, input$tot_fc_mem)

        if(input$mod_selec_tot_fc[1] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else if (input$mod_selec_tot_fc[1] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = pars$beta1[idx],
                               beta2 = pars$beta2[idx],
                               beta3 = pars$beta3[idx])
        } else if(input$mod_selec_tot_fc[1] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else {
          params <- "NULL"
        }
      } else {
        if(input$mod_selec_tot_fc[1] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else if (input$mod_selec_tot_fc[1] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = mean(pars$beta1),
                               beta2 = mean(pars$beta2),
                               beta3 = mean(pars$beta3))
        } else if(input$mod_selec_tot_fc[1] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else {
          params <- "NULL"
        }
      }
      if("Initial Conditions" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        mat[1, ] <- rnorm(input$tot_fc_mem, df$wtemp[which(df$Date == fc_date)], sd = input$ic_uc)
      } else {
        mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
      }

      for(mem in 2:nrow(mat)) {

        # Calculate process noise each time step
        if("Process" %in% fc_uncertA | "Total" %in% fc_uncertA) {
          Wt <- rnorm(input$tot_fc_mem, 0, 0.2)
        } else {
          Wt <- 0
        }

        if(input$mod_selec_tot_fc[1] == "Atemp") {
          mat[mem, ] <- params$m * driv_mat[mem, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[1] == "Pers") {
          mat[mem, ] <- mat[mem-1, ] + Wt
        } else if (input$mod_selec_tot_fc[1] == "Wtemp") {
          mat[mem, ] <- params$m * mat[mem-1, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[1] == "Both") {
          mat[mem, ] <- driv_mat[mem, ] * params$beta1 + mat[mem-1, ] * params$beta2 + params$beta3 + Wt
        }
      }

      # Calculate distributions
      tot_fc_dataA$mat <- mat

      if(fc_uncertA == "Total") {
        dat <- apply(mat, 1, function(x) {
          quantile(x, c(0.05, 0.5, 0.95))
        })
        dat <- as.data.frame(t(dat))
        colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
        dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        # dat$Level <- as.character(idx)
        tot_fc_dataA$dist <- dat
        df2 <- as.data.frame(mat)
        df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        mlt <- reshape::melt(df2, id.vars = "Date")
        # mlt$Level <- as.character(idx)
        tot_fc_dataA$mlt <- mlt
      }

      if(fc_uncertA != "Total") {
        # Quantify UC
        std <- apply(tot_fc_dataA$mat, 1, sd)

        df2 <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                          sd = std, label = fc_uncertA)

        if(is.null(quantfcA$df)) {
          quantfcA$df <- df2
        } else {
          # Overwrite previous Std Dev.
          if((df2$label[1] %in% quantfcA$df$label)) {
            idx <- which(quantfcA$df$label %in% df2$label[1])
            quantfcA$df[idx, ] <- df2
          } else {
            quantfcA$df <- rbind(quantfcA$df, df2)
          }
        }
      }

      progress$set(value = (pidx / length(uc_sources)))
    }
  })

  #* Quantify Forecast UC A ----
  quantfcA <- reactiveValues(df = NULL)
  observeEvent(input$quant_ucA, {
    req(input$table01_rows_selected != "")

    # std <- apply(tot_fc_dataA$mat, 1, sd)
    #
    # df <- data.frame(Date = tot_fc_dataA$dist$Date,
    #                  sd = std, label = tot_fc_dataA$lab)
    #
    # if(is.null(quantfcA$df)) {
    #   quantfcA$df <- df
    # } else {
    #   # Overwrite previous Std Dev.
    #   if((df$label[1] %in% quantfcA$df$label)) {
    #     idx <- which(quantfcA$df$label %in% df$label[1])
    #     quantfcA$df[idx, ] <- df
    #   } else {
    #     quantfcA$df <- rbind(quantfcA$df, df)
    #   }
    # }
  })

  output$fc_quantA <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(quantfcA$df), "Click 'Run forecast' above.")
    )
    validate(
      need(input$quant_ucA > 0, "Click 'Quantify uncertainty'")
    )

    p <- ggplot() +
      geom_bar(data = quantfcA$df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
      ylab("Standard Deviation (\u00B0C)") +
      scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                   "Driver" = cols2[4], "Total" = cols2[5])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(fill = "Uncertainty") +
      theme_bw(base_size = 12)

    gp <- ggplotly(p, dynamicTicks = TRUE)
    return(gp)
  })

  # Uncertainty plots B
  output$tot_fc_uncertB <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )

    idx <- which(mod_names == input$mod_selec_tot_fc[2])
    sel_col <- cols[idx]

    if(idx != 1) {
      validate(
        need(!is.na(param_dist3b$dist[[idx]]), "Need to generate parameters in Objective 7.")
      )
    }
    validate(
      need(!is.null(noaa_df$airt), "Please click 'Load forecast' in Objective 9.")
    )
    validate(
      need(!is.null(tot_fc_dataA$dist), "Click 'Run forecast'")
    )

    dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]

    p <- ggplot() +
      geom_point(data = dat, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)

    if(input$plot_type_totB == "Line") {
      if(!is.null(tot_fc_dataB$mlt)) {

        mlt <- tot_fc_dataB$mlt

        p <- p +
          geom_line(data = mlt, aes(Date, value, group = variable), color = sel_col, alpha = 0.6)
      }
    } else if(input$plot_type_totB == "Distribution") {
      if(!is.null(tot_fc_dataB$dist)) {
        mlt <- tot_fc_dataB$dist

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95), fill = sel_col, alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50), color = sel_col)
      }
    }

    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d")


    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  tot_fc_dataB <- reactiveValues(mlt = NULL, dist = NULL, mat = NULL, lab = NULL)
  observeEvent(input$run_tot_fcB, {

    req(input$table01_rows_selected != "")
    req(!is.null(wtemp_fc_data5$lst))
    req(!is.na(lr_pars$dt$m_est[4]))

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Forecasting water temperature"),
                 detail = "This may take a while. This window will disappear
                     when it is finished loading.", value = 0.05)

    df <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp)

    tot_fc_dataB$lab <- input$mod_selec_tot_fc[2]

    for(fc_uncertA in uc_sources) {

      mat <- matrix(NA, 8, input$tot_fc_mem)
      pidx <- which(uc_sources == fc_uncertA)


      if("Driver" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        driv_mat <- sapply(1:input$noaa_n_mems, function(x) wtemp_fc_data5$lst[[x]]$airt[wtemp_fc_data5$lst[[x]]$Date >= fc_date])
        tmes <- ceiling(input$tot_fc_mem / input$noaa_n_mems)
        M <- do.call(cbind, replicate(tmes, driv_mat, simplify = FALSE))
        driv_mat <- M[, 1:input$tot_fc_mem]
      } else {
        driv_mat <- sapply(1, function(x) wtemp_fc_data5$lst[[x]]$airt[wtemp_fc_data5$lst[[x]]$Date >= fc_date])
      }
      if("Parameter" %in% fc_uncertA | "Total" %in% fc_uncertA) {

        idx <- sample(1:5000, input$tot_fc_mem)

        if(input$mod_selec_tot_fc[2] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else if (input$mod_selec_tot_fc[2] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = pars$beta1[idx],
                               beta2 = pars$beta2[idx],
                               beta3 = pars$beta3[idx])
        } else if(input$mod_selec_tot_fc[2] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else {
          params <- "NULL"
        }
      } else {
        if(input$mod_selec_tot_fc[2] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else if (input$mod_selec_tot_fc[2] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = mean(pars$beta1),
                               beta2 = mean(pars$beta2),
                               beta3 = mean(pars$beta3))
        } else if(input$mod_selec_tot_fc[2] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else {
          params <- "NULL"
        }
      }
      if("Initial Conditions" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        mat[1, ] <- rnorm(input$tot_fc_mem, df$wtemp[which(df$Date == fc_date)], sd = input$ic_uc)
      } else {
        mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
      }

      for(mem in 2:nrow(mat)) {
        # Calculate process noise each step
        if("Process" %in% fc_uncertA | "Total" %in% fc_uncertA) {
          Wt <- rnorm(input$tot_fc_mem, 0, 0.2)
        } else {
          Wt <- 0
        }

        if(input$mod_selec_tot_fc[2] == "Atemp") {
          mat[mem, ] <- params$m * driv_mat[mem, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[2] == "Pers") {
          mat[mem, ] <- mat[mem-1, ] + Wt
        } else if (input$mod_selec_tot_fc[2] == "Wtemp") {
          mat[mem, ] <- params$m * mat[mem-1, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[2] == "Both") {
          mat[mem, ] <- driv_mat[mem, ] * params$beta1 + mat[mem-1, ] * params$beta2 + params$beta3 + Wt
        }
      }

      # Calculate distributions
      tot_fc_dataB$mat <- mat

      if(fc_uncertA == "Total") {
        dat <- apply(mat, 1, function(x) {
          quantile(x, c(0.05, 0.5, 0.95))
        })
        dat <- as.data.frame(t(dat))
        colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
        dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        # dat$Level <- as.character(idx)
        tot_fc_dataB$dist <- dat
        df2 <- as.data.frame(mat)
        df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        mlt <- reshape::melt(df2, id.vars = "Date")
        # mlt$Level <- as.character(idx)
        tot_fc_dataB$mlt <- mlt
      }

      if(fc_uncertA != "Total") {
        # Quantify UC
        std <- apply(tot_fc_dataB$mat, 1, sd)

        df2 <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                          sd = std, label = fc_uncertA)

        if(is.null(quantfcB$df)) {
          quantfcB$df <- df2
        } else {
          # Overwrite previous Std Dev.
          if((df2$label[1] %in% quantfcB$df$label)) {
            idx <- which(quantfcB$df$label %in% df2$label[1])
            quantfcB$df[idx, ] <- df2
          } else {
            quantfcB$df <- rbind(quantfcB$df, df2)
          }
        }
      }
      progress$set(value = (pidx / length(uc_sources)))
    }
  })

  #* Quantify Forecast UC B ----
  quantfcB <- reactiveValues(df = NULL)
  observeEvent(input$quant_ucB, {
    req(input$table01_rows_selected != "")

    # std <- apply(tot_fc_dataB$mat, 1, sd)
    #
    # df <- data.frame(Date = tot_fc_dataB$dist$Date,
    #                  sd = std, label = tot_fc_dataB$lab)
    #
    # if(is.null(quantfcB$df)) {
    #   quantfcB$df <- df
    # } else {
    #   # Overwrite previous Std Dev.
    #   if((df$label[1] %in% quantfcB$df$label)) {
    #     idx <- which(quantfcB$df$label %in% df$label[1])
    #     quantfcB$df[idx, ] <- df
    #   } else {
    #     quantfcB$df <- rbind(quantfcB$df, df)
    #   }
    # }
    # print(quantfcB$df)
  })

  output$fc_quantB <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(quantfcB$df), "Click 'Run forecast' above.")
    )
    validate(
      need(input$quant_ucB > 0, "Click 'Quantify uncertainty'")
    )

    p <- ggplot() +
      geom_bar(data = quantfcB$df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
      ylab("Standard Deviation (\u00B0C)") +
      scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                   "Driver" = cols2[4], "Total" = cols2[5])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(fill = "Uncertainty") +
      theme_bw(base_size = 12)

    gp <- ggplotly(p) #, dynamicTicks = TRUE)
    return(gp)
  })

  #####

  #** Scenario Plots ----

  output$scen1_plot <- renderPlot({

    p <- ggplot(scen_fc1) +
      geom_hline(yintercept = 12, linetype = "dashed") +
      geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
      geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
      geom_line(aes(Date, surftemp, color = "Surface")) +
      geom_line(aes(Date, bottemp, color = "Bottom")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Day") +
      guides(color = "none") +
      labs(fill = "Location") +
      scale_x_date(breaks = "1 day", date_labels = "%a") +
      scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
      scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
      # scale_fill_discrete(breaks = c("Surface", "Bottom")) +
      coord_cartesian(ylim = c(8, 14)) +
      theme_bw(base_size = 22)
    return(p)

    # gp <- ggplotly(p, dynamicTicks = TRUE)
    # # Code to remove parentheses in plotly
    # for (i in 1:length(gp$x$data)){
    #   if (!is.null(gp$x$data[[i]]$name)){
    #     gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
    #   }
    # }
    # return(gp)
  })

  output$scen2_plot <- renderPlot({

    validate(
      need(input$scen1_dec > 0, "Complete Decision #1 above.")
    )

    p <- ggplot(scen_fc2) +
      geom_hline(yintercept = 12, linetype = "dashed") +
      geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
      geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
      geom_line(aes(Date, surftemp, color = "Surface")) +
      geom_line(aes(Date, bottemp, color = "Bottom")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Day") +
      guides(color = "none") +
      labs(fill = "Location") +
      scale_x_date(breaks = "1 day", date_labels = "%a") +
      scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
      scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
      coord_cartesian(ylim = c(8, 14)) +
      theme_bw(base_size = 22)
    return(p)

    # gp <- ggplotly(p, dynamicTicks = TRUE)
    # # Code to remove parentheses in plotly
    # for (i in 1:length(gp$x$data)){
    #   if (!is.null(gp$x$data[[i]]$name)){
    #     gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
    #   }
    # }
    # return(gp)
  })

  observeEvent(input$scen1_dec, {
    shinyjs::disable("dec_scen1")
    shinyjs::disable("scen1_dec")

    shinyjs::enable("dec_scen2")
    shinyjs::enable("scen2_dec")
  })

  observe({
    if(input$scen1_dec < 1) {
      shinyjs::disable("dec_scen2")
      shinyjs::disable("scen2_dec")
    }
  })

  observeEvent(input$scen2_dec, {
    shinyjs::disable("dec_scen2")
    shinyjs::disable("scen2_dec")
  })



  #** Load NOAA airT ----
  noaa_df <- reactiveValues(airt = NULL, swr = NULL)
  observeEvent(input$load_noaa_at, {

    req(input$table01_rows_selected != "")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Loading NOAA forecast data"),
                 detail = "This may take a while. This window will disappear
                     when it is finished loading.", value = 0.1)

    fpath <- file.path("data", "NOAAGEFS_1hr", siteID$lab)
    fc_date <- list.files(fpath)[1]
    fpath2 <- file.path(fpath, fc_date[1], "00")
    fils <- list.files(fpath2, full.names = TRUE)
    fils <- fils[-c(grep("ens00", fils))]
    fid <- ncdf4::nc_open(file.path(fils[1]))
    vars <- fid$var # Extract variable names for selection
    fc_vars <- names(vars)[c(1, 5)] # Extract air temp
    membs <- length(fils)
    ncdf4::nc_close(fid)

    out <- lapply(fc_date, function(dat) {
      idx <- which(fc_date == dat)

      fpath2 <- file.path(fpath, dat, "00")
      fils <- list.files(fpath2)
      fils <- fils[-c(grep("ens00", fils))]

      for( i in seq_len(length(fils))) {

        fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID$lab, dat,
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
        cnam <- paste0("mem", formatC(i, width = 2, format = "d", flag = "0"))
        if(i == 1) {
          df2 <- mlt1
          colnames(df2)[3] <- cnam
        } else {
          df2 <- merge(df2, mlt1, by = c(1,2))
          colnames(df2)[ncol(df2)] <- cnam
        }

      }
      progress$set(value = i/length(fils))
      return(df2)
    })

    names(out) <- fc_date
    # out$`2020-09-25`$L1 <- "Air temperature"
    idx <- which(met_pars$Site == siteID$lab)

    noaa_df$airt <- reshape::melt(out[[1]][out[[1]]$L1 == "air_temperature", ], id.vars = c("time", "L1"))
    noaa_df$swr <- reshape::melt(out[[1]][out[[1]]$L1 == "surface_downwelling_shortwave_flux_in_air", ], id.vars = c("time", "L1"))
    noaa_df$swt <- noaa_df$airt
    noaa_df$swt$L1 <- "surface_water_temperature"
    noaa_df$swt$value <- met_pars$airt_m[idx] * noaa_df$swt$value + met_pars$airt_b[idx]
    noaa_df$upar <- noaa_df$swr
    noaa_df$upar$L1 <- "underwater_photosynthetically_active_radiation"
    noaa_df$upar$value <- met_pars$swr_m[idx] * noaa_df$upar$value + met_pars$swr_b[idx]

    mlt <- noaa_df$airt
    mlt$Date <- as.Date(mlt$time)
    mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
    mlt <- mlt[mlt$Date <= "2020-10-02", ]

    wid <- tidyr::pivot_wider(mlt, c(Date, L1), names_from = variable, values_from = value)
    wid <- as.data.frame(wid)

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)

    dat$wtemp_yday[-c(1:1)] <- dat$wtemp[-c((nrow(dat)+1-1):nrow(dat))]
    dat$airt_yday[-c(1:1)] <- dat$airt[-c((nrow(dat)+1-1):nrow(dat))]

    lag_date <- (as.Date(fc_date) + 1)
    mn_date <- (as.Date(fc_date) + 1)

    wtemp_fc_data5$lst <- lapply(1:input$noaa_n_mems, function(x) {
      dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
      dat$wtemp[dat$Date > fc_date] <- NA
      dat$forecast <- NA
      dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
      dat$airt[dat$Date > fc_date] <- wid[2:8, x+2]
      dat$wtemp_yday[dat$Date > lag_date] <- NA
      dat$airt_yday[dat$Date > mn_date] <- NA

      df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
      df <- merge(dat, df, by = "Date", all.y = TRUE)
    })

  })

  output$noaa_at_loaded <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt), "Please click 'Load forecast'")
    )
    return(paste0("Forecast loaded for ", siteID$lab))
  })

  #** NOAA Air temperature plot ----
  output$noaa_at_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt), "Please click 'Load forecast'")
    )
    validate(
      need(is.numeric(input$noaa_n_mems), "Please input a number for 'Number of forecasts'")
    )
    # validate(
    #   need(input$view_day0, "Please click 'Load observation'")
    # )
    set.seed(123)


    mlt <- noaa_df$airt

    if(input$noaa_timestep == "Daily mean") {
      mlt$date <- as.Date(mlt$time)
      mlt <- plyr::ddply(mlt, c("date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
      mlt$time <- as.POSIXct(mlt$date)
      fut_offset <- lubridate::days(6) #+ lubridate::hours(19)
    } else if(input$noaa_timestep == "Hourly") {
      fut_offset <- lubridate::days(6) + lubridate::hours(19)
    }

    idx1 <- which(mlt$time == mlt$time[1])
    dat <- mlt[idx1, ]
    dat$time2 <- dat$time + rnorm(nrow(dat), mean = 0, sd = 6000)
    idx2 <- which(mlt$time == (mlt$time[1] + fut_offset))
    dat2 <- mlt[idx2, ]
    dat2$time2 <- dat2$time + rnorm(nrow(dat2), mean = 0, sd = 6000)

    st <- data.frame(time = c(mlt$time[1], (mlt$time[1] + fut_offset)),
                     mean = c(mean(dat$value, na.rm = TRUE), mean(dat2$value, na.rm = TRUE)),
                     sd = c(sd(dat$value, na.rm = TRUE), sd(dat2$value, na.rm = TRUE)),
                     null = rep(mean(dat$value, na.rm = TRUE), 2),
                     L1 = dat$L1[1])

    if(input$view_day7) {
      ylims <- c(min(mlt$value, na.rm = TRUE) - 1, max(mlt$value, na.rm = TRUE) + 1)
      xlims <- c(mlt$time[1], (mlt$time[1] + fut_offset))
    } else {
      xlims <- c((mlt$time[1] - lubridate::hours(12)), (mlt$time[1] + lubridate::hours(12)))
      ylims <- c(min(dat$value, na.rm = TRUE) - 1, max(dat$value, na.rm = TRUE) + 1)
    }

    dat <- dat[dat$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]
    dat2 <- dat2[dat2$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]

    if(input$noaa_n_mems > 0 & input$view_day7) {
      cur_df <- data.frame(time = dat$time2, value = dat$value, variable = dat$variable, xend = dat2$time2, yend = dat2$value)
    }


    ylab <- "Temperature (\u00B0C)"
    xlab <- "Time"

    p <- ggplot() +
      # geom_point(data = dat2, aes(time2, value)) + # Day 7
      # geom_line(data = mlt[mlt$time <= xlims[2], ], aes(time, value, group = variable)) +
      # geom_errorbar(data = st, aes(time, ymin = mean - sd, ymax = mean + sd),
      #               color = "red", width = 12000, size = 2) + # Error bars - Std. Dev
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      # stat_ellipse(data = dat2, aes(time2, value), na.rm = TRUE, inherit.aes = FALSE) +
      # facet_wrap(~L1, scales = "free_y", ncol = 1) +
      ylab(ylab) +
      xlab(xlab) +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_bw(base_size = 18)

    if(input$view_day0) {
      p <- p +
        geom_point(data = st[1, ], aes(time, mean), color = "red", size = 5) # Mean
    }

    if(input$add_obs_uc) {
      p <- p + geom_errorbar(data = st[1, ], aes(time, ymin = mean - sd, ymax = mean + sd),
                        color = "red", width = 12000, size = 2) # Error bars - Std. Dev

    }
    if(input$view_ic & input$noaa_n_mems > 0) {
      p <- p +
        geom_point(data = dat, aes(time2, value), size = 2) + # Day 1
        # geom_ellipse(aes(x0 = (st$time[1] - 6000), y0 = st$mean[1], a = (2 * 6000), b = (2 * st$sd[1]), angle = 0))
        stat_ellipse(data = dat, aes(time2, value), na.rm = TRUE,
                     inherit.aes = FALSE, type = "norm")
    }
    if(input$view_day7 & input$noaa_n_mems > 0) {
      p <- p +
        geom_point(data = dat2, aes(time2, value), size = 2) + # Day 1
        stat_ellipse(data = dat2, aes(time2, value), na.rm = TRUE,
                     inherit.aes = FALSE, type = "norm")
    }

    if(!is.null(input$add_to_plot)) {

      if(input$view_ic & input$noaa_n_mems > 0 & input$view_day7 & input$add_to_plot == "Line") {
        p <- p +
          geom_segment(data = cur_df, aes(time, value, group = variable, xend = xend, yend = yend),
                       color = "gray", alpha = 0.6)
        # geom_curve(data = cur_df, aes(time, value, group = variable, xend = xend, yend = yend),
        #            curvature = -0.2) # Connecting curves
      }
      if(input$view_ic & input$noaa_n_mems > 0 & input$view_day7 & input$add_to_plot == "Forecast members") {

        mlt <- mlt[mlt$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]
        p <- p +
          geom_line(data = mlt[mlt$time <= xlims[2], ], aes(time, value, group = variable),
                    color = "gray", alpha = 0.6)
      }
      if(input$view_ic & input$noaa_n_mems > 0 & input$view_day7 & input$add_to_plot == "Forecast distribution") {

        validate(
          need(input$noaa_n_mems >= 2, "Number of members must be greater than 1.")
        )

        wid <- tidyr::pivot_wider(mlt, c(time, L1), names_from = variable, values_from = value)
        wid <- wid[wid$time <= (mlt$time[1] + fut_offset), 1:(input$noaa_n_mems + 2)]
        df <- apply(wid[, -c(1, 2)], 1, function(x){
          quantile(x, c(0.05, 0.5, 0.95))
        })
        df <- as.data.frame(t(df))
        colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
        df$time <- wid$time
        p <- p +
          geom_ribbon(data = df, aes(time, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3)+
          # geom_ribbon(data = df, aes(time, ymin = p12.5, ymax = p87.5, fill = "75%"), alpha = 0.3)+
          geom_line(data = df, aes(time, p50, color = "Median"))
      }
    }

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    # return(p)

    return(gp)
  })

  # Run chl-A forecasts with IC UC ----
  # Generate Initial condition distribution plots
  ic_dist_plot <- reactiveValues(phy = NULL, nut = NULL, phy_xlims = NULL, nut_xlims = NULL)
  observeEvent(input$gen_ic_dist, {
    req(!is.null(input$n_samp_ic))
    ic_dist_plot$phy <- rnorm(input$n_samp_ic, mean = input$phy_ic_value, sd = input$phy_ic_sd)
    ic_dist_plot$phy[ic_dist_plot$phy <= 0] <- 0.01
    ic_dist_plot$nut <- rnorm(input$n_samp_ic, mean = input$nut_ic_value, sd = input$nut_ic_sd)
    ic_dist_plot$nut[ic_dist_plot$nut <= 0] <- 0.01
    ic_dist_plot$phy_xlims <- c(input$phy_ic_value - 3 * input$phy_ic_sd, input$phy_ic_value + 3 * input$phy_ic_sd)
    ic_dist_plot$nut_xlims <- c(input$nut_ic_value - 3 * input$nut_ic_sd, input$nut_ic_value + 3 * input$nut_ic_sd)
    ic_dist_plot$phy_vline <- input$phy_ic_value
    ic_dist_plot$nut_vline <- input$nut_ic_value
  })

  output$ic_phy_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$n_samp_ic),
           message = "Please select a number of samples.")
    )
    validate(
      need(input$gen_ic_dist > 0, "Click 'Generate initial condition distributions'")
    )
    df <- data.frame(par = "Chlorophyll-a (μg/L)", value = ic_dist_plot$phy)

    p <- ggplot(df) +
      geom_vline(xintercept = ic_dist_plot$phy_vline) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = ic_dist_plot$phy_xlims, ylim = c(0, 5)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Chlorophyll-a (μg/L)") +
      theme_bw(base_size = 22)
    return(p)
  })
  output$ic_nut_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$n_samp_ic),
           message = "Please select a number of samples.")
    )
    validate(
      need(input$gen_ic_dist > 0, "Click 'Generate initial condition distributions'")
    )
    df <- data.frame(par = "Nutrients (mg/L)", value = ic_dist_plot$nut)

    p <- ggplot(df) +
      geom_vline(xintercept = ic_dist_plot$nut_vline) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = ic_dist_plot$nut_xlims, ylim = c(0, 5)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Nutrient (mg/L)") +
      theme_bw(base_size = 22)
    return(p)
  })

  # Run IC FC ----
  ic_fc_data <- reactiveValues(chla = NULL, nut = NULL)
  observeEvent(input$run_ic_fc, {

    req(!is.null(noaa_df$airt))

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model with ", length(ic_dist_plot$phy), " different sets of initial conditions."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running", value = 0.01)

    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    n_mem <- length(ic_dist_plot$phy)
    arr <- array(NA, dim = c(8, 3, n_mem))
    for(mem in 1:n_mem) {
      res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
      res[1, 1] <- ic_dist_plot$phy[mem] * 0.016129 # Convert from μg/L to mmolN/m3
      res[1, 2] <- ic_dist_plot$nut[mem] * 16.129 # Convert from mg/L to mmolN/m3
      res[1, 3] <- res[1, 1]  * 62
      for(i in 2:8) {

        out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
        res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                      (res[i-1, 2] + out[[1]][2]),
                      (res[i-1, 1] + out[[1]][1]) * 62)

      }
      arr[, , mem] <- res
      progress$set(value = mem/n_mem)
    }

    mlt <- reshape2::melt(arr[, 3, ])
    mlt$date <- np_inp$date[1:8]
    ic_fc_data$chla <- mlt

    mlt <- reshape2::melt(arr[, 2, ])
    mlt$date <- np_inp$date[1:8]
    mlt$value <- mlt$value / 16.129
    ic_fc_data$nut <- mlt
  })

  output$ic_fc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(ic_dist_plot$phy),
           "Please generate distributions of initial conditions required")
    )
    validate(
      need(!is.null(input$ic_fc_type),
           "Please select a type of plot")
    )
    validate(
      need(!is.null(ic_fc_data$chla),
           message = "Click 'Run forecast'")
    )

    dat <- ic_fc_data$chla

    p <- ggplot()

    if(input$ic_fc_type == "Distribution") {
      wid <- tidyr::pivot_wider(dat, c(date), names_from = Var2, values_from = value)
      df <- apply(wid[, -c(1)], 1, function(x){
        quantile(x, c(0.05, 0.5, 0.95))
      })
      df <- as.data.frame(t(df))
      colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
      df$time <- wid$date
      p <- p +
        geom_ribbon(data = df, aes(time, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3)+
        # geom_ribbon(data = df, aes(time, ymin = p12.5, ymax = p87.5, fill = "75%"), alpha = 0.3)+
        geom_line(data = df, aes(time, p50, color = "Median"))
      } else if(input$ic_fc_type == "Line") {
      p <- p +
        geom_line(data = dat, aes_string("date", "value", group = "Var2"),
                  color = "gray", alpha = 0.6)
      }
    p <- p +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_bw(base_size = 18)

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
        }
      }
    return(gp)
    })

  # Model Uncertainty ----
  #* Process Uncertainty ----
  mod0_runs <- reactiveValues(curr = NULL, prev = NULL,  prev2 = NULL,
                              none = NULL, low = NULL, med = NULL, high = NULL)
  observeEvent(input$run_mod0, {

    req(input$table01_rows_selected != "")
    req(!is.null(noaa_df$airt))

    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    parms[1] <- 0.15
    parms[7] <- 0.85
    if(input$proc_uc0 == "None") {
      w_sd <- 0
    } else if(input$proc_uc0 == "Low") {
      w_sd <- 0.01
    } else if(input$proc_uc0 == "Medium") {
      w_sd <- 0.025
    } else if(input$proc_uc0 == "High") {
      w_sd <- 0.05
    }
    sig_w <- rnorm(8, mean = 0, sd = w_sd)

    res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
    res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
    res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
    res[1, 3] <- res[1, 1]  * 62
    for(i in 2:8) {

      out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
      new_phy <- res[i-1, 1] + out[[1]][1] + sig_w[i]
      new_nut <- res[i-1, 2] + out[[1]][2]
      if(new_phy <= 0) {
        new_phy <- 0.01
      }
      if(new_nut <= 0) {
        new_nut <- 1
      }
      res[i, ] <- c(new_phy,
                    new_nut,
                    (new_phy * 62))
    }



    res <- as.data.frame(res)
    res$date <- np_inp[1:8, 1]
    if(input$proc_uc0 == "None") {
      mod0_runs$none <- res
    } else if(input$proc_uc0 == "Low") {
      mod0_runs$low <- res
    } else if(input$proc_uc0 == "Medium") {
      mod0_runs$med <- res
    } else if(input$proc_uc0 == "High") {
      mod0_runs$high <- res
    }

    mod0_runs$curr <- res
  })

  output$proc_uc_plot <- renderPlot({


    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    # validate(
    #   need(input$proc_uc0 != "None",
    #        message = "Add process uncertainty")
    # )

    xlims <- c((0 - 3*0.05), (0 + 3*0.05))
    ylims <- c(0, 40)
    df <- data.frame(Low = rnorm(1000, mean = 0, sd = 0.01),
                     Medium = rnorm(1000, mean = 0, sd = 0.025),
                     High = rnorm(1000, mean = 0, sd = 0.05))

    p <- ggplot(df) +
      geom_vline(xintercept = 0) +
      geom_density(aes(x = Low, fill = "Low"), alpha = 0.6) +
      geom_density(aes(x = Medium, fill = "Medium"), alpha = 0.6) +
      geom_density(aes(x = High, fill = "High"), alpha = 0.6) +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      scale_fill_manual(values = c("Low" = cols[2], "Medium" = cols[3], "High" = cols[4])) +
      guides(fill = guide_legend(override.aes = list(alpha = 0.6))) +
      labs(fill = "Level") +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Process Uncertainty") +
      theme_bw(base_size = 22)
    return(p)
    })

  output$run_mod0_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(mod0_runs$curr),
           message = "Click 'Run model'")
    )

    p <- ggplot()

    if(!is.null(mod0_runs$none)) {
      p <- p +
        geom_line(data = mod0_runs$none, aes(date, Chla, color = "None"),
                  linetype = "solid")
    }
    if(!is.null(mod0_runs$low)) {
      p <- p +
        geom_line(data = mod0_runs$low, aes(date, Chla, color = "Low"),
                  linetype = "twodash")
    }
    if(!is.null(mod0_runs$med)) {
      p <- p +
        geom_line(data = mod0_runs$med, aes(date, Chla, color = "Medium"),
                  linetype = "dotted")
    }
    if(!is.null(mod0_runs$high)) {
      p <- p +
        geom_line(data = mod0_runs$high, aes(date, Chla, color = "High"),
                  linetype = "dashed")
    }

    p <- p +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      scale_color_manual(values = c("None" = cols[1], "Low" = cols[2], "Medium" = cols[3], "High" = cols[4])) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      labs(color = "Process uncertainty") +
      theme_bw(base_size = 18)

    return(ggplotly(p, dynamicTicks = TRUE))

  })


  #* Run model & explore parameters

  mod1_runs <- reactiveValues(curr = NULL, prev = NULL,  prev2 = NULL,
                              pars_df = data.frame(matrix(NA, nrow = 3, ncol = 3, dimnames = list(c("Current run", "Previous run", "Previous run2"), c("Mortality rate", "Nutrient uptake", "Reference temperature")))),
                              pars_curr = NULL, pars_prev = NULL, pars_prev2 = NULL)
  observeEvent(input$run_mod1, {

    req(input$table01_rows_selected != "")
    req(!is.null(noaa_df$airt))

    mod1_runs$prev2 <- mod1_runs$prev
    mod1_runs$prev <- mod1_runs$curr

    mod1_runs$pars_df[3, ] <- mod1_runs$pars_df[2, ]
    mod1_runs$pars_df[2, ] <- mod1_runs$pars_df[1, ]
    mod1_runs$pars_df[1, ] <- c(input$mort_rate1, input$nut_uptake1, input$refTEMP1)


    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    parms[1] <- input$nut_uptake1
    parms[7] <- input$mort_rate1
    parms[12] <- input$refTEMP1

    res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
    res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
    res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
    res[1, 3] <- res[1, 1]  * 62
    for(i in 2:8) {

      out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
      res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                    (res[i-1, 2] + out[[1]][2]),
                    (res[i-1, 1] + out[[1]][1]) * 62)

    }

    res <- as.data.frame(res)
    res$date <- np_inp[1:8, 1]

    mod1_runs$curr <- res
    mod1_runs$pars_curr <- c(parms[7], parms[1], parms[12])
  })

  output$run_mod1_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(mod1_runs$curr),
           message = "Click 'Run model'")
    )

    p <- ggplot()

    if(!is.null(mod1_runs$prev2)) {
      p <- p +
        geom_line(data = mod1_runs$prev2, aes(date, Chla, color = "Previous run2"),
                  linetype = "dotted")
    }
    if(!is.null(mod1_runs$prev)) {
      p <- p +
        geom_line(data = mod1_runs$prev, aes(date, Chla, color = "Previous run"),
                  linetype = "dashed")
    }

    p <- p +
      geom_line(data = mod1_runs$curr, aes(date, Chla, color = "Current run")) +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_bw(base_size = 18)

    return(ggplotly(p, dynamicTicks = TRUE))

  })

  #** Data table of used parameters ----
  output$run_mod1_pars <- renderDT(
    mod1_runs$pars_df, rownames = TRUE, options = list(ordering = FALSE, dom = 't'),
    colnames = c("Mortality rate", "Nutrient uptake", "Reference temperature")
  )

  #* Generate parameters ----
  pars_dist <- reactiveValues(mort_rate = NULL, nut_uptake = NULL, mort_xlims = NULL, nut_xlims = NULL)
  plot_switch <- reactiveValues(pars_UC = FALSE)
  observeEvent(input$gen_param_dist, {

    req(!is.null(input$n_samp_pars))
    if(input$add_nut_uc) {
      pars_dist$nut_uptake <- data.frame(value = rnorm(input$n_samp_pars, mean = input$nut_uptake2, sd = input$nut_uptake2_sd),
                                         par = "Nutrient uptake")
      pars_dist$nut_uptake[pars_dist$nut_uptake <= 0] <- 0.01
      pars_dist$nut_uptake[pars_dist$nut_uptake >= 1] <- 0.99
      pars_dist$nut_uptake_xlims <- c(input$nut_uptake2 - 3 * input$nut_uptake2_sd, input$nut_uptake2 + 3 * input$nut_uptake2_sd)
      pars_dist$nut_uptake_vline <- input$nut_uptake2
    }

    if(input$add_mort_uc) {
      pars_dist$mort_rate <- data.frame(value = rnorm(input$n_samp_pars, mean = input$mort_rate2, sd = input$mort_rate2_sd), par =  "Mortality rate")
      pars_dist$mort_rate[pars_dist$mort_rate <= 0] <- 0.01
      pars_dist$mort_rate[pars_dist$mort_rate >= 1] <- 0.99
      pars_dist$mort_rate_xlims <- c(input$mort_rate2 - 3 * input$mort_rate2_sd, input$mort_rate2 + 3 * input$mort_rate2_sd)
      pars_dist$mort_rate_vline <- input$mort_rate2
    }
    plot_switch$pars_UC <- FALSE
  })

  #** Mortality rate distribution plot ----
  output$mort_rate_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$add_mort_uc, "Check 'Add uncertainty for mortality rate'")
    )
    validate(
      need(!is.null(input$n_samp_pars), "Select the number of samples")
    )
    validate(
      need(!is.null(pars_dist$mort_rate), "Click 'Generate parameter distributions'")
    )

    df <- pars_dist$mort_rate

    p <- ggplot(df) +
      geom_vline(xintercept = pars_dist$mort_rate_vline) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = c(0, 1)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Mortality rate") +
      theme_bw(base_size = 22)
    return(p)
  })

  #** Nutrient uptake distribution plot ----
  output$nut_uptake_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$add_nut_uc, "Check 'Add uncertainty for nutrient uptake'")
    )
    validate(
      need(!is.null(pars_dist$nut_uptake), "Click 'Generate parameter distributions'")
    )

    df <- pars_dist$nut_uptake

    p <- ggplot(df) +
      geom_vline(xintercept = pars_dist$nut_uptake_vline, size = l_siz) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = c(0, 1)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Nutrient uptake") +
      theme_bw(base_size = 22)
    return(p)
  })


  # Run Param UC FC ----
  pars_fc_data <- reactiveValues(chla = NULL, nut = NULL)
  observeEvent(input$run_pars_fc, {

    req(!is.null(noaa_df$airt))
    req(!is.null(input$pars_fc_type))
    plot_switch$pars_UC <- TRUE

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model with ", input$n_samp_pars, " different sets of parameters."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running", value = 0.01)

    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    n_mem <- nrow(pars_dist$mort_rate)
    arr <- array(NA, dim = c(8, 3, n_mem))
    for(mem in 1:n_mem) {

      if(input$add_mort_uc) {
        parms[7] <- pars_dist$mort_rate$value[mem]
      }
      if(input$add_nut_uc) {
        parms[1] <- pars_dist$nut_uptake$value[mem]
      }

      res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
      res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
      res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
      res[1, 3] <- res[1, 1]  * 62

      for(i in 2:8) {
        out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
        res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                      (res[i-1, 2] + out[[1]][2]),
                      (res[i-1, 1] + out[[1]][1]) * 62)
      }
      res[res[, 3] > 50 | res[, 3] < 0, 3] <- NA # Reset outlier values
      arr[, , mem] <- res
      progress$set(value = mem/n_mem)
    }

    mlt <- reshape2::melt(arr[, 3, ])
    mlt$date <- np_inp$date[1:8]
    pars_fc_data$chla <- mlt

    mlt <- reshape2::melt(arr[, 2, ])
    mlt$date <- np_inp$date[1:8]
    mlt$value <- mlt$value / 16.129
    pars_fc_data$nut <- mlt
  })

  output$pars_fc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(pars_dist$mort_rate),
           "Please generate distributions of parameters required")
    )
    validate(
      need(!is.null(input$pars_fc_type),
           message = "Select a type of plot")
    )
    validate(
      need(plot_switch$pars_UC,
           message = "Click 'Run forecast'")
    )

    dat <- pars_fc_data$chla

    p <- ggplot()

    if(input$pars_fc_type == "Distribution") {
      wid <- tidyr::pivot_wider(dat, c(date), names_from = Var2, values_from = value)
      df <- apply(wid[, -c(1)], 1, function(x){
        quantile(x, c(0.05, 0.125, 0.5, 0.875, 0.95), na.rm = TRUE)
      })
      df <- as.data.frame(t(df))
      colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
      df$time <- wid$date
      p <- p +
        geom_ribbon(data = df, aes(time, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3)+
        # geom_ribbon(data = df, aes(time, ymin = p12.5, ymax = p87.5, fill = "75%"), alpha = 0.3)+
        geom_line(data = df, aes(time, p50, color = "Median"))
    } else if(input$pars_fc_type == "Line") {
      p <- p +
        geom_line(data = dat, aes_string("date", "value", group = "Var2"),
                  color = "gray", alpha = 0.6)
    }
    p <- p +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_bw(base_size = 18)

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  # Run Driver UC FC ----
  driv_fc_data0 <- reactiveValues(chla = NULL, nut = NULL)
  rand_samp <- reactiveValues(val = NULL)
  observeEvent(input$run_driv_fc0, {

    req(!is.null(noaa_df$airt))
    # plot_switch$pars_UC <- TRUE

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running", value = 0.01)

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.12, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.85, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    n_mem <- 30
    arr <- array(NA, dim = c(8, 3, n_mem))
    for(mem in 1:n_mem) {

      sel_mem <- paste0("mem", formatC(mem, width = 2, format = "d", flag = "0"))

      swt <- noaa_df$swt
      swt <- swt[swt$variable == sel_mem, ]
      swt$date <- as.Date(swt$time)
      swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

      upar <- noaa_df$upar
      upar <- upar[upar$variable == sel_mem, ]
      upar$date <- as.Date(upar$time)
      upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

      np_inp <- merge(swt_dly, upar_dly, by = 1)
      np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
      times <- 1:nrow(np_inp)

      np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

      res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
      res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
      res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
      res[1, 3] <- res[1, 1]  * 62

      for(i in 2:8) {
        out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
        res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                      (res[i-1, 2] + out[[1]][2]),
                      (res[i-1, 1] + out[[1]][1]) * 62)
      }
      # res[res[, 3] > 50 | res[, 3] < 0, 3] <- NA # Reset outlier values
      arr[, , mem] <- res
    }

    mlt <- reshape2::melt(arr[, 3, ])
    mlt$date <- np_inp$date[1:8]
    driv_fc_data0$chla <- mlt

    mlt <- reshape2::melt(arr[, 2, ])
    mlt$date <- np_inp$date[1:8]
    mlt$value <- mlt$value / 16.129
    driv_fc_data0$nut <- mlt

    rand_samp$val <- sample(1:30, 1)

    progress$set(value = 1)
  })

  output$driv_fc_plot0 <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(driv_fc_data0$chla),
           message = "Click 'Run forecast'")
    )

    dat <- driv_fc_data0$chla
    dat$label <- paste0("mem", formatC((dat$Var2), width = 2, format = "d", flag = "0"))
    ylims <- range(dat$value, na.rm = TRUE)
    if(input$add_mem > 0) {
      add_dat <- dat[dat$Var2 %in% c(2:(input$add_mem + 1)), ]
    }
    dat <- dat[dat$Var2 == 1, ]
    col30 <- cols

    p <- ggplot() +
      geom_line(data = dat, aes(date, value, color = label)) +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      coord_cartesian(ylim = ylims) +
      theme_bw(base_size = 18)

    if(input$add_mem > 0) {
      p <- p +
        geom_line(data = add_dat, aes(date, value, color = label))
    }
    if(input$add_mem > 7) {
      col30 <- c(rep("black", input$add_mem - 7), cols)
    }
    p <- p + scale_color_manual(values = col30) +
      guides(color = "none")

    return(p)
  })

  output$driv_fc_plot1 <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(driv_fc_data0$chla),
           message = "Click 'Run forecast'")
    )

    swt <- noaa_df$swt
    # swt <- swt[swt$variable == sel_mem, ]
    swt$date <- as.Date(swt$time)
    dat <- plyr::ddply(swt, c("date", "variable"), function(x) {
      data.frame(value = mean(x$value, na.rm = TRUE))
    })

    sub_dates <- unique(dat$date)[1:8]

    ylims <- range(dat$value, na.rm = TRUE)
    if(input$add_mem > 0) {
      add_dat <- dat[dat$variable %in% paste0("mem", formatC((2:(input$add_mem + 1)), width = 2, format = "d", flag = "0")) & dat$date %in% sub_dates, ]
      }
    dat <- dat[dat$variable == "mem01" & dat$date %in% sub_dates, ]
    col30 <- cols


    p <- ggplot() +
      geom_line(data = dat, aes(date, value, color = variable)) +
      scale_x_date(date_labels = "%a", date_breaks = "1 day") +
      ylab("Water temperature (\u00B0C)") +
      xlab("Time") +
      coord_cartesian(ylim = ylims) +
      theme_bw(base_size = 18)

    if(input$add_mem > 0) {
      p <- p +
        geom_line(data = add_dat, aes(date, value, color = variable))
    }
    if(input$add_mem > 7) {
      col30 <- c(rep("black", input$add_mem - 7), cols)
    }
    p <- p + scale_color_manual(values = col30) +
      guides(color = "none")

    return(p)
  })

  # Navigating Tabs ----
  #* Main Tab ====
  rv1 <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$maintab, {
    curr_tab1 <- input$maintab
    rv1$prev <- readr::parse_number(curr_tab1) - 1
    rv1$nxt <- readr::parse_number(curr_tab1) + 1
  })

  observe({
    toggleState(id = "prevBtn1", condition = rv1$prev > 0)
    if(rv1$nxt > 7 & rv4a$nxt > 14) {
      shinyjs::disable("nextBtn1")
    } else {
      shinyjs::enable("nextBtn1")
    }
    hide(selector = ".page")
    # show(paste0("mtab", rv1$nxt))
  })


  # Next button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if(curr_tab1 == "mtab7") {
      curr_obj <- input$tabseries4
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if(curr_tab1 == "mtab7" & rv4a$nxt > 14) {
      updateActionButton(session, inputId = "nextBtn1", label = paste("Next >"))
    } else {
      updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
    }
  })

  # Previous button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx - 1]

    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj1") idx2 <- idx2 - 1 # Move off Activity A label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj3") idx2 <- idx2 - 1 # Move off Activity B label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj6") idx2 <- idx2 - 1 # Move off Activity C label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if(curr_tab1 == "mtab7") {
      curr_obj <- input$tabseries4
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj12") idx2 <- idx2 - 1 # Move off Activity C label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if(curr_tab1 == "mtab1") {
      updateActionButton(session, inputId = "prevBtn1", label = paste("< Previous"))
    } else {
      # shinyjs::show(id = "prevBtn1")
      updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
    }
  })


  # Advancing Tabs
  observeEvent(input$nextBtn1, {

    if(input$nextBtn1 %in% c(5, 9, 15)) {
      showModal(
        modalDialog(
          title = "Save Progress",
          "Don't forget to save your progress as you go just in case you lose connection with the server. Click 'Download user input' at the bottom of the page to save a snapshot of your answers so far.")
      )
    } else {
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      if (curr_tab1 == "mtab4" & rv1a$nxt < 3) {
        curr_obj <- input$tabseries1

        updateTabsetPanel(session, "tabseries1",
                          selected = paste0("obj", rv1a$nxt))

      } else if (curr_tab1 == "mtab5" & rv2a$nxt < 6) {
        curr_obj <- input$tabseries2

        updateTabsetPanel(session, "tabseries2",
                          selected = paste0("obj", rv2a$nxt))

      } else if (curr_tab1 == "mtab6" & rv3a$nxt < 12) {
        curr_obj <- input$tabseries3
        updateTabsetPanel(session, "tabseries3",
                          selected = paste0("obj", rv3a$nxt))
      } else if (curr_tab1 == "mtab7" & rv4a$nxt < 15) {
        curr_obj <- input$tabseries4
        updateTabsetPanel(session, "tabseries4",
                          selected = paste0("obj", rv4a$nxt))
      } else {
        updateTabsetPanel(session, "tabseries1",
                          selected = "obj1")
        updateTabsetPanel(session, "tabseries2",
                          selected = "obj3")
        updateTabsetPanel(session, "tabseries3",
                          selected = "obj6")
        updateTabsetPanel(session, "tabseries4",
                          selected = "obj12")
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$nxt))
      }
      shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
    }
  })

  # Moving back through tabs
  observeEvent(input$prevBtn1, {
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$prev))

    } else if (curr_tab1 == "mtab5" & rv2a$prev > 2) {
      curr_obj <- input$tabseries2

      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$prev))

    } else if (curr_tab1 == "mtab6" & rv3a$prev > 5) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$prev))
    } else if (curr_tab1 == "mtab7" & rv4a$prev > 11) {
      curr_obj <- input$tabseries4
      updateTabsetPanel(session, "tabseries4",
                        selected = paste0("obj", rv4a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj2")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj5")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj11")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$prev))
    }
    shinyjs::runjs("window.scrollTo(0, 0)")

  })

  #* Tab 1a ----
  rv1a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries1, {
    curr_tab1 <- input$tabseries1
    rv1a$prev <- readr::parse_number(curr_tab1) - 1
    rv1a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  #* Tab 2a ----
  rv2a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries2, {
    curr_tab1 <- input$tabseries2
    rv2a$prev <- readr::parse_number(curr_tab1) - 1
    rv2a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  #* Tab 3a ----
  rv3a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries3, {
    curr_tab1 <- input$tabseries3
    rv3a$prev <- readr::parse_number(curr_tab1) - 1
    rv3a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  #* Tab 4a ----
  rv4a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries4, {
    curr_tab1 <- input$tabseries4
    rv4a$prev <- readr::parse_number(curr_tab1) - 1
    rv4a$nxt <- readr::parse_number(curr_tab1) + 1
  })


  #** Render Report ----
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  report2 <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath

  observeEvent(input$generate, {

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering data and building report.",
                 detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 0)

    # Generate tables
    table_list <- list(tab_lr = NA,
                       tab_mlr = NA,
                       tab_models = NA)


    # Create directories for storing plots & tables
    dir.create("data/out_tables", showWarnings = TRUE)
    dir.create("www/out_plots", showWarnings = TRUE)

    table_list$tab_lr <- tryCatch({
      write.csv(lr_eqn$dt, "data/out_tables/tab_lr.csv", row.names = FALSE)
      "data/out_tables/tab_lr.csv"
    }, error = function(e) {NULL})

    table_list$tab_mlr <- tryCatch({
      write.csv(mlr$dt, "data/out_tables/tab_mlr.csv", row.names = FALSE)
      "data/out_tables/tab_mlr.csv"
    }, error = function(e) {NULL})

    table_list$tab_models <- tryCatch({
      write.csv(mod_selec_tab$dt, "data/out_tables/tab_models.csv", row.names = mod_names)
      "data/out_tables/tab_models.csv"
    }, error = function(e) {NULL})

    # Generate plots
    plot_list <- list(airt_wtemp_ts = NA,
                      lr_mod_ts = NA,
                      param_dist_lr = NA,
                      pers_mod = NA,
                      mlr_mod_ts = NA,
                      deter_fc = NA,
                      proc_uc_fc = NA,
                      param_dist_fc = NA,
                      param_uc_fc = NA,
                      ic_ts_dist = NA,
                      ic_uc_fc = NA,
                      airt_fc = NA,
                      driver_uc_fc = NA,
                      all_fc = NA,
                      tot_uc_fc1 = NA,
                      quant_uc_fc1 = NA,
                      tot_uc_fc2 = NA,
                      quant_uc_fc2 = NA,
                      dec1 = NA,
                      dec2 = NA)

    incr <- 1
    plot_list$airt_wtemp_ts <- tryCatch({
      df <- airt_swt$df
      df$airt[is.na(df$wtemp)] <- NA
      df$wtemp[is.na(df$airt)] <- NA
      df <- df[df$Date > "2019-01-01", ]
      p <- ggplot() +
        geom_line(data = df, aes(Date, airt, color = "Air"), size = l_siz) +
        geom_line(data = df, aes(Date, wtemp, color = "Water"), size = l_siz) +
        scale_color_manual(values = cols[5:6]) +
        # geom_point(data = airt_swt$df, aes(airt, wtemp), color = "black") +
        ylab("Temperature (\u00B0C)") +
        xlab("Time") +
        guides(color = guide_legend(title = "", override.aes = list(size = 3))) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/airt_wtemp_ts.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/airt_wtemp_ts.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$lr_mod_ts <- tryCatch({

      df <- airt_swt$df
      df$airt[is.na(df$wtemp)] <- NA
      df$wtemp[is.na(df$airt)] <- NA
      df <- df[df$Date > "2020-01-01", ]

      pars <- na.exclude(lr_pars$dt)
      freq_idx <- which(!is.na(lr_pars$dt[, 1]))

      if(nrow(pars) > 0) {
        mod <- lapply(1:nrow(pars), function(x) {
          df2 <- data.frame(Date = df$Date,
                            Model = pars$m_est[x] * df$airt + pars$b_est[x],
                            Frequency = samp_freq[freq_idx[x]])
          df2$rmse <- round(sqrt(mean((df2$Model - df$wtemp)^2, na.rm = TRUE)), 2)
          return(df2)
        })
        mlt <- do.call(rbind, mod)
        mlt$Frequency <- factor(mlt$Frequency, levels = samp_freq)

        for(freq in samp_freq) {
          sub <- mlt[mlt$Frequency == freq, ]
          fidx <- which(samp_freq == freq)
          if(nrow(sub) > 0) {
            lr_pars$dt$rmse[fidx] <- sub$rmse[1]
            if(freq == "Daily") {
              mod_selec_tab$dt$rmse[3] <- sub$rmse[1]
            }
          }
        }
      }

      p <- ggplot() +
        geom_point(data = df, aes(Date, wtemp), color = "black") +
        ylab("Temperature (\u00B0C)") +
        xlab("Time") +
        guides(color = guide_legend(override.aes = list(size = 3))) +
        theme_bw(base_size = 18) +
        png_theme

      if(nrow(pars) > 0) {
        p <- p +
          geom_line(data = mlt, aes(Date, Model, color = Frequency)) +
          scale_color_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4])) +
          labs(color = "Frequency")
      }

      ggsave("www/out_plots/lr_mod_ts.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/lr_mod_ts.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$param_dist_lr <- tryCatch({
      lst <- lr_dist_plot$lst[!is.na(lr_dist_plot$lst)]

      mlt <- do.call(rbind, lst)

      y_max_m <- lapply(lst, function(x) {
        dens_m <- density(x$m)
        max(dens_m$y)
      })

      ylims_m <- c(0, max(c(6, max(unlist(y_max_m), na.rm = TRUE))))

      y_max_b <- lapply(lst, function(x) {
        dens_b <- density(x$b)
        max(dens_b$y)
      })

      ylims_b <- c(0, max(c(1.5, max(unlist(y_max_b), na.rm = TRUE))))


      xlims_m <- c(min(0, mlt$m), max(2, mlt$m))
      xlims_b <- c(min(-2.5, mlt$b), max(10, mlt$b))


      p1 <- ggplot() +
        geom_density(data = mlt, aes(m, fill = Frequency), color = NA, alpha = 0.5) +
        coord_cartesian(xlim = xlims_m, ylim = ylims_m) +
        ylab("Density") +
        xlab("Value") +
        ggtitle("Slope (m)") +
        scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4], "User input" = cols[5])) +
        theme_bw(base_size = 18) +
        png_theme

      p2 <- ggplot() +
        geom_density(data = mlt, aes(b, fill = Frequency), color = NA, alpha = 0.5) +
        coord_cartesian(xlim = xlims_b, ylim = ylims_b) +
        ylab("Density") +
        xlab("Value") +
        ggtitle("Intercept (b)") +
        scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4], "User input" = cols[5])) +
        theme_bw(base_size = 18) +
        png_theme

      g <- ggpubr::ggarrange(p1, p2, nrow = 1, align = "h", common.legend = TRUE, legend = "bottom")

      ggsave("www/out_plots/param_dist_lr.png", g, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/param_dist_lr.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$pers_mod <- tryCatch({

      df <- persist_df$df
      df <- df[df$Date > "2020-01-01", ]

      p <- ggplot() +
        geom_point(data = df, aes(Date, wtemp, color = "Obs")) +
        ylab("Water temperature (\u00B0C)") +
        xlab("Time") +
        scale_color_manual(values = c("Pers" = cols[3], "Obs" = "black")) +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3))) +
        theme_bw(base_size = 18) +
        png_theme

      if(input$plot_persist > 0) {
        p <- p +
          geom_line(data = df, aes(Date, Mod, color = "Pers"), size = l_siz)
      }

      ggsave("www/out_plots/pers_mod.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/pers_mod.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$mlr_mod_ts <- tryCatch({

      df <- airt_swt$df
      df$airt[is.na(df$wtemp)] <- NA
      df$wtemp[is.na(df$airt)] <- NA

      df <- df[df$Date > "2020-01-01", ]

      p <- ggplot() +
        geom_point(data = df, aes(Date, wtemp), color = "black") +
        ylab("Temperature (\u00B0C)") +
        xlab("Time") +
        # facet_wrap(~per, scales = "free_x") +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3))) +
        scale_color_manual(values = c("Wtemp" = cols[4],"Both" = cols[6])) +
        theme_bw(base_size = 18) +
        png_theme

      sub_lst <- mlr_pred$lst[!is.na(mlr_pred$lst)]

      if(length(sub_lst) > 0) {
        mlt <- reshape::melt(sub_lst, id.vars = "Date")
        colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
        mlt$Label <- as.character(mlt$Label)

        mlt$Label[mlt$Label == 1] <- "Wtemp"
        mlt$Label[mlt$Label == 2] <- "Both"

        mlt$Label <- factor(mlt$Label, levels = c("Wtemp", "Both"))

        mlt <- mlt[mlt$Date > "2020-01-01", ]

        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Label), size = l_siz)
      }

      ggsave("www/out_plots/mlr_mod_ts.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/mlr_mod_ts.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$deter_fc <- tryCatch({

      if(any(!is.na(wtemp_fc_out1a$lst))) {
        sub_lst <- wtemp_fc_out1a$lst[!is.null(wtemp_fc_out1a$lst)]
        mlt <- reshape::melt(sub_lst, id.vars = "Date")
        colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
        for(num in 1:4) {
          if(num %in% mlt$Label) {
            mlt$Label[mlt$Label == num] <- mod_names[num]
          }
        }
        mlt$Label <- factor(mlt$Label, levels = mod_names)
      }

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3))) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme

      if(any(!is.na(wtemp_fc_out1a$lst))) {
        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Label), size = l_siz)
      }

      p <- p +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6]))



      ggsave("www/out_plots/deter_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/deter_fc.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$proc_uc_fc <- tryCatch({

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)")

      if(any(!is.na(wtemp_fc_out2$dist))) {
        sub_lst <- wtemp_fc_out2$dist[!is.null(wtemp_fc_out2$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4])) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/proc_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_proc <- p

      "www/out_plots/proc_uc_fc.png"
    }, error = function(e) {p_proc <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    if(all(is.na(lst))) {
      plot_list$param_dist_fc <- NA
    } else {
      plot_list$param_dist_fc <- tryCatch({

        lst <- param_dist3b$dist
        names(lst) <- mod_names
        # mlt <- reshape::melt(lst)

        pl <- lapply(names(lst), function(x) {
          mlt <-  reshape::melt(lst[[x]])
          if(nrow(mlt) == 1) mlt$variable = NA
          ggplot(mlt) +
            geom_density(aes(value, fill = x), alpha = 0.5) +
            facet_wrap(~variable, nrow = 1, scales = "free_x") +
            guides(fill = guide_legend(title = "Model:")) +
            ggtitle(x) +
            {if(nrow(mlt) != 1)scale_x_continuous(n.breaks = 4)} +
            scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                         "Atemp" = l.cols[3], "Both" = l.cols[4])) +
            theme_bw(base_size = 12) +
            theme(plot.title = element_text(hjust = 0.5)) +
            png_theme
        })

        g <- ggarrange(plotlist = pl, common.legend = TRUE, legend = "bottom")

        ggsave("www/out_plots/param_dist_fc.png", g, dpi = png_dpi, width = p_wid, height = p_wid, units = p_units)

        "www/out_plots/param_dist_fc.png"
      }, error = function(e) {NA})
    }
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$param_uc_fc <- tryCatch({

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme

      if(any(!is.na(wtemp_fc_out3b$dist))) {
        sub_lst <- wtemp_fc_out3b$dist[!is.null(wtemp_fc_out3b$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4]))



      ggsave("www/out_plots/param_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_param <- p

      "www/out_plots/param_uc_fc.png"
    }, error = function(e) {p_param <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$ic_ts_dist <- tryCatch({

      df <- wtemp_fc_data$hist[2:5, ]

      p1 <- ggplot()

      if(!is.null(ic_dist$df)) {
        quants <- quantile(ic_dist$df$value, c(0.25, 0.75))

        err_bar <- data.frame(x = as.Date(fc_date), ymin = quants[1], ymax = quants[2])
        p1 <- p1 +
          geom_errorbar(data = err_bar, aes(x, ymin = ymin, ymax = ymax, width = 0.5), size = (l_siz + 0.1))
      }

      p1 <- p1 +
        geom_point(data = df, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        xlab("Date") +
        scale_color_manual(values = c("Obs" = cols[2])) +
        guides(color = "none") +
        theme_bw(base_size = 14) +
        png_theme

      df <- data.frame(x = wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date == fc_date],
                       label = "Observed")

      xlims <- c(df$x -1.5, df$x + 1.5)
      ylims <- c(0,7)

      p2 <- ggplot() +
        geom_vline(xintercept = df$x, size = l_siz) +
        geom_density(data = ic_dist$df, aes(value), fill = l.cols[2], alpha = 0.3) +
        xlab("Temperature (\u00B0C)") +
        ylab("Density") +
        coord_cartesian(xlim = xlims, ylim = ylims) +
        theme_bw(base_size = 14) +
        png_theme

      g <- ggarrange(p1, p2, nrow = 1, labels = "AUTO", align = "h")

      ggsave("www/out_plots/ic_ts_dist.png", g, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/ic_ts_dist.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$ic_uc_fc <- tryCatch({

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)")


      if(any(!is.na(wtemp_fc_out4$dist))) {
        sub_lst <- wtemp_fc_out4$dist[!is.null(wtemp_fc_out4$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4])) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/ic_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_ic <- p

      "www/out_plots/ic_uc_fc.png"
    }, error = function(e) {p_ic <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$airt_fc <- tryCatch({

      mlt <- noaa_df$airt

      mlt$Date <- as.Date(mlt$time)
      mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
      mlt <- mlt[mlt$Date <= "2020-10-02", ]
      mlt$time <- as.POSIXct(mlt$Date)
      fut_offset <- lubridate::days(6) #+ lubridate::hours(19)

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp."), size = p_siz) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 14) +
        png_theme


        mlt <- mlt[mlt$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]
        p1 <- p +
          geom_line(data = mlt, aes(Date, value, group = variable), color = "gray", alpha = 0.6, size = l_siz)

        wid <- tidyr::pivot_wider(mlt, c(Date, L1), names_from = variable, values_from = value)
        wid <- wid[, 1:(input$noaa_n_mems + 2)]
        df <- apply(wid[, -c(1, 2)], 1, function(x){
          quantile(x, c(0.05, 0.5, 0.875, 0.95))
        })
        df <- as.data.frame(t(df))
        colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
        df$Date <- wid$Date
        p2 <- p +
          geom_ribbon(data = df, aes(Date, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3)+
          geom_line(data = df, aes(Date, p50, color = "Median"), size = l_siz)

      g <- ggarrange(p1, p2, nrow = 1, labels = "AUTO", align = "h")


      ggsave("www/out_plots/airt_fc.png", g, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/airt_fc.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$driver_uc_fc <- tryCatch({

      if(any(!is.na(wtemp_fc_out5$lst))) {
        sub_lst <- wtemp_fc_out5$lst[!is.na(wtemp_fc_out5$lst)]
        mlt <- reshape::melt(sub_lst, id.vars = "Date")
        colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
        mlt$Label <- as.character(mlt$Label)
      }

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)")

      if(any(!is.na(wtemp_fc_out5$dist))) {
        sub_lst <- wtemp_fc_out5$dist[!is.null(wtemp_fc_out5$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4])) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/driver_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_driv <- p

      "www/out_plots/driver_uc_fc.png"
    }, error = function(e) {p_driv <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$all_fc <- tryCatch({

      pl <- list(p_proc + ggtitle(uc_sources[1]), p_param + ggtitle(uc_sources[2]), p_ic + ggtitle(uc_sources[3]), p_driv + ggtitle(uc_sources[4]))
      g <- ggarrange(plotlist = pl, # labels = uc_sources[1:4],
                     align = "hv", common.legend = TRUE, legend = "bottom")

      ggsave("www/out_plots/all_fc.png", g, dpi = png_dpi, width = p_wid, height = 2*p_hei, units = p_units)

      "www/out_plots/all_fc.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$tot_uc_fc1 <- tryCatch({

      idx <- which(mod_names == tot_fc_dataA$lab)
      sel_col <- cols[idx]

      dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]


      p <- ggplot() +
        geom_point(data = dat, aes(Date, wtemp, color = "Water temp.")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme


      if(!is.null(tot_fc_dataA$dist)) {
        mlt <- tot_fc_dataA$dist

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95), fill = sel_col, alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50), color = sel_col, size = l_siz)
      }

      ggsave("www/out_plots/tot_uc_fc1.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/tot_uc_fc1.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$quant_uc_fc1 <- tryCatch({

      p <- ggplot() +
        geom_bar(data = quantfcA$df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
        ylab("Standard Deviation (\u00B0C)") +
        scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                     "Driver" = cols2[4], "Total" = cols2[5])) +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        labs(fill = "Uncertainty:") +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/quant_uc_fc1.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/quant_uc_fc1.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$tot_uc_fc2 <- tryCatch({

      idx <- which(mod_names == tot_fc_dataB$lab)
      sel_col <- cols[idx]

      dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]

      p <- ggplot() +
        geom_point(data = dat, aes(Date, wtemp, color = "Water temp.")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme


      if(!is.null(tot_fc_dataB$dist)) {
        mlt <- tot_fc_dataB$dist
        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95), fill = sel_col, alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50), color = sel_col, size = l_siz)
      }

      ggsave("www/out_plots/tot_uc_fc2.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/tot_uc_fc2.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$quant_uc_fc2 <- tryCatch({

      p <- ggplot() +
        geom_bar(data = quantfcB$df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
        ylab("Standard Deviation (\u00B0C)") +
        scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                     "Driver" = cols2[4], "Total" = cols2[5])) +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        labs(fill = "Uncertainty:") +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/quant_uc_fc2.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/quant_uc_fc2.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$dec1 <- tryCatch({
      p <- ggplot(scen_fc1) +
        geom_hline(yintercept = 12, linetype = "dashed", size = l_siz) +
        geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
        geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
        geom_line(aes(Date, surftemp, color = "Surface"), size = l_siz) +
        geom_line(aes(Date, bottemp, color = "Bottom"), size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        xlab("Day") +
        guides(color = "none") +
        labs(fill = "Location") +
        scale_x_date(breaks = "1 day", date_labels = "%a") +
        scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
        scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
        coord_cartesian(ylim = c(8, 14)) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/dec1.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/dec1.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$dec2 <- tryCatch({
      p <- ggplot(scen_fc2) +
        geom_hline(yintercept = 12, linetype = "dashed", size = l_siz) +
        geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
        geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
        geom_line(aes(Date, surftemp, color = "Surface"), size = l_siz) +
        geom_line(aes(Date, bottemp, color = "Bottom"), size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        xlab("Day") +
        guides(color = "none") +
        labs(fill = "Location") +
        scale_x_date(breaks = "1 day", date_labels = "%a") +
        scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
        scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
        coord_cartesian(ylim = c(8, 14)) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/dec2.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/dec2.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    # Set up parameters to pass to Rmd document
    params <- list(name = input$name,
                   id_number = input$id_number,
                   answers = answers,
                   plot_list = plot_list,
                   pheno_file = pheno_file$img,
                   mod_selec1 = input$mod_selec_tot_fc[1],
                   mod_selec2 = input$mod_selec_tot_fc[2],
                   dec1 = input$dec_scen1,
                   dec2 = input$dec_scen2
    )
    # print(params$plot_list)


    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored

    rmarkdown::render("report.Rmd",
                      output_format = "all",
                      output_file = tmp_file,
                      params = params,
                      envir = new.env(parent = globalenv()))
    progress$set(value = 1)
    report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above

  })

  observeEvent(input$generate2, {

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering data and building report.",
                 detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 0)

    # Generate tables
    table_list <- list(tab_lr = NA,
                       tab_mlr = NA,
                       tab_models = NA)


    # Create directories for storing plots & tables
    dir.create("data/out_tables", showWarnings = TRUE)
    dir.create("www/out_plots", showWarnings = TRUE)

    table_list$tab_lr <- tryCatch({
      write.csv(lr_eqn$dt, "data/out_tables/tab_lr.csv", row.names = FALSE)
      "data/out_tables/tab_lr.csv"
    }, error = function(e) {NULL})

    table_list$tab_mlr <- tryCatch({
      write.csv(mlr$dt, "data/out_tables/tab_mlr.csv", row.names = FALSE)
      "data/out_tables/tab_mlr.csv"
    }, error = function(e) {NULL})

    table_list$tab_models <- tryCatch({
      write.csv(mod_selec_tab$dt, "data/out_tables/tab_models.csv", row.names = mod_names)
      "data/out_tables/tab_models.csv"
    }, error = function(e) {NULL})

    # Generate plots
    plot_list <- list(airt_wtemp_ts = NA,
                      lr_mod_ts = NA,
                      param_dist_lr = NA,
                      pers_mod = NA,
                      mlr_mod_ts = NA,
                      deter_fc = NA,
                      proc_uc_fc = NA,
                      param_dist_fc = NA,
                      param_uc_fc = NA,
                      ic_ts_dist = NA,
                      ic_uc_fc = NA,
                      airt_fc = NA,
                      driver_uc_fc = NA,
                      all_fc = NA,
                      tot_uc_fc1 = NA,
                      quant_uc_fc1 = NA,
                      tot_uc_fc2 = NA,
                      quant_uc_fc2 = NA,
                      dec1 = NA,
                      dec2 = NA)

    incr <- 1
    plot_list$airt_wtemp_ts <- tryCatch({
      df <- airt_swt$df
      df$airt[is.na(df$wtemp)] <- NA
      df$wtemp[is.na(df$airt)] <- NA
      df <- df[df$Date > "2019-01-01", ]
      p <- ggplot() +
        geom_line(data = df, aes(Date, airt, color = "Air"), size = l_siz) +
        geom_line(data = df, aes(Date, wtemp, color = "Water"), size = l_siz) +
        scale_color_manual(values = cols[5:6]) +
        # geom_point(data = airt_swt$df, aes(airt, wtemp), color = "black") +
        ylab("Temperature (\u00B0C)") +
        xlab("Time") +
        guides(color = guide_legend(title = "", override.aes = list(size = 3))) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/airt_wtemp_ts.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/airt_wtemp_ts.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$lr_mod_ts <- tryCatch({

      df <- airt_swt$df
      df$airt[is.na(df$wtemp)] <- NA
      df$wtemp[is.na(df$airt)] <- NA
      df <- df[df$Date > "2020-01-01", ]

      pars <- na.exclude(lr_pars$dt)
      freq_idx <- which(!is.na(lr_pars$dt[, 1]))

      if(nrow(pars) > 0) {
        mod <- lapply(1:nrow(pars), function(x) {
          df2 <- data.frame(Date = df$Date,
                            Model = pars$m_est[x] * df$airt + pars$b_est[x],
                            Frequency = samp_freq[freq_idx[x]])
          df2$rmse <- round(sqrt(mean((df2$Model - df$wtemp)^2, na.rm = TRUE)), 2)
          return(df2)
        })
        mlt <- do.call(rbind, mod)
        mlt$Frequency <- factor(mlt$Frequency, levels = samp_freq)

        for(freq in samp_freq) {
          sub <- mlt[mlt$Frequency == freq, ]
          fidx <- which(samp_freq == freq)
          if(nrow(sub) > 0) {
            lr_pars$dt$rmse[fidx] <- sub$rmse[1]
            if(freq == "Daily") {
              mod_selec_tab$dt$rmse[3] <- sub$rmse[1]
            }
          }
        }
      }

      p <- ggplot() +
        geom_point(data = df, aes(Date, wtemp), color = "black") +
        ylab("Temperature (\u00B0C)") +
        xlab("Time") +
        guides(color = guide_legend(override.aes = list(size = 3))) +
        theme_bw(base_size = 18) +
        png_theme

      if(nrow(pars) > 0) {
        p <- p +
          geom_line(data = mlt, aes(Date, Model, color = Frequency)) +
          scale_color_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4])) +
          labs(color = "Frequency")
      }

      ggsave("www/out_plots/lr_mod_ts.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/lr_mod_ts.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$param_dist_lr <- tryCatch({
      lst <- lr_dist_plot$lst[!is.na(lr_dist_plot$lst)]

      mlt <- do.call(rbind, lst)

      y_max_m <- lapply(lst, function(x) {
        dens_m <- density(x$m)
        max(dens_m$y)
      })

      ylims_m <- c(0, max(c(6, max(unlist(y_max_m), na.rm = TRUE))))

      y_max_b <- lapply(lst, function(x) {
        dens_b <- density(x$b)
        max(dens_b$y)
      })

      ylims_b <- c(0, max(c(1.5, max(unlist(y_max_b), na.rm = TRUE))))


      xlims_m <- c(min(0, mlt$m), max(2, mlt$m))
      xlims_b <- c(min(-2.5, mlt$b), max(10, mlt$b))


      p1 <- ggplot() +
        geom_density(data = mlt, aes(m, fill = Frequency), color = NA, alpha = 0.5) +
        coord_cartesian(xlim = xlims_m, ylim = ylims_m) +
        ylab("Density") +
        xlab("Value") +
        ggtitle("Slope (m)") +
        scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4], "User input" = cols[5])) +
        theme_bw(base_size = 18) +
        png_theme

      p2 <- ggplot() +
        geom_density(data = mlt, aes(b, fill = Frequency), color = NA, alpha = 0.5) +
        coord_cartesian(xlim = xlims_b, ylim = ylims_b) +
        ylab("Density") +
        xlab("Value") +
        ggtitle("Intercept (b)") +
        scale_fill_manual(values = c("Monthly" = cols[1], "Fortnightly" = cols[2], "Weekly" = cols[3], "Daily" = cols[4], "User input" = cols[5])) +
        theme_bw(base_size = 18) +
        png_theme

      g <- ggpubr::ggarrange(p1, p2, nrow = 1, align = "h", common.legend = TRUE, legend = "bottom")

      ggsave("www/out_plots/param_dist_lr.png", g, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/param_dist_lr.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$pers_mod <- tryCatch({

      df <- persist_df$df
      df <- df[df$Date > "2020-01-01", ]

      p <- ggplot() +
        geom_point(data = df, aes(Date, wtemp, color = "Obs")) +
        ylab("Water temperature (\u00B0C)") +
        xlab("Time") +
        scale_color_manual(values = c("Pers" = cols[3], "Obs" = "black")) +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3))) +
        theme_bw(base_size = 18) +
        png_theme

      if(input$plot_persist > 0) {
        p <- p +
          geom_line(data = df, aes(Date, Mod, color = "Pers"), size = l_siz)
      }

      ggsave("www/out_plots/pers_mod.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/pers_mod.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$mlr_mod_ts <- tryCatch({

      df <- airt_swt$df
      df$airt[is.na(df$wtemp)] <- NA
      df$wtemp[is.na(df$airt)] <- NA

      df <- df[df$Date > "2020-01-01", ]

      p <- ggplot() +
        geom_point(data = df, aes(Date, wtemp), color = "black") +
        ylab("Temperature (\u00B0C)") +
        xlab("Time") +
        # facet_wrap(~per, scales = "free_x") +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3))) +
        scale_color_manual(values = c("Wtemp" = cols[4],"Both" = cols[6])) +
        theme_bw(base_size = 18) +
        png_theme

      sub_lst <- mlr_pred$lst[!is.na(mlr_pred$lst)]

      if(length(sub_lst) > 0) {
        mlt <- reshape::melt(sub_lst, id.vars = "Date")
        colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
        mlt$Label <- as.character(mlt$Label)

        mlt$Label[mlt$Label == 1] <- "Wtemp"
        mlt$Label[mlt$Label == 2] <- "Both"

        mlt$Label <- factor(mlt$Label, levels = c("Wtemp", "Both"))

        mlt <- mlt[mlt$Date > "2020-01-01", ]

        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Label), size = l_siz)
      }

      ggsave("www/out_plots/mlr_mod_ts.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/mlr_mod_ts.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$deter_fc <- tryCatch({

      if(any(!is.na(wtemp_fc_out1a$lst))) {
        sub_lst <- wtemp_fc_out1a$lst[!is.null(wtemp_fc_out1a$lst)]
        mlt <- reshape::melt(sub_lst, id.vars = "Date")
        colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
        for(num in 1:4) {
          if(num %in% mlt$Label) {
            mlt$Label[mlt$Label == num] <- mod_names[num]
          }
        }
        mlt$Label <- factor(mlt$Label, levels = mod_names)
      }

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3))) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme

      if(any(!is.na(wtemp_fc_out1a$lst))) {
        p <- p +
          geom_line(data = mlt, aes(Date, value, color = Label), size = l_siz)
      }

      p <- p +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6]))



      ggsave("www/out_plots/deter_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/deter_fc.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$proc_uc_fc <- tryCatch({

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)")

      if(any(!is.na(wtemp_fc_out2$dist))) {
        sub_lst <- wtemp_fc_out2$dist[!is.null(wtemp_fc_out2$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4])) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/proc_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_proc <- p

      "www/out_plots/proc_uc_fc.png"
    }, error = function(e) {p_proc <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    if(all(is.na(lst))) {
      plot_list$param_dist_fc <- NA
    } else {
      plot_list$param_dist_fc <- tryCatch({

        lst <- param_dist3b$dist
        names(lst) <- mod_names
        # mlt <- reshape::melt(lst)

        pl <- lapply(names(lst), function(x) {
          mlt <-  reshape::melt(lst[[x]])
          if(nrow(mlt) == 1) mlt$variable = NA
          ggplot(mlt) +
            geom_density(aes(value, fill = x), alpha = 0.5) +
            facet_wrap(~variable, nrow = 1, scales = "free_x") +
            guides(fill = guide_legend(title = "Model:")) +
            ggtitle(x) +
            {if(nrow(mlt) != 1)scale_x_continuous(n.breaks = 4)} +
            scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                         "Atemp" = l.cols[3], "Both" = l.cols[4])) +
            theme_bw(base_size = 12) +
            theme(plot.title = element_text(hjust = 0.5)) +
            png_theme
        })

        g <- ggarrange(plotlist = pl, common.legend = TRUE, legend = "bottom")

        ggsave("www/out_plots/param_dist_fc.png", g, dpi = png_dpi, width = p_wid, height = p_wid, units = p_units)

        "www/out_plots/param_dist_fc.png"
      }, error = function(e) {NA})
    }
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$param_uc_fc <- tryCatch({

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme

      if(any(!is.na(wtemp_fc_out3b$dist))) {
        sub_lst <- wtemp_fc_out3b$dist[!is.null(wtemp_fc_out3b$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4]))



      ggsave("www/out_plots/param_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_param <- p

      "www/out_plots/param_uc_fc.png"
    }, error = function(e) {p_param <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$ic_ts_dist <- tryCatch({

      df <- wtemp_fc_data$hist[2:5, ]

      p1 <- ggplot()

      if(!is.null(ic_dist$df)) {
        quants <- quantile(ic_dist$df$value, c(0.25, 0.75))

        err_bar <- data.frame(x = as.Date(fc_date), ymin = quants[1], ymax = quants[2])
        p1 <- p1 +
          geom_errorbar(data = err_bar, aes(x, ymin = ymin, ymax = ymax, width = 0.5), size = (l_siz + 0.1))
      }

      p1 <- p1 +
        geom_point(data = df, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        xlab("Date") +
        scale_color_manual(values = c("Obs" = cols[2])) +
        guides(color = "none") +
        theme_bw(base_size = 14) +
        png_theme

      df <- data.frame(x = wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date == fc_date],
                       label = "Observed")

      xlims <- c(df$x -1.5, df$x + 1.5)
      ylims <- c(0,7)

      p2 <- ggplot() +
        geom_vline(xintercept = df$x, size = l_siz) +
        geom_density(data = ic_dist$df, aes(value), fill = l.cols[2], alpha = 0.3) +
        xlab("Temperature (\u00B0C)") +
        ylab("Density") +
        coord_cartesian(xlim = xlims, ylim = ylims) +
        theme_bw(base_size = 14) +
        png_theme

      g <- ggarrange(p1, p2, nrow = 1, labels = "AUTO", align = "h")

      ggsave("www/out_plots/ic_ts_dist.png", g, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/ic_ts_dist.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$ic_uc_fc <- tryCatch({

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)")


      if(any(!is.na(wtemp_fc_out4$dist))) {
        sub_lst <- wtemp_fc_out4$dist[!is.null(wtemp_fc_out4$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4])) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/ic_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_ic <- p

      "www/out_plots/ic_uc_fc.png"
    }, error = function(e) {p_ic <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$airt_fc <- tryCatch({

      mlt <- noaa_df$airt

      mlt$Date <- as.Date(mlt$time)
      mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
      mlt <- mlt[mlt$Date <= "2020-10-02", ]
      mlt$time <- as.POSIXct(mlt$Date)
      fut_offset <- lubridate::days(6) #+ lubridate::hours(19)

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp."), size = p_siz) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 14) +
        png_theme


      mlt <- mlt[mlt$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]
      p1 <- p +
        geom_line(data = mlt, aes(Date, value, group = variable), color = "gray", alpha = 0.6, size = l_siz)

      wid <- tidyr::pivot_wider(mlt, c(Date, L1), names_from = variable, values_from = value)
      wid <- wid[, 1:(input$noaa_n_mems + 2)]
      df <- apply(wid[, -c(1, 2)], 1, function(x){
        quantile(x, c(0.05, 0.5, 0.875, 0.95))
      })
      df <- as.data.frame(t(df))
      colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
      df$Date <- wid$Date
      p2 <- p +
        geom_ribbon(data = df, aes(Date, ymin = p5, ymax = p95), fill = l.cols[2], alpha = 0.3)+
        geom_line(data = df, aes(Date, p50, color = "Median"), size = l_siz)

      g <- ggarrange(p1, p2, nrow = 1, labels = "AUTO", align = "h")


      ggsave("www/out_plots/airt_fc.png", g, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/airt_fc.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$driver_uc_fc <- tryCatch({

      if(any(!is.na(wtemp_fc_out5$lst))) {
        sub_lst <- wtemp_fc_out5$lst[!is.na(wtemp_fc_out5$lst)]
        mlt <- reshape::melt(sub_lst, id.vars = "Date")
        colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
        mlt$Label <- as.character(mlt$Label)
      }

      p <- ggplot() +
        geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Obs")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)")

      if(any(!is.na(wtemp_fc_out5$dist))) {
        sub_lst <- wtemp_fc_out5$dist[!is.null(wtemp_fc_out5$dist)]
        mlt <- do.call(rbind, sub_lst)
        mlt <- na.exclude(mlt)
        for(num in 1:4) {
          if(num %in% mlt$Level) {
            mlt$Level[mlt$Level == num] <- mod_names[num]
          }
        }
        mlt$Level <- factor(mlt$Level, levels = mod_names)

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = Level), size = l_siz)
      }

      p <- p +
        guides(color = guide_legend(title = "Model:", override.aes = list(size = 3)), fill = "none") +
        scale_color_manual(values = c("Obs" = cols[2],
                                      "Pers" = cols[3], "Wtemp" = cols[4],
                                      "Atemp" = cols[5], "Both" = cols[6])) +
        scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                     "Atemp" = l.cols[3], "Both" = l.cols[4])) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/driver_uc_fc.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)
      p_driv <- p

      "www/out_plots/driver_uc_fc.png"
    }, error = function(e) {p_driv <- p;NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$all_fc <- tryCatch({

      pl <- list(p_proc + ggtitle(uc_sources[1]), p_param + ggtitle(uc_sources[2]), p_ic + ggtitle(uc_sources[3]), p_driv + ggtitle(uc_sources[4]))
      g <- ggarrange(plotlist = pl, # labels = uc_sources[1:4],
                     align = "hv", common.legend = TRUE, legend = "bottom")

      ggsave("www/out_plots/all_fc.png", g, dpi = png_dpi, width = p_wid, height = 2*p_hei, units = p_units)

      "www/out_plots/all_fc.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$tot_uc_fc1 <- tryCatch({

      idx <- which(mod_names == tot_fc_dataA$lab)
      sel_col <- cols[idx]

      dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]


      p <- ggplot() +
        geom_point(data = dat, aes(Date, wtemp, color = "Water temp.")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme


      if(!is.null(tot_fc_dataA$dist)) {
        mlt <- tot_fc_dataA$dist

        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95), fill = sel_col, alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50), color = sel_col, size = l_siz)
      }

      ggsave("www/out_plots/tot_uc_fc1.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/tot_uc_fc1.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$quant_uc_fc1 <- tryCatch({

      p <- ggplot() +
        geom_bar(data = quantfcA$df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
        ylab("Standard Deviation (\u00B0C)") +
        scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                     "Driver" = cols2[4], "Total" = cols2[5])) +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        labs(fill = "Uncertainty:") +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/quant_uc_fc1.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/quant_uc_fc1.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$tot_uc_fc2 <- tryCatch({

      idx <- which(mod_names == tot_fc_dataB$lab)
      sel_col <- cols[idx]

      dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]

      p <- ggplot() +
        geom_point(data = dat, aes(Date, wtemp, color = "Water temp.")) +
        geom_vline(xintercept = as.Date(fc_date), linetype = "dashed", size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        theme_bw(base_size = 18) +
        png_theme


      if(!is.null(tot_fc_dataB$dist)) {
        mlt <- tot_fc_dataB$dist
        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95), fill = sel_col, alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50), color = sel_col, size = l_siz)
      }

      ggsave("www/out_plots/tot_uc_fc2.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/tot_uc_fc2.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$quant_uc_fc2 <- tryCatch({

      p <- ggplot() +
        geom_bar(data = quantfcB$df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
        ylab("Standard Deviation (\u00B0C)") +
        scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                     "Driver" = cols2[4], "Total" = cols2[5])) +
        scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
        labs(fill = "Uncertainty:") +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/quant_uc_fc2.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/quant_uc_fc2.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$dec1 <- tryCatch({
      p <- ggplot(scen_fc1) +
        geom_hline(yintercept = 12, linetype = "dashed", size = l_siz) +
        geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
        geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
        geom_line(aes(Date, surftemp, color = "Surface"), size = l_siz) +
        geom_line(aes(Date, bottemp, color = "Bottom"), size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        xlab("Day") +
        guides(color = "none") +
        labs(fill = "Location") +
        scale_x_date(breaks = "1 day", date_labels = "%a") +
        scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
        scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
        coord_cartesian(ylim = c(8, 14)) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/dec1.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/dec1.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    plot_list$dec2 <- tryCatch({
      p <- ggplot(scen_fc2) +
        geom_hline(yintercept = 12, linetype = "dashed", size = l_siz) +
        geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
        geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
        geom_line(aes(Date, surftemp, color = "Surface"), size = l_siz) +
        geom_line(aes(Date, bottemp, color = "Bottom"), size = l_siz) +
        ylab("Temperature (\u00B0C)") +
        xlab("Day") +
        guides(color = "none") +
        labs(fill = "Location") +
        scale_x_date(breaks = "1 day", date_labels = "%a") +
        scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
        scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
        coord_cartesian(ylim = c(8, 14)) +
        theme_bw(base_size = 18) +
        png_theme

      ggsave("www/out_plots/dec2.png", p, dpi = png_dpi, width = p_wid, height = p_hei, units = p_units)

      "www/out_plots/dec2.png"
    }, error = function(e) {NA})
    progress$set(value = incr/length(plot_list))
    incr <- incr + 1

    # Set up parameters to pass to Rmd document
    params <- list(name = input$name,
                   id_number = input$id_number,
                   answers = answers,
                   plot_list = plot_list,
                   pheno_file = pheno_file$img,
                   mod_selec1 = input$mod_selec_tot_fc[1],
                   mod_selec2 = input$mod_selec_tot_fc[2],
                   dec1 = input$dec_scen1,
                   dec2 = input$dec_scen2
    )
    # print(params$plot_list)


    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored

    rmarkdown::render("report.Rmd",
                      output_format = "all",
                      output_file = tmp_file,
                      params = params,
                      envir = new.env(parent = globalenv()))
    progress$set(value = 1)
    report2$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above

  })

  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden = FALSE)

  # Hide download button until report is generated
  output$reportbuilt2 <- reactive({
    return(!is.null(report2$filepath))
  })
  outputOptions(output, 'reportbuilt2', suspendWhenHidden = FALSE)


  #** Download Report ----

  #Download report
  output$download <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("report_", input$id_number, ".docx") %>%
        gsub(" ", "_", .)
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      file.copy(report$filepath, file)
    }
  )

  #Download report
  output$download2 <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("report_", input$id_number, ".docx") %>%
        gsub(" ", "_", .)
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      file.copy(report2$filepath, file)
    }
  )

  #* Download '.eddie' file ----
  # Save answers in .eddie file
  ans_list <- reactiveValues()
  observe({
    for(i in 1:nrow(answers)) {
      if(length(input[[qid[i]]]) != 0) {
        answers[qid[i], 1] <<- input[[qid[i]]]
      }
    }

    ans_list <<- list(
      name = input$name,
      id_number = input$id_number,
      answers = answers,
      lr_eqn_dt = lr_eqn$dt,
      lr_pars_dt = lr_pars$dt,
      linr_stats_dt = linr_stats$dt,
      mlr_dt = mlr$dt,
      mod_selec_tab_dt = mod_selec_tab$dt,
      site_row = input$table01_rows_selected,
      mlr_params_df = mlr_params$df
    )
  })

  output$download_answers <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("module6_answers_", input$id_number, ".eddie") %>%
        gsub(" ", "_", .)
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # write.csv(ans_list, file)
      saveRDS(ans_list, file = file)
    }
  )

  #** Upload .eddie file ----
  observeEvent(input$upload_answers, {

    up_answers <<- readRDS(input$upload_answers$datapath)
    updateTextAreaInput(session, "name", value = up_answers$name)
    updateTextAreaInput(session, "id_number", value = up_answers$id_number)

    lr_eqn$dt <- up_answers$lr_eqn_dt
    lr_pars$dt <- up_answers$lr_pars_dt
    linr_stats$dt <- up_answers$linr_stats_dt
    mlr$dt <- up_answers$mlr_dt
    mod_selec_tab$dt <- up_answers$mod_selec_tab_dt
    mlr_params$df = up_answers$mlr_params_df


    for(i in 1:nrow(up_answers$answers)) {
      if(qid[i] == "q7") {
        updateRadioButtons(session, qid[i], selected = up_answers$answers[qid[i], 1])
      } else if(!(qid[i] %in% c("q3", "q7"))) {
        updateTextAreaInput(session, qid[i], value = up_answers$answers[qid[i], 1])
      }
    }

    showModal(
      modalDialog(
        title = "Upload complete!",
        "All your answers have been uploaded and your models. You will need to regenerate the plots within the Shiny app before generating your final report.")
    )

  })

  # Select site when uploading answers
  observe({
    req(input$maintab == "mtab4" & exists("up_answers") & input$tabseries1 == "obj1")
    req(!is.null(up_answers$site_row))
    tryCatch(updateSelectizeInput(session, "row_num", selected = up_answers$site_row), error = function(e) {NA})
  })
  observe({
    if(input$row_num != "") {
      dt_proxy <- dataTableProxy("table01")
      selectRows(dt_proxy, input$row_num)
    }
  })

  # Checklist for user inputs
  output$check_list <- renderUI({
    chk_list()
  })

  output$check_list2 <- renderUI({
    chk_list()
  })

  chk_list <- reactive({
    out_chk <- c(
      if(input$name == "") "Introduction: Name",
      if(input$id_number == "") "Introduction: ID number"
    )

    for(i in 1:nrow(answers)) {
      if(qid[i] == "q7") {
        if(is.null(input[[qid[i]]])) out_chk <- c(out_chk, answers[qid[i], 2])
      } else if(grepl("q3", qid[i])) {
        if(!("Site Selection: Objective 1 - Q.3" %in% out_chk)) {
          if(is.na(answers["q3", 1])) out_chk <- c(out_chk, answers[qid[i], 2])
        }
      } else {
        if(is.null(input[[qid[i]]])) {
          out_chk <- c(out_chk, answers[qid[i], 2])
        } else if(input[[qid[i]]] == "") {
          out_chk <- c(out_chk, answers[qid[i], 2])
        }
      }
    }

    if(length(out_chk) == 0) {
      out_chk <- "Finished! All answers have been input into the app."
    }

    HTML(
      paste(
        out_chk,
        collapse = "<br/>"
      )
    )
  })


})

# end
