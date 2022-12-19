library(shiny)
library(plotly)
library(DT)

df <- data.frame(x = rnorm(365, 9, 5),
                 y = rnorm(365, 12, 7))

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

ui <- fluidPage(
  plotlyOutput("plot1"),
  numericInput("m", "Slope (m)", value = 1, min = -2, max = 2, step = 0.1),
  actionButton("draw_line", "Draw line"),
  numericInput("b", "Intercept (b)", value = 0, min = -15, max = 15, step = 0.1),
  actionButton("save_line", "Save line"),
  DTOutput("lr_DT", width = "40%"),
  p("Calculate the mean and standard deviation of the parameters"),
  actionButton("calc_stats", "Calculate!"),
  DTOutput("lr_stats", width = "40%"),
  plotOutput("lr_par_dist_plot"),
  actionButton("gen_lr_dist_plot", "Generate plot!"),
  radioButtons("n_samp", "No. of samples", choices = c(1, 10, 50, 100)),
  actionButton("gen_lin_mods", "Add lines"),
  plotlyOutput("add_lin_mods"),
  actionButton("gen_lm_plot", "Add lm plot"),
  plotlyOutput("lm_plot")

)

server <- function(input, output, session) {

  reg_line <- reactiveValues(m = 1, b = 0)
  sav_lines <- reactiveValues(m = 1, b = 0)
  observeEvent(input$draw_line, {
    reg_line$m <- input$m
    reg_line$b <- input$b
  })

  lr_pars <- reactiveValues(dt = data.frame(m = rep(NA, 10), b = rep(NA, 10)))

  output$lr_DT <- renderDT(lr_pars$dt, selection = "single",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                              ), colnames = c("Slope (m)", "Intercept (b)"),
                              server = FALSE, escape = FALSE)

  observeEvent(input$save_line, {
    if(input$save_line == 1) {
      sav_lines$m <- input$m
      sav_lines$b <- input$b
    } else {
      sav_lines$m <- c(sav_lines$m, input$m)
      sav_lines$b <- c(sav_lines$b, input$b)
    }
    if(!is.null(input$lr_DT_rows_selected)) {
      lr_pars$dt$m[input$lr_DT_rows_selected] <- input$m
      lr_pars$dt$b[input$lr_DT_rows_selected] <- input$b
    } else {
      idx <- which(is.na(lr_pars$dt$m))[1]
      lr_pars$dt$m[idx] <- input$m
      lr_pars$dt$b[idx] <- input$b
    }
  })
  output$plot1 <- renderPlotly({
    p <- ggplot(df) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(aes(x, y), color = "black") +
      coord_cartesian(xlim = c(-5, 25), ylim = c(-5, 25)) +
      theme_minimal(base_size = 12)

    if(input$draw_line > 0) {
      p <- p +
        geom_abline(slope = reg_line$m, intercept = reg_line$b, color = "gray", linetype = "dashed")
    }
    if(input$save_line > 0) {
      p <- p +
        geom_abline(slope = lr_pars$dt$m, intercept = lr_pars$dt$b, color = "red", linetype = "solid")
    }
    return(ggplotly(p, dynamicTicks = TRUE))
  })

  linr_stats <- reactiveValues(dt = data.frame("Mean (m)" = 0,
                                               "Std. Dev (m)" = 0,
                                               "Mean (b)" = 0,
                                               "Std. Dev (b)" = 0))
  observeEvent(input$calc_stats, {
    req(sum(!is.na(lr_pars$dt$m)) > 1)
    df <- data.frame("Mean (m)" = mean(lr_pars$dt$m, na.rm = TRUE),
                     "Std. Dev (m)" = sd(lr_pars$dt$m, na.rm = TRUE),
                     "Mean (b)" = mean(lr_pars$dt$b, na.rm = TRUE),
                     "Std. Dev (b)" = sd(lr_pars$dt$b, na.rm = TRUE))
    linr_stats$dt <- signif(df, 3)
  })

  output$lr_stats <- renderDT(linr_stats$dt, selection = "none",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                              ),
                              colnames = c("Mean (m)", "Std. Dev (m)", "Mean (b)", "Std. Dev (b)"),
                              rownames = FALSE, container = sketch1,
                              server = FALSE, escape = FALSE)

  lr_dist_plot <- reactiveValues(m = NA, b = NA)
  observeEvent(input$gen_lr_dist_plot, {
    lr_dist_plot$m <- rnorm(500, mean = linr_stats$dt[1, 1], sd = linr_stats$dt[1, 2])
    lr_dist_plot$b <- rnorm(500, mean = linr_stats$dt[1, 3], sd = linr_stats$dt[1, 4])
  })

  output$lr_par_dist_plot <- renderPlot({
    validate(
      need(input$gen_lr_dist_plot > 0, "Click 'Generate plot!'")
    )
    df1 <- data.frame(par = "Slope (m)", value = lr_dist_plot$m)
    df2 <- data.frame(par = "Intercept (b)", value = lr_dist_plot$b)
    df <- rbind.data.frame(df1, df2)
    dat <- data.frame(x = c(linr_stats$dt[1, 1], linr_stats$dt[1, 3]),
                      par = c("Slope (m)", "Intercept (b)"))

    p <- ggplot(df) +
      geom_vline(data = dat, aes(xintercept = x)) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      facet_wrap(~par, scales = "free") +
      theme_minimal(base_size = 22)
    return(p)
  })

  mb_samples <- reactiveValues(df = NULL)
  observeEvent(input$gen_lin_mods, {
    mb_samples$df <- data.frame("m" = sample(lr_dist_plot$m, input$n_samp),
                                "b" = sample(lr_dist_plot$b, input$n_samp))
  })

  output$add_lin_mods <- renderPlotly({
    p <- ggplot(df) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(aes(x, y), color = "black") +
      coord_cartesian(xlim = c(-5, 25), ylim = c(-5, 25)) +
      theme_minimal(base_size = 12)

    if(!is.null(mb_samples$df)) {
      p <- p +
        geom_abline(slope = mb_samples$df$m, intercept = mb_samples$df$b, color = "gray", linetype = "solid")
    }
    return(ggplotly(p, dynamicTicks = TRUE))
  })

  lm_plot <- reactiveValues(p = NULL)
  observeEvent(input$gen_lm_plot, {
    lm_plot$p <- ggplot(df) +
      geom_smooth(aes(x, y), method = "lm") +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(aes(x, y), color = "black") +
      coord_cartesian(xlim = c(-5, 25), ylim = c(-5, 25)) +
      theme_minimal(base_size = 12)
  })
  output$lm_plot <- renderPlotly({
    validate(
      need(input$gen_lm_plot > 0, "Click add lm plot")
    )
    ggplotly(lm_plot$p, dynamicTicks = TRUE)
  })
}

shinyApp(ui, server)

