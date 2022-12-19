# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE))
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE))
suppressPackageStartupMessages(library(DT, quietly = TRUE))
suppressPackageStartupMessages(library(sf, quietly = TRUE))
suppressPackageStartupMessages(library(leaflet, quietly = TRUE))
suppressPackageStartupMessages(library(plotly, quietly = TRUE))
suppressPackageStartupMessages(library(ggpubr, quietly = TRUE))
suppressPackageStartupMessages(library(kableExtra, quietly = TRUE))

# Colors for plots
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols2 <- ggthemes::ggthemes_data$colorblind$value
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]
p.cols <- RColorBrewer::brewer.pal(12, "Paired")

# Plot saving
png_dpi <- 120
p_wid <- 213
p_hei <- 120
p_units <- "mm"
l_siz <- 0.6
p_siz <- 1.6

# Functions required
source("R/download_phenocam.R")
source("R/get_html.R")
source("R/textAreaInput2.R")

# Load and format questions
quest <- read.csv("data/student_questions.csv", row.names = 1)

idx <- which(grepl("Name of selected ", quest$Question))
idx2 <- which(grepl("Elevation", quest$Question))
# Number rows
row.names(quest) <- NULL
row.names(quest)[1:(idx-1)] <- paste0("q", 1:(idx-1))
row.names(quest)[idx:(idx2)] <- paste0("q", (idx-1), letters[1:length(idx:idx2)])
row.names(quest)[(idx2+1):nrow(quest)] <- paste0("q", ((idx2+1):nrow(quest) - 6))
qid <- row.names(quest)
# Number questions
quest$Question[1:(idx-1)] <- paste0("Q.", 1:(idx-1), " ", quest$Question[1:(idx-1)])
quest$Question[idx:(idx2)] <- paste0(letters[1:length(idx:idx2)], ". ", quest$Question[idx:idx2])
quest$Question[(idx2+1):nrow(quest)] <- paste0("Q.", ((idx2+1):nrow(quest) - 6), " ", quest$Question[(idx2+1):nrow(quest)])
# Number location
quest$location[1:(idx-1)] <- paste0(quest$location[1:(idx-1)], " - Q.", 1:(idx-1))
# quest$location[idx:(idx2)] <- paste0(quest$location[idx:idx2],letters[1:length(idx:idx2)], ". ", )
quest$location[(idx2+1):nrow(quest)] <- paste0(quest$location[(idx2+1):nrow(quest)], " - Q.", ((idx2+1):nrow(quest) - 6))
# Create dataframe for answers
answers <- quest
quest$location <- NULL
colnames(answers)[1] <- "Answer"
answers[, 1] <- ""

# Slides
recap_slides <- list.files("www/shiny_slides", full.names = TRUE)
proc_uc_slides <- list.files("www/proc_uc_slides", full.names = TRUE)
param_uc_slides <- list.files("www/param_uc_slides", full.names = TRUE)
ic_uc_slides <- list.files("www/ic_uc_slides", full.names = TRUE)
driver_uc_slides <- list.files("www/driver_uc_slides", full.names = TRUE)

# Load in sp format with coordinates
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites <- neon_sites[which(neon_sites$siteID %in% c("BARC", "PRPO", "LIRO")), ]
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))

#Load in the dataframe
neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df <- neon_sites_df[which(neon_sites_df$siteID %in% c("BARC", "PRPO", "LIRO")), ]
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map

# Add type labels
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Aquatic"]))] <- "Aquatic"
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Terrestrial"]))] <- "Terrestrial"

# Subset to aquatic
neon_sites <- neon_sites[neon_sites$type == "Aquatic", ]
neon_sites_df <- neon_sites_df[neon_sites_df$type == "Aquatic", ]

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")

# Statistics
stats <- list("Minimum" = "Min.", "1st Quartile" = "1st Qu.", "Median" = "Median", "Mean" = "Mean", "3rd Quartile" = "3rd Qu.", "Maximum" = "Max.", "Standard Deviation" = "sd")
mod_choices <- c("Negative", "No change", "Positive")
# Sorting variables
state_vars <- c("Phytoplankton", "Nitrogen")
process_vars <- c("Mortality", "Uptake")

# Parameters for NP model
parms <- c(
  maxUptake = 1.0, #day-1
  kspar=120, #uEinst m-2 s-1
  ksdin=0.5, #mmol m-3
  maxGrazing=1.0, # day-1
  ksphyto=1, #mmol N m-3
  pFaeces=0.3, #unitless
  mortalityRate=0.4, #(mmmolN m-3)-1 day-1
  excretionRate=0.1, #day-1
  mineralizationRate=0.1, #day-1
  Chl_Nratio = 1, #mg chl (mmolN)-1
  Q10 = 2,  #unitless
  refTEMP = 20 # Reference temperature for q10
)

calib_model_png <- gsub("www/", "", list.files("www/calib_model/", full.names = TRUE))

# Initial conditions for NP
yini <- c(
  PHYTO = 2, #mmolN m-3
  DIN = 9) #mmolN m-3

mytheme <- theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'),
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"),
                 panel.grid.major = element_line(colour = "gray"),
                 legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))


png_theme <- theme(legend.position = "bottom",
                   legend.text = element_text(size = 14),
                   legend.title = element_text(size = 14))
# Linear regression variables ----
lin_reg_vars <- read.csv("data/multiple_linear_regression_variables.csv",
                         fileEncoding = "UTF-8-BOM")

# Forecast Date
fc_date <- "2020-09-25"

# Sampling frequency
samp_freq <- c("Monthly", "Fortnightly", "Weekly", "Daily")
samp_freq2 <- c("Month", "Fortnight", "Week", "Day")

# Uncertainty sources to include
uc_sources <- c("Process", "Parameter", "Initial Conditions", "Driver", "Total")

# Add last update time
app_time <- format(file.info("ui.R")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)

# Tab names for updating buttons
tab_names <- read.csv("data/tab_names.csv", fileEncoding = "UTF-8-BOM")

# Model names
mod_names <- c("Pers", "Wtemp", "Atemp", "Both")

# Dam levels
dam_lev <- c("Surface", "Bottom")

# Scenario Forecast #1
set.seed(123)
scen_fc1 <- data.frame(Date = seq.Date(as.Date("2021-08-16"), as.Date("2021-08-22"), by = 1),
                       surftemp = c(9.6, 9.8, 10.1, 11.1, 11.5, 11.4, 11.8),
                       bottemp = c(7.3, 7.6, 7.9, 8.2, 8.6, 9.2, 9.4))
scen_fc1$bottemp <- scen_fc1$bottemp + 1.9

bsd <- rnorm(nrow(scen_fc1), 1.2, 0.1) - 0.9
ssd <- rnorm(nrow(scen_fc1), 1.25, 0.18) - 0.9

scen_fc1$surf_uci <- scen_fc1$surftemp + ssd[order(ssd)]
scen_fc1$surf_lci <- scen_fc1$surftemp - ssd[order(ssd)]

scen_fc1$bot_uci <- scen_fc1$bottemp + bsd[order(bsd)]
scen_fc1$bot_lci <- scen_fc1$bottemp - bsd[order(bsd)]

# Scenario Forecast #2
scen_fc2 <- data.frame(Date = seq.Date(as.Date("2021-08-16"), as.Date("2021-08-22"), by = 1),
                       surftemp = c(9.6, 9.8, 10.1, 11.1, 11.5, 11.4, 11.8),
                       bottemp = c(7.3, 7.6, 7.9, 8.2, 8.6, 9.2, 9.4))
scen_fc2$bottemp <- scen_fc2$bottemp + 1.9
set.seed(123)
bsd <- rnorm(nrow(scen_fc2), 1.75, 0.6) - 0.9
ssd <- rnorm(nrow(scen_fc2), 1.2, 0.25) - 0.9

scen_fc2$surf_uci <- scen_fc2$surftemp + ssd[order(ssd)]
scen_fc2$surf_uci[5] <- scen_fc2$surftemp[5] + max(ssd)
scen_fc2$surf_lci <- scen_fc2$surftemp - ssd[order(ssd)]
scen_fc2$surf_lci[5] <- scen_fc2$surftemp[5] - max(ssd)

scen_fc2$bot_uci <- scen_fc2$bottemp + bsd[order(bsd)]
scen_fc2$bot_lci <- scen_fc2$bottemp - bsd[order(bsd)]

# end
