###############################################################
#
#      Motivations, qualifications, and strategies of science 
#               communicators on YouTube: 
#         a case study of the french ecosystem
#                 Part 2: Channel description
#
###############################################################

library(DescTools) # For Gini coefficient
library(packcircles)
library(RColorBrewer)
library(fields)
library(ggplot2)

source("0_UsefulFunctions.R")

#-----------------------------
#  Data reading and preparation
#-----------------------------

#----- API data
video_data <- read.csv("Data/all_videos.csv", header = T, sep = ",")
channel_data <- read.csv("Data/all_channels.csv", header = T, sep = ",")

# Date variables as Date objects
video_data$video_date <- as.POSIXlt(video_data$video_date,
  format = "%Y-%m-%dT%H:%M:%SZ")
video_data$statistics_accessed_on <- as.POSIXlt(video_data$statistics_accessed_on)

channel_data$channel_creation <- as.POSIXlt(channel_data$channel_creation,
  format = "%Y-%m-%dT%H:%M:%SZ")
  
# Boolean for institutions
is_institution <- channel_data$category == "Institution"

# Transform 0 values as NA
channel_data$channel_views[channel_data$channel_views == 0] <- NA
channel_data$channel_subscribers[channel_data$channel_subscribers == 0] <- NA

#----- Survey data
datatab <- readRDS("Data/Survey_data.RDS")

# Boolean for institutions
is_inst_survey <- datatab$institution == "Institution"

#-----------------------------
# Figure 3: Channel creation date
#-----------------------------  

# Initialize plot
x11(width = 10)

#----- Channel creation and first uploaded video
# Keep first video for each channel
first_vid_date <- tapply(video_data$video_date, video_data$channel_id, min, 
  simplify = F)
channel_data$first_video <- as.POSIXlt(unlist(sapply(first_vid_date, as.character))[channel_data$channel_id])

# Years covered
minyear <- as.POSIXlt(min(channel_data$channel_creation))$year + 1900
dateSeq <- seq.POSIXt(as.POSIXlt(sprintf("%i-01-01", minyear)), 
  as.POSIXlt("2021-01-01"), by = "year")

#--- Density of channel creation
# Institution
cc_kde_inst <- density(sapply(channel_data$channel_creation[is_institution], as.numeric), 
  from = as.numeric(dateSeq[1]), to = as.numeric(dateSeq[length(dateSeq)]))
cc_kde_inst$y[c(1, length(cc_kde_inst$y))] <- 0

# Individuals
cc_kde_indiv <- density(sapply(channel_data$channel_creation[!is_institution], as.numeric), 
  from = as.numeric(dateSeq[1]), to = as.numeric(dateSeq[length(dateSeq)]))
cc_kde_indiv$y[c(1, length(cc_kde_indiv$y))] <- 0

#--- Plot
plot(0, 0, col = NA, xlim = range(dateSeq), 
  ylim = range(c(cc_kde_inst$y, cc_kde_indiv$y)),
  xaxt = "n", xlab = "Date", ylab = "Density")
polygon(cc_kde_inst, col = adjustcolor(2, .5), lty = 2)
polygon(cc_kde_indiv, col = adjustcolor(4, .5))
axis.intervals(1, as.numeric(dateSeq), labels = minyear:2020, las = 3)
abline(h = 0)
legend("topleft", c("Individuals", "Institutions"),
  fill = c(4, 2), border = NA, bty = "n")
  
#--- Saving the plot
dev.print(png, filename = "Figures/Fig3.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Figures/Fig3.eps", fallback_resolution = 600)

#--- Density of first video
# Institution
#fv_kde_inst <- density(sapply(channel_data$first_video[is_institution], as.numeric), 
#  from = as.numeric(dateSeq[1]), to = as.numeric(dateSeq[length(dateSeq)]))
#fv_kde_inst$y[c(1, length(cc_kde_inst$y))] <- 0
#fv_kde_inst$y <- -fv_kde_inst$y
#
## Individuals
#fv_kde_indiv <- density(sapply(channel_data$first_video[!is_institution], as.numeric), 
#  from = as.numeric(dateSeq[1]), to = as.numeric(dateSeq[length(dateSeq)]))
#fv_kde_indiv$y[c(1, length(cc_kde_indiv$y))] <- 0
#fv_kde_indiv$y <- -fv_kde_indiv$y

#--- Plot
#plot(0, 0, col = NA, xlim = range(dateSeq), main = "Channel creation", 
#  ylim = range(c(cc_kde_inst$y, cc_kde_indiv$y, fv_kde_inst$y, fv_kde_indiv$y)),
#  xaxt = "n", yaxt = "n", xlab = "Date", ylab = "Density")
#polygon(cc_kde_inst, col = adjustcolor(2, .5), lty = 2)
#polygon(cc_kde_indiv, col = adjustcolor(4, .5))
#polygon(fv_kde_inst, col = adjustcolor(2, .5), lty = 2)
#polygon(fv_kde_indiv, col = adjustcolor(4, .5))
#axis.intervals(1, as.numeric(dateSeq), labels = minyear:2020, las = 3)
#axis(2, at = axTicks(2), labels = abs(axTicks(2)))
#abline(h = 0)
#legend("topleft", c("Individuals", "Institutions"),
#  fill = c(4, 2), border = NA, bty = "n")
#mtext(c("First video", "Channel creation"), side = 4, at = par("usr")[3:4] / 2,
#  cex = 1, line = -1.5)
#  
##--- Saving the plot
#dev.print(png, filename = "Figures/Fig3.png", units = "in",
#  res = 200)
#dev.print (pdf, file = "Figures/Fig3.pdf")

#-----------------------------
# Figure 4: Publication frequency
#-----------------------------  

# From API data
channel_age <- (as.POSIXlt("2020-08-05") - channel_data$channel_creation) / 365.25
nvideo <- tapply(video_data$video_id, video_data$channel_id, length)
nvideo <- nvideo[channel_data$channel_id]
APIfreq <- nvideo / as.numeric(channel_age)

# Summary statistics for publication frequency
tapply(APIfreq, is_institution, mean)
tapply(APIfreq, is_institution, sd)

# Density frequency API data
API_kde_inst <- density(APIfreq[is_institution], from = 0)
API_kde_inst$y[c(1, length(API_kde_inst$y))] <- 0

API_kde_indiv <- density(APIfreq[!is_institution], from = 0, adjust = 2)
API_kde_indiv$y[c(1, length(API_kde_indiv$y))] <- 0

# Density frequency Survey data
survey_kde_inst <- density(datatab$pubFrequency[is_inst_survey], 
  from = 0, na.rm = T)
survey_kde_inst$y[c(1, length(survey_kde_inst$y))] <- 0
survey_kde_inst$y <- -survey_kde_inst$y

survey_kde_indiv <- density(datatab$pubFrequency[!is_inst_survey], 
  from = 0, na.rm = T, adjust = 2)
survey_kde_indiv$y[c(1, length(survey_kde_indiv$y))] <- 0
survey_kde_indiv$y <- -survey_kde_indiv$y

#---- Plot the densities
x11(width = 10)
plot(0, 0, col = NA, main = "Publication frequency", 
#  xlim = range(c(APIfreq, datatab$pubFrequency), na.rm = T),
  xlim = c(0, 130),
  ylim = range(c(API_kde_inst$y, API_kde_indiv$y, survey_kde_inst$y, survey_kde_indiv$y)),
  yaxt = "n", xlab = "Videos / year", ylab = "Density")
polygon(API_kde_inst, col = adjustcolor(2, .5), lty = 2)
polygon(API_kde_indiv, col = adjustcolor(4, .5))
polygon(survey_kde_inst, col = adjustcolor(2, .5), lty = 2)
polygon(survey_kde_indiv, col = adjustcolor(4, .5))
axis(2, at = axTicks(2), labels = abs(axTicks(2)))
abline(h = 0)
legend("topright", c("Individuals", "Institutions"),
  fill = c(4, 2), border = NA, bty = "n")
mtext(c("Survey", "API"), side = 4, at = par("usr")[3:4] / 2,
  cex = 1, line = -1.5)
# Add the number
APIgt <- APIfreq > par("usr")[2]
if (any(APIgt)){
  text(par("usr")[2], par("usr")[4] / 4, sprintf("%i \U2192", sum(APIgt)),
    adj = c(2, .5), cex = 1.5)
}
surveygt <- datatab$pubFrequency > par("usr")[2]
if (any(surveygt)){
  text(par("usr")[2], par("usr")[3] / 4, sprintf("%i \U2192", 
    sum(surveygt, na.rm = T)), adj = c(1.5, .5), cex = 1.5)
}

#---- Saving the plot
dev.print(png, filename = "Figures/Fig4.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Figures/Fig4.eps", fallback_resolution = 600)

#-----------------------------
# Figure 5: Represented scientific fields
#-----------------------------  

# number of answers with field
nrep_field <- sum(sapply(datatab$field, length) > 0)

# Count How many time each field appears for institutions and individuals
field_inst <- table(unlist(datatab$field[is_inst_survey]))
field_inst_prop <- field_inst / sum(is_inst_survey)

field_indiv <- table(unlist(datatab$field[!is_inst_survey]))
field_indiv_prop <- field_indiv / (nrow(datatab) - sum(is_inst_survey))

# Remove those that are null
which_null <- field_inst_prop == 0 & field_indiv_prop == 0
field_indiv_prop <- field_indiv_prop[!which_null]
field_inst_prop <- field_inst_prop[!which_null]

ord <- order(field_indiv, decreasing = TRUE)

# Barplot
x11(width = 10)
par(mar = c(10, 4, 4, 2) + .1)
bp <- barplot(100 * field_indiv_prop[ord], col = adjustcolor(4, .5), 
  ylab = "Proportion(%)", cex.lab = 1.3, cex.names = 1.1, cex.axis = 1.2,
  border = NA, las = "3", 
  ylim = c(-60, 40), yaxt = "n")
barplot(-100 * field_inst_prop[ord], col = adjustcolor(2, .5), border = NA, 
  add = T, names.arg = NA, axes = F)
abline(h = 0)
axis(2, at = axTicks(2), labels = abs(axTicks(2)), cex.axis = 1.2)
mtext(c("Institutions", "Individuals"), side = 4, at = par("usr")[3:4] / 2,
  line = 1, cex = 1.3, col = c(2, 4))
text(par("usr")[2], par("usr")[4], 
  labels = sprintf("n = %i", nrow(datatab) - sum(is_inst_survey)), 
  adj = c(1.2, 1), cex = 1.5)
text(par("usr")[2], par("usr")[3], 
  labels = sprintf("n = %i", sum(is_inst_survey)), 
  adj = c(1.2, 0), cex = 1.5)

#--- Saving the plot
dev.print(png, filename = "Figures/Fig5.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Figures/Fig5.eps", fallback_resolution = 600)

#-----------------------------
# Figure 6: number of subscribers
#----------------------------- 

# Compute densities
subs_kde_indiv <- density(log10(channel_data$channel_subscribers[!is_institution]),
  na.rm = T)
subs_kde_inst <- density(log10(channel_data$channel_subscribers[is_institution]),
  na.rm = T)

# Mode of the distributions
10^subs_kde_indiv$x[which.max(subs_kde_indiv$y)]
10^subs_kde_inst$x[which.max(subs_kde_inst$y)]

# x11()
# plot(0, 0, col = NA, 
#   xlim = range(c(subs_kde_inst$x, subs_kde_indiv$x)),
#   ylim = range(c(subs_kde_inst$y, subs_kde_indiv$y)),
#   xaxt = "n", xlab = "Number of subscribers", ylab = "Density")
# polygon(subs_kde_inst, col = adjustcolor(2, .5), lty = 2)
# polygon(subs_kde_indiv, col = adjustcolor(4, .5))
# abline(h = 0)
# axis(1, at = axTicks(1), labels = 10^axTicks(1)) 
# legend("topright", c("Individuals", "Institutions"),
#   fill = c(4, 2), border = NA, bty = "n")
# 
# #---- Saving the plot  
# dev.print(png, filename = "Figures/Fig6.png", units = "in",
#   res = 200)
# dev.print(cairo_ps, file = "Figures/Fig6.eps", fallback_resolution = 600)

# Mode of the number of view
view_kde_indiv <- density(log10(channel_data$channel_views[!is_institution]),
  na.rm = T)
view_kde_inst <- density(log10(channel_data$channel_views[is_institution]),
  na.rm = T)

10^view_kde_indiv$x[which.max(view_kde_indiv$y)]
10^view_kde_inst$x[which.max(view_kde_inst$y)]

#---- Plot ECDF
# subs_cdf_indiv <- ecdf(log10(channel_data$channel_subscribers[!is_institution]))
# subs_cdf_inst <- ecdf(log10(channel_data$channel_subscribers[is_institution]))
# 
# x11()
# plot(subs_cdf_inst, col = 2, verticals = F, xlim = c(0, 7), axes = F,
#   main = "", ylab = "Proportion below (%)", xlab = "Number of subscribers")
# plot(subs_cdf_indiv, col = 4, add = T)
# axis(1, at = axTicks(1), labels = 10^axTicks(1))
# axis(2, at = axTicks(2), labels = 100 * axTicks(2))
# legend("topleft", c("Individuals", "Institutions"),
#   col = c(4, 2), pch = 16, bty = "n", inset = .05)
# grid()

x11(width = 15, height = 15)
par(mfrow = c(2,2))

#----- Panel A: Histogram -----
# We consider log10 of subs
logsubs <- na.omit(log10(channel_data$channel_subscribers))
# Breaks for the histogram
hbr <- seq(0, ceiling(max(logsubs)), by = 1)
# Compute histogram
hsub_indiv <- hist(logsubs[!is_institution], breaks = hbr, plot = F)
hsub_inst <- hist(logsubs[is_institution], breaks = hbr, plot = F)

# Plot histogram
plot(hsub_indiv, col = adjustcolor(4, .5), freq = F, border = "white",
  xlim = range(hbr), ylim = c(0, .4), xaxt = "n", yaxt = "n",
  main = "Subscriber histogram", xlab = "Number of subscribers", 
  ylab = "Proportion (%)"
)
plot(hsub_inst, col = adjustcolor(2, .5), freq = F, add = T, border = "white")
axis(1, at = hbr, labels = 10^hbr)
axis(2, at = axTicks(2), labels = axTicks(2) * 100)
text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)
legend("topright", c("Individuals", "Institutions"),
  fill = adjustcolor(c(4, 2), .5), border = NA, bty = "n")

#----- Panel B: Lorenz curves showing inequalities -----
# Lorenz curves
lorenz_indiv <- cumsum(sort(channel_data$channel_subscribers[!is_institution]))
lorenz_indiv <- 100* lorenz_indiv / max(lorenz_indiv)
lorenz_inst <- cumsum(sort(channel_data$channel_subscribers[is_institution]))
lorenz_inst <- 100 * lorenz_inst / max(lorenz_inst)

# Gini coef
gini_indiv <- Gini(channel_data$channel_subscribers[!is_institution], na.rm = T)
gini_inst <- Gini(channel_data$channel_subscribers[is_institution], na.rm = T)

# plot it
plot(seq(0, 100, length.out = length(lorenz_indiv)), lorenz_indiv, type = "l",
  col = 4, lwd = 3, ylab = "Cumulative subscriber proportion (%)", 
  xlab = "Channel quantile (%)", xaxs = "i", yaxs = "i",
  main = "Inequality curve")
lines(seq(0, 100, length.out = length(lorenz_indiv)), lorenz_indiv, 
  col = 2, lwd = 3, lty = 2)
abline(a = 0, b = 1)
legend("topleft", sprintf("%s: %0.2f", c("Individuals", "Institutions"),
    c(gini_indiv, gini_inst), digits = 2),
  title = "Gini coefficient", lty = 1:2, col = c(4,2),
  bty = "n", inset = 0.02)
text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Panel C and D: PackCircles -----
packing_indiv <- circleProgressiveLayout(channel_data$channel_subscribers[!is_institution], 
  sizetype='area')
poly_indiv <- circleLayoutVertices(packing_indiv, npoints = 50)
poly_indiv <- split(poly_indiv[,1:2], poly_indiv[,3])
col_indiv <- rev(brewer.pal(9, "Blues")[-1])[cut(as.numeric(names(poly_indiv)), 8)]
indiv_lims <- with(packing_indiv, 
  range(c(x + radius, x - radius, y + radius, y - radius), na.rm = T))

packing_inst <- circleProgressiveLayout(channel_data$channel_subscribers[is_institution], 
  sizetype='area')
poly_inst <- circleLayoutVertices(packing_inst, npoints = 50)
poly_inst <- split(poly_inst[,1:2], poly_inst[,3])
col_inst <- rev(brewer.pal(9, "Reds")[-1])[cut(as.numeric(names(poly_inst)), 8)]
inst_lims <- with(packing_inst, 
  range(c(x + radius, x - radius, y + radius, y - radius), na.rm = T))

## Plot individuals
par(mar = c(4, 4, 4, 4))
plot(0, 0, col = NA, bty = "n", axes = F, xlab = "", ylab = "", 
  xlim = indiv_lims, ylim = indiv_lims, main = "Individuals")
invisible(Map(polygon, poly_indiv, col = col_indiv))
text(packing_indiv$x, packing_indiv$y, 
  formatC(channel_data$channel_subscribers[!is_institution], big.mark = " "),
  cex = packing_indiv$radius / max(packing_indiv$radius, na.rm = T))
text(par("usr")[1], par("usr")[4], "C", adj = c(2, -1.2), xpd = T, cex = 3)

## Plot institutions
par(mar = c(0, 4, 4, 4))
plot(0, 0, col = NA, bty = "n", axes = F, xlab = "", ylab = "", 
  xlim = inst_lims, ylim = inst_lims, main = "Institutions")
invisible(Map(polygon, poly_inst, col = col_inst))
text(packing_inst$x, packing_inst$y, 
  formatC(channel_data$channel_subscribers[is_institution], big.mark = " "),
  cex = 1.5 * packing_inst$radius / max(packing_inst$radius, na.rm = T))
text(par("usr")[1], par("usr")[4], "D", adj = c(2, -1.2), xpd = T, cex = 3)


#Saving the plot  
dev.print(png, filename = "Figures/Fig6.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Figures/Fig6.eps", fallback_resolution = 600)
  
#-----------------------------
# Correlation between #subs and channel creation
#-----------------------------    

# Age of the channel
channel_age <- (as.POSIXlt("2020-08-05") - channel_data$channel_creation) / 365.25
channel_age <- as.numeric(channel_age)

#--- Correlation tests
pearson <- cor.test(~ channel_age + log10(channel_subscribers), 
  data = channel_data, na.action = na.omit, subset = channel_subscribers > 0)
  

spearman <- cor.test(~ channel_age + log10(channel_subscribers), 
  data = channel_data, na.action = na.omit, subset = channel_subscribers > 0,
  method = "spearman")
