###############################################################
#
#      Motivations, qualifications, and strategies of science 
#               communicators on YouTube: 
#         a case study of the french ecosystem
#          Part 3: Comparison between institutions/individuals
#
###############################################################

library(nlme)
library(sfsmisc)

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

video_data <- merge(video_data, channel_data[,c("channel_id", "category")])
video_is_inst <- video_data$category == "Institution"

# Transform 0 values as NA
channel_data$channel_views[channel_data$channel_views == 0] <- NA
channel_data$channel_subscribers[channel_data$channel_subscribers == 0] <- NA

video_data$view_count[video_data$view_count == 0] <- NA

#-----------------------------
# Figure 7: Relationships
#-----------------------------

x11(width = 15)
par(mfrow = c(1,2))

#---- Regression views ~ subs
reg_inst <- lm(log10(channel_views) ~ log10(channel_subscribers), 
  data = channel_data, subset = is_institution)
reg_indiv <- lm(log10(channel_views) ~ log10(channel_subscribers), 
  data = channel_data, subset = !is_institution)

plot(channel_views ~ channel_subscribers, data = channel_data, 
  pch = ifelse(is_institution, 15, 16),
  ylab = "Number of views", xlab = "Number of subscribers", log = "xy",
  col = adjustcolor(ifelse(is_institution, 2, 4), .3))
abline(reg_inst, lwd = 2, lty = 2, col = 2)
abline(reg_indiv, lwd = 2, col = 4)
legend("topleft", c("Individuals", "Institutions"), col = adjustcolor(c(4,2), .5), 
  pch = 16:15, bty = "n", cex = 1.3)
text(10^par("usr")[1], 10^par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)
  
#---- Density mean view count / video
view_count_mean <- aggregate(view_count ~ channel_id, video_data, mean)
names(view_count_mean)[2] <- "mean_view_count"

# Add to the channel data
channel_data <- merge(channel_data, view_count_mean, sort = F)

# Compute mean count / video normalized by subs
channel_data <- within(channel_data, {
  norm_view_count <- mean_view_count / channel_subscribers
  norm_view_count[norm_view_count == Inf] <- NA
})

# Density
view_kde_indiv <- density(log10(channel_data$norm_view_count[!is_institution]),
  na.rm = T)
view_kde_inst <- density(log10(channel_data$norm_view_count[is_institution]),
  na.rm = T)

plot(0, 0, col = NA, ylim = range(c(view_kde_inst$y, view_kde_indiv$y)),
  xlim = range(c(view_kde_inst$x, view_kde_indiv$x)),
  xaxt = "n", xlab = "Mean view count / # subscribers", ylab = "Density")
polygon(view_kde_inst, col = adjustcolor(2, .5), lty = 2)
polygon(view_kde_indiv, col = adjustcolor(4, .5))
axis(1, at = axTicks(1), labels = 10^axTicks(1)) 
abline(h = 0)
legend("topright", c("Individuals", "Institutions"),
  fill = adjustcolor(c(4, 2), .5), border = NA, bty = "n", cex = 1.3)
text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)

#---- Saving the plot
dev.print(png, filename = "Figures/Fig7.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Figures/Fig7.eps", fallback_resolution = 600)


#-----------------------------
# Figure 8: Difference of engagement
#-----------------------------

x11(width = 10)
layout(matrix(c(1,1,2,2,0,3,3,0), nrow = 2, byrow = T))

#---- Distribution likes / views
# Ratio computation
likes_views <- with(video_data, like_count / view_count)

# Density
likvi_kde_indiv <- density(likes_views[!video_is_inst], na.rm = T, 
  from = 0, to = 1)
likvi_kde_indiv$y <- c(0, likvi_kde_indiv$y)
likvi_kde_indiv$x <- c(0, likvi_kde_indiv$x)
likvi_kde_inst <- density(likes_views[video_is_inst], na.rm = T, 
  from = 0, to = 1)
likvi_kde_inst$y <- c(0, likvi_kde_inst$y)
likvi_kde_inst$x <- c(0, likvi_kde_inst$x)

integrate.xy(likvi_kde_indiv$x, likvi_kde_indiv$y, a = 0.05)
mean(likes_views[!video_is_inst] > 0.05, na.rm = T)

# Plot
plot(0, 0, col = NA, ylim = range(c(likvi_kde_inst$y, likvi_kde_indiv$y)),
  xlim = c(0, .2), cex.lab = 1.5,
  xaxt = "n", xlab = "Like count / view count", ylab = "Density")
polygon(likvi_kde_inst, col = adjustcolor(2, .5), lty = 2)
polygon(likvi_kde_indiv, col = adjustcolor(4, .5))
axis(1, at = axTicks(1), labels = axTicks(1)) 
abline(h = 0)
legend("topright", c("Individuals", "Institutions"),
  fill = adjustcolor(c(4, 2), .5), border = NA, bty = "n", cex = 1.5)
text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)

#---- Distribution likes / dislikes
likes_dislikes <- with(video_data, like_count / dislike_count)

# Density
likdis_kde_indiv <- density(likes_dislikes[!video_is_inst], na.rm = T, 
  from = 0)
likdis_kde_indiv$y <- c(0, likdis_kde_indiv$y)
likdis_kde_indiv$x <- c(0, likdis_kde_indiv$x)
likdis_kde_inst <- density(likes_dislikes[video_is_inst], na.rm = T, 
  from = 0)
likdis_kde_inst$y <- c(0, likdis_kde_inst$y)
likdis_kde_inst$x <- c(0, likdis_kde_inst$x)

plot(0, 0, col = NA, ylim = range(c(likdis_kde_inst$y, likdis_kde_indiv$y)),
  xlim = c(0, 500), cex.lab = 1.5,
  xaxt = "n", xlab = "Like count / dislike count", ylab = "Density")
polygon(likdis_kde_inst, col = adjustcolor(2, .5), lty = 2)
polygon(likdis_kde_indiv, col = adjustcolor(4, .5))
axis(1, at = axTicks(1), labels = axTicks(1)) 
abline(h = 0)
legend("topright", c("Individuals", "Institutions"),
  fill = adjustcolor(c(4, 2), .5), border = NA, bty = "n", cex = 1.5)
text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)

#---- Distribution comments / views

comment_view <- with(video_data, comment_count / view_count)

# Density
comview_kde_indiv <- density(comment_view[!video_is_inst], na.rm = T, 
  from = 0, to = .05)
comview_kde_indiv$y <- c(0, comview_kde_indiv$y)
comview_kde_indiv$x <- c(0, comview_kde_indiv$x)
comview_kde_inst <- density(comment_view[video_is_inst], na.rm = T, 
  from = 0, to = .05)
comview_kde_inst$y <- c(0, comview_kde_inst$y)
comview_kde_inst$x <- c(0, comview_kde_inst$x)

plot(0, 0, col = NA, ylim = c(0, 500),
  xlim = c(0,.05), cex.lab = 1.5,
  xaxt = "n", xlab = "Comment count / view count", ylab = "Density")
polygon(comview_kde_inst, col = adjustcolor(2, .5), lty = 2)
polygon(comview_kde_indiv, col = adjustcolor(4, .5))
axis(1, at = axTicks(1), labels = axTicks(1)) 
abline(h = 0)
legend("topright", c("Individuals", "Institutions"),
  fill = adjustcolor(c(4, 2), .5), border = NA, bty = "n", cex = 1.5)
text(par("usr")[1], par("usr")[4], "C", adj = c(2, -1.2), xpd = T, cex = 3)

dev.print(png, filename = "Figures/Fig8.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Figures/Fig8.eps", fallback_resolution = 600)



