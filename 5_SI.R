###############################################################
#
#      Motivations, qualifications, and strategies of science 
#               communicators on YouTube: 
#         a case study of the french ecosystem
#                 Part 4: supplementary figures
#
###############################################################

library(packcircles)
library(RColorBrewer)
library(DescTools)
library(nlme)
library(fields)

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

video_data$view_count[video_data$view_count == 0] <- NA

#----- Survey data
datatab <- readRDS("Data/Survey_data.RDS")

# Boolean for institutions
is_inst_survey <- datatab$institution == "Institution"


#-----------------------------
# Supplementary Figure 1 : financial situation of institutions
#----------------------------- 

# /!\ Cannot be reproduced with the shared data as info on income have be removed for confidentiality reasons

x11(width = 10, height = 7)
layout(matrix(c(1,1,2,2,0,3,3,0), nrow = 2, byrow = T))

#----- Balance -----
bal_table <- table(datatab$balance[datatab$institution == "Institution"])
bal_prop <- 100 * bal_table / sum(bal_table)

bp <- barplot(bal_prop, col = 4, border = NA, ylab = "Proportion (%)", 
  cex.lab = 1.3, cex.axis = 1.2, cex.names = 1.3, main = "Balance")
text(bp, bal_prop, sprintf("%i %%", round(bal_prop)), pos = 3, cex = 1.5, xpd = T)
text(par("usr")[2], par("usr")[4], labels = sprintf("n = %i", sum(bal_table)),
  adj = c(1.2, 1.5), cex = 1.5, xpd = T)
text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Funding sources -----
levs <- levels(datatab$funding[[1]])
prop_fund <- 100 * sapply(levs,
  function(l) mean(sapply(datatab$funding[datatab$institution == "Institution"], 
    function(f) l %in% f))
)
prop_fund <- sort(prop_fund, decreasing = TRUE)
prop_fund <- c(prop_fund[!names(prop_fund) %in% c("None", "Other")],
  prop_fund["None"])

#par(mar = c(7, 4, 4, 2) + .1)
bp <- barplot(prop_fund, col = 4, border = NA, ylab = "Proportion (%)", 
  cex.lab = 1.3, cex.axis = 1.2, cex.names = 1.3, main = "Income sources", 
  las = 3, xpd = T)
text(bp, prop_fund, sprintf("%i %%", round(prop_fund)), pos = 3, xpd = T)
text(par("usr")[2], par("usr")[4], 
  labels = sprintf("n = %i", sum(!is.na(datatab$funding[datatab$institution == "Institution"]))),
  adj = c(1.2, 1.5), cex = 1.5, xpd = T)
text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Income -----
# By how many the income must be divided
npeople_income <- as.numeric(datatab$npeople) - 1
npeople_income[npeople_income == 0] <- 1

# Divide income
income_norm <- datatab$income / npeople_income

# Count in each category
par(mar = c(5, 4, 4, 4) + .1)
h <- hist(log10(income_norm[datatab$institution == "Institution"] + 1), 
  breaks = -1:4, plot = F)
hc <- h$counts
h$counts <- round(100 * h$density, digits = 1)

# plot histogram
plot(h, xaxt = "n", labels = sprintf("%2.0f %%", h$counts), col = 4, border = NA, 
  ylab = "Proportion (%)", main = "Monthly income", xlab = "Normalized income (%)",
  cex.lab = 1.3, cex.axis = 1.2, ylim = c(0, 110))
axis(1, at = -1:4, cex.axis = 1.2,
  labels = formatC(c(0, 10^(0:4)), format = "fg", big.mark = " "))
text(par("usr")[1], par("usr")[4], "C", adj = c(2, -1.2), xpd = T, cex = 3)
text(par("usr")[2], par("usr")[4], labels = sprintf("n = %i", sum(hc)),
  adj = c(1.2, 1.5), cex = 1.5, xpd = T)

dev.print(png, filename = "Supplementary_figures/SF1.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF1.eps")

#--------------------------
#  Supplementary Figure 2: whether communicators wish to professionalize
#--------------------------

#----- Pro wish -----
# table
wish_count <- table(datatab$prowish)
wish_prop <- wish_count / sum(wish_count)

# Barplot
x11()
bp <- barplot(100 * wish_prop, col = 4, border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1, cex.axis = 1.2,
  main = "Income objective", ylim = c(0, 40))
text(bp, 100 * wish_prop, sprintf("%i %%", round(100 * wish_prop)), 
  pos = 3, xpd = T, cex = 1.5)
text(par("usr")[1], par("usr")[4], labels = sprintf("n = %i", sum(wish_count)),
  adj = c(-0.2, 1.5), cex = 1.5)

# Save the plot
dev.print(png, filename = "Supplementary_figures/SF2.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF2.eps")


#-----------------------------
# Supplementary Figure 3: audience age distribution according to targets
#----------------------------- 

# Determine which channel has which target
which_target <- sapply(levels(datatab$target[[1]]), function(x)
  sapply(datatab$target, function(y) any(y == x))
)
mean_dist <- apply(which_target, 2, function(x){
  prop <- aggregate(datatab[,grep("Prop", names(datatab))[-1]], by = list(x),
    mean, na.rm = T)[2,-1]
  100 * unlist(prop) / sum(unlist(prop))
})

# Audience age distribution
x11(width = 10)
par(mfrow = c(2,2), cex.lab = 1.3)
for (i in 1:nlevels(datatab$target[[1]])){
  bp <- barplot(mean_dist[,i], col = 4, border = NA, ylim = c(0, 40), 
    names.arg = gsub("Prop", "", rownames(mean_dist)), 
    ylab = "Proportion (%)", main = colnames(mean_dist)[i])
  text(bp, mean_dist[,i], pos = 3, sprintf("%i", round(mean_dist[,i])), xpd = T)
  text(par("usr")[2], par("usr")[4], sprintf("n = %i", sum(which_target[,i], na.rm = T)),
    adj  = c(1, 0), xpd = T, cex = 1.3)
  text(par("usr")[1], par("usr")[4], LETTERS[i], adj = c(2, -1.2), xpd = T, cex = 3)  
}

dev.print(png, filename = "Supplementary_figures/SF3.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF3.eps")

#--------------------------
#  Supplementary Figure 4: objectives of institutional channels
#--------------------------

x11(width = 15, height = 10)
layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = T))

#----- Institutions: channel objective -----
inst_goal_count <- table(datatab$institutionGoal)
inst_goal_prop <- inst_goal_count / sum(inst_goal_count)

# Barplot
bp <- barplot(100 * inst_goal_prop, col = 4, border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1, cex.axis = 1.2,
  main = "Channel promotes", ylim = c(0, 60))
text(bp, 100 * inst_goal_prop, sprintf("%i %%", round(100 * inst_goal_prop)), 
  pos = 3, xpd = T, cex = 1.5)
text(par("usr")[1], par("usr")[4], labels = sprintf("n = %i", sum(inst_goal_count)),
  adj = c(-0.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Institutions: channel priority -----
h <- hist(datatab$priority, breaks = 0:5 + .5, plot = F)

bp <- barplot(100 * h$density, col = 4, border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1.3, cex.axis = 1.2,
  main = "Priority score", names.arg = h$mids, ylim = c(0, 40))
mtext(c("Low priority", "High priority"), side = 1, at = bp[c(1,5)], line = 2.5)
text(bp, 100 * h$density, sprintf("%i %%", round(100 * h$density)), 
  pos = 3, xpd = T, cex = 1.5)
text(par("usr")[1], par("usr")[4], labels = sprintf("n = %i", sum(h$counts)),
  adj = c(-0.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Institutions: channel satisfaction -----
h <- hist(datatab$satisfaction, breaks = 0:5 + .5, plot = F)

bp <- barplot(100 * h$density, col = 4, border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1.3, cex.axis = 1.2,
  main = "Satisfaction score", names.arg = h$mids, ylim = c(0, 40))
mtext(c("Not satisfied at all", "Very statisfied"), side = 1, at = bp[c(1,5)], 
  line = 2.5)
text(bp, 100 * h$density, sprintf("%i %%", round(100 * h$density)), 
  pos = 3, xpd = T, cex = 1.5)
text(par("usr")[1], par("usr")[4], labels = sprintf("n = %i", sum(h$counts)),
  adj = c(-0.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "C", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Institutions: feedback -----
h <- hist(datatab$feedback, breaks = 0:5 + .5, plot = F)

bp <- barplot(100 * h$density, col = 4, border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1.3, cex.axis = 1.2,
  main = "Feedback score", names.arg = h$mids, ylim = c(0, 40))
mtext(c("No, none", "Yes, a lot"), side = 1, at = bp[c(1,5)], line = 2.5)
text(bp, 100 * h$density, sprintf("%i %%", round(100 * h$density)), 
  pos = 3, xpd = T, cex = 1.5)
text(par("usr")[1], par("usr")[4], labels = sprintf("n = %i", sum(h$counts)),
  adj = c(-0.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "D", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Institutions: channel reluctance -----
h <- hist(datatab$reluctance, breaks = 0:5 + .5, plot = F)

bp <- barplot(100 * h$density, col = 4, border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1.3, cex.axis = 1.2,
  main = "Reluctance score", names.arg = h$mids, ylim = c(0, 80))
mtext(c("No, none", "Yes, a lot"), side = 1, at = bp[c(1,5)], line = 2.5)
text(bp, 100 * h$density, sprintf("%i %%", round(100 * h$density)), 
  pos = 3, xpd = T, cex = 1.5)
text(par("usr")[2], par("usr")[4], labels = sprintf("n = %i", sum(h$counts)),
  adj = c(1.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "E", adj = c(2, -1.2), xpd = T, cex = 3)

#---- saving

dev.print(png, filename = "Supplementary_figures/SF4.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF4.eps")


#-----------------------------
# Supplementary Figure 5 : reluctance by channel age
#----------------------------- 

mean_channel_age <- aggregate(Channel_age ~ reluctance, data = datatab, mean)[,2]
sd_channel_age <- aggregate(Channel_age ~ reluctance, data = datatab, sd)[,2]

x11()
par(cex.lab = 1.3, cex.axis = 1.2)
plot(mean_channel_age, col = NA, xlab = "Reluctance score", ylab = "Channel age (years)",
  ylim = range(c(mean_channel_age + sd_channel_age, 
    mean_channel_age - sd_channel_age), na.rm = T), xaxt = "n")
arrows(x0 = 1:4, y0 = mean_channel_age - sd_channel_age, 
  y1 = mean_channel_age + sd_channel_age, code = 3, lwd = 2, angle = 90, length = 0.05)
points(mean_channel_age, col = 4, pch = 16)
axis(1, at = 1:4)

dev.print(png, filename = "Supplementary_figures/SF5.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF5.eps")

#-----------------------------
# Supplementary Figure 6: Video production
#-----------------------------  

x11(width = 10)
par(mfrow = c(1,2))

#----- Production format: faceCam
faceCam_inst <- table(datatab$faceCam[is_inst_survey])
faceCam_inst_prop <- faceCam_inst / sum(faceCam_inst)
faceCam_indiv <- table(datatab$faceCam[!is_inst_survey])
faceCam_indiv_prop <- faceCam_indiv / sum(faceCam_indiv)

# Barplot
bp <- barplot(100 * rbind(faceCam_inst_prop, faceCam_indiv_prop), 
  col = adjustcolor(c(2,4), .5), ylim = c(0, 70),
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1.5, cex.axis = 1.2,
  border = NA, main = "Use of talking-head", beside = T)
abline(h = 0)
text(bp, 100 * rbind(faceCam_inst_prop, faceCam_indiv_prop), 
  sprintf("%i %%", round(100 * rbind(faceCam_inst_prop, faceCam_indiv_prop))), 
  pos = 3, xpd = T, cex = 1.2)
leg <- legend("topright", levels(datatab$institution), fill = c(2,4), 
  border = NA, bty = "n", cex = 1.2)
text(leg$rect$left, with(leg$rect, top - h), 
  labels = sprintf("n = %i", length(na.omit(datatab$faceCam))), 
  adj = c(-0.2, 1), cex = 1.5)
text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Production format: animation
animation_inst <- table(datatab$animation[is_inst_survey])
animation_inst_prop <- animation_inst / sum(animation_inst)
animation_indiv <- table(datatab$animation[!is_inst_survey])
animation_indiv_prop <- animation_indiv / sum(animation_indiv)

# Barplot
bp <- barplot(100 * rbind(animation_inst_prop, animation_indiv_prop), 
  col = adjustcolor(c(2,4), .5), ylim = c(0, 70),
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1.5, cex.axis = 1.2,
  border = NA, main = "Use of animation", beside = T)
abline(h = 0)
text(bp, 100 * rbind(animation_inst_prop, animation_indiv_prop), 
  sprintf("%i %%", round(100 * rbind(animation_inst_prop, animation_indiv_prop))), 
  pos = 3, xpd = T, cex = 1.2)
text(par("usr")[2], par("usr")[4], 
  labels = sprintf("n = %i", length(na.omit(datatab$animation))), 
  adj = c(1, 1), cex = 1.5, xpd = T)
text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)

dev.print(png, filename = "Supplementary_figures/SF6.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Supplementary_figures/SF6.eps", 
  fallback_resolution = 600)

#-----------------------------
# MOVED AS FIGURE 6
# Supplementary Figure 7: Showing the fact that most views are toward a few channels
#-----------------------------    

# x11(width = 12, height = 10)
# par(mfrow = c(2,2), oma = c(0, 0, 0, 2))
# 
# bp <- barplot(sort(channel_data$channel_subscribers[!is_institution], decreasing = TRUE), 
#   col = 4, border = 4, bty = "l", xlab = "Channel rank", 
#   ylab = "Number of subscribers", main = "Individuals", ylim = c(0, 4*10^6))
# abline(h = 0)
# rank_ticks <- pretty(1:sum(!is_institution))
# axis(1, at = c(0, bp[rank_ticks[-1]]), labels = rank_ticks)
# text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)
# 
# bp <- barplot(sort(channel_data$channel_subscribers[is_institution], decreasing = TRUE), 
#   col = 2, border = 2, bty = "l", xlab = "Channel rank", 
#   ylab = "Number of subscribers", main = "Institutions", ylim = c(0, 8*10^5))
# abline(h = 0)
# rank_ticks <- pretty(1:sum(is_institution))
# axis(1, at = c(0, bp[rank_ticks[-1]]), labels = rank_ticks)
# text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)
# 
# #--- packcircles
# pack_indiv <- circleProgressiveLayout(channel_data$channel_subscribers[!is_institution])
# poly_indiv <- circleLayoutVertices(pack_indiv, npoints = 50)
# poly_indiv <- split(poly_indiv[,1:2], poly_indiv[,3])
# 
# pack_inst <- circleProgressiveLayout(channel_data$channel_subscribers[is_institution])
# poly_inst <- circleLayoutVertices(pack_inst, npoints = 50)
# poly_inst <- split(poly_inst[,1:2], poly_inst[,3])
# 
# # Colors
# sub_sc_indiv <- c(0,5,6,7)
# #pretty(log10(channel_data$channel_subscribers[!is_institution]))
# cut_indiv <- cut(log10(channel_data$channel_subscribers[!is_institution]), 
#   sub_sc_indiv)
# col_indiv <- brewer.pal(nlevels(cut_indiv), "Blues")[cut_indiv]
# 
# sub_sc_inst <- c(0,4:6)
# cut_inst <- cut(log10(channel_data$channel_subscribers[is_institution]), 
#   sub_sc_inst)
# col_inst <- brewer.pal(nlevels(cut_inst), "Reds")[cut_inst]
# 
# # Limits
# indiv_lims <- with(pack_indiv, 
#   range(c(x + radius, x - radius, y + radius, y - radius), na.rm = T))
# inst_lims <- with(pack_inst, 
#   range(c(x + radius, x - radius, y + radius, y - radius), na.rm = T))
# 
# par(mar = c(0, 4, 4, 4))
# 
# plot(0, 0, col = NA, bty = "n", axes = F, xlab = "", ylab = "", 
#   xlim = indiv_lims, ylim = indiv_lims)
# Map(polygon, poly_indiv, col = col_indiv)
# text(par("usr")[1], par("usr")[4], "C", adj = c(2, -1.2), xpd = T, cex = 3)
# image.plot(zlim = range(sub_sc_indiv), breaks = sub_sc_indiv, 
#   lab.breaks = 10^sub_sc_indiv, legend.only = T, 
#   col = brewer.pal(nlevels(cut_indiv), "Blues"))
# 
# plot(0, 0, col = NA, bty = "n", axes = F, xlab = "", ylab = "", 
#   xlim = inst_lims, ylim = inst_lims)
# Map(polygon, poly_inst, col = col_inst)
# text(par("usr")[1], par("usr")[4], "D", adj = c(2, -1.2), xpd = T, cex = 3)
# image.plot(zlim = range(sub_sc_inst), breaks = sub_sc_inst, 
#   lab.breaks = 10^sub_sc_inst, legend.only = T, 
#   col = brewer.pal(nlevels(cut_inst), "Reds"))
# 
# dev.print(png, filename = "Supplementary_figures/SF7.png", units = "in",
#   res = 200)
# dev.copy2eps(file = "Supplementary_figures/SF7.eps")
# 
# #----- Gini coef -----
# Gini(channel_data$channel_subscribers[!is_institution], na.rm = T)
# Gini(channel_data$channel_subscribers[is_institution], na.rm = T)


#-----------------------------
# Supplementary Figure 7: likes / views vs subscribers
#-----------------------------

likes_views <- with(video_data, like_count / view_count)

likes_views_channel <- tapply(likes_views, video_data$channel_id, 
  mean, na.rm = T)
likes_views_channel <- likes_views_channel[channel_data$channel_id]
likes_views_channel[likes_views_channel == 0] <- NA

reg_data <- data.frame(likes_views = likes_views_channel, 
  subs = channel_data$channel_subscribers, institution = is_institution)
regs <- lmList(log10(likes_views) ~ log10(subs) | institution,
  data = reg_data, na.action = na.omit)

x11()
plot(channel_data$channel_subscribers, likes_views_channel, 
  pch = ifelse(is_institution, 15, 16),
  ylab = "Like count / view count", xlab = "Number of subscribers", log = "xy",
  col = adjustcolor(ifelse(is_institution, 2, 4), .3))
Map(abline, regs, lwd = 2, lty = 1:2, col = c(4,2))
legend("topleft", c("Individuals", "Institutions"), pch = 16:15, 
  col = adjustcolor(c(4,2), .3), bty = "n")

dev.print(png, filename = "Supplementary_figures/SF7.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Supplementary_figures/SF7.eps", 
  fallback_resolution = 600)

#--------------------------
#  Supplementary Figure 8: Correlation between satisfaction and # Subscribers
#--------------------------

science_outreach <- datatab$institutionGoal %in% 
  c("Science", "Both", "Sep. channels")
  
cor_prio_subs <- with(datatab[science_outreach,], cor(priority, subs))

x11()  
boxplot(subs ~ priority, data = datatab[science_outreach,], col = 4, pch = 16,
  ylab = "# Subscribers", xlab = "Priority")
mtext(c("Low priority", "High priority"), side = 1, at = c(1,5), line = 2)
mtext(c("n = ", table(datatab[science_outreach, "priority"])), at = 0:5,
  line = 1, cex = 1.5)
text(par("usr")[2], par("usr")[4], sprintf("cor = %1.2f", cor_prio_subs),
  adj = c(1.2, 1.2), cex = 1.2)
  
dev.print(png, filename = "Supplementary_figures/SF8.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF8.eps")

#--------------------------
#  Supplementary Figure 9: Mean # subscribers for each field
#-------------------------- 

nrep_field <- sum(sapply(datatab$field, length) > 0)
field_indiv <- table(unlist(datatab$field[!is_inst_survey]))
field_indiv <- field_indiv[field_indiv > 0]
ord <- order(field_indiv, decreasing = TRUE)

fields_mat <- apply(datatab[,c("field", "subs")], 1, function(x){
  subs <- rep(NA, nlevels(x$field))
  subs[x$field] <- x$subs
  subs
})

x11(width = 10)
par(mar = c(10, 4, 4, 2) + .1)

boxplot(t(log10(fields_mat[ord,])), yaxt = "n", ylab = "Number of subscribers", 
  names = levels(datatab$field[[1]])[ord], las = 3, col = 4)
axis(2, at = axTicks(2), labels = 10^axTicks(2))

top <- 400000
above <- apply(fields_mat, 1, function(x) sum(x > top, na.rm = T))[ord]
boxplot(t(fields_mat[ord,]), ylab = "Number of subscribers", 
  names = levels(datatab$field[[1]])[ord], las = 3, col = 4,
  ylim = c(0, top), at = seq_len(length(ord)))
mtext(above[above > 0], side = 3, at = seq_len(length(ord))[above > 0])

dev.print(png, filename = "Supplementary_figures/SF9.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF9.eps")

#-----------------------------
# Supplementary Figure 10: Number of subscribers as a function of channel age
#-----------------------------    

# Age of the channel
channel_age <- (as.POSIXlt("2020-08-05") - channel_data$channel_creation) / 365.25
channel_age <- as.numeric(channel_age)

# regressions
reg_subs_age_indiv <- lm(log10(channel_data$channel_subscribers) ~ channel_age,
  subset = !is_institution)
reg_subs_age_inst <- lm(log10(channel_data$channel_subscribers) ~ channel_age,
  subset = is_institution)

x11()
plot(log10(channel_data$channel_subscribers) ~ channel_age,
  pch = ifelse(is_institution, 15, 16), yaxt = "n",
  xlab = "Channel age in years", ylab = "Number of subscribers", 
  col = adjustcolor(ifelse(is_institution, 2, 4), .3))
abline(reg_subs_age_inst, lwd = 2, lty = 2, col = 2)
abline(reg_subs_age_indiv, lwd = 2, col = 4)
legend("topleft", c("Individuals", "Institutions"), col = adjustcolor(c(4,2), .5), 
  pch = 16:15, bty = "n")
axis(2, axTicks(2), labels = 10^axTicks(2))

dev.print(png, filename = "Supplementary_figures/SF10.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Supplementary_figures/SF10.eps", 
  fallback_resolution = 600)

#-----------------------------
# Supplementary Figure 11: proportion of women viewer er field
#----------------------------- 
# Determine which channel treats which field
which_field <- sapply(levels(datatab$field[[1]]), function(x)
  sapply(datatab$field, function(y) any(y == x))
)
mean_female <- apply(which_field, 2, function(x){
  prop <- aggregate(datatab[,"PropFemale"], by = list(x),
    mean, na.rm = T)[2,-1]
})
mean_female <- na.omit(mean_female)
ordf <- order(mean_female, decreasing = TRUE)

x11(width = 10)
par(mar = c(10, 4, 4, 2) + .1)
barplot(mean_female[ordf], col = 4, border = NA, ylab = "Women viewer proportion (%)",
  names.arg = names(mean_female)[ordf], las = "3", ylim = c(0, 40))
  
dev.print(png, filename = "Supplementary_figures/SF11.png", units = "in",
  res = 200)
dev.copy2eps(file = "Supplementary_figures/SF11.eps")

#--------------------------
#  Supplementary Figure 12: number of channel managed
#--------------------------

x11(width = 10)

#----- Number of channel owned
nchan_inst <- table(datatab$nchannel[is_inst_survey])
pchan_inst <- nchan_inst / sum(nchan_inst)
nchan_indiv <- table(datatab$nchannel[!is_inst_survey])
pchan_indiv <- nchan_indiv / sum(nchan_indiv)

# Barplot
bp <- barplot(100 * rbind(pchan_inst, pchan_indiv), col = adjustcolor(c(2,4), .5), 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1.5, cex.axis = 1.2,
  border = NA, main = "", beside = T, ylim = c(0, 100))
abline(h = 0)
text(bp, 100 * rbind(pchan_inst, pchan_indiv), 
  sprintf("%i %%", round(100 * rbind(pchan_inst, pchan_indiv))), 
  pos = 3, xpd = T, cex = .8)
leg <- legend("topright", rev(levels(datatab$institution)), fill = c(2,4), 
  border = NA, bty = "n", cex = 1.2)
text(leg$rect$left, with(leg$rect, top - h), 
  labels = sprintf("n = %i", length(na.omit(datatab$nchannel))), 
  adj = c(-0.2, 1), cex = 1.5)

#--- Saving the plot
dev.print(png, filename = "Supplementary_figures/SF12.png", units = "in",
  res = 200)
dev.print(cairo_ps, file = "Supplementary_figures/SF12.eps", 
  fallback_resolution = 600)



