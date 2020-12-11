###############################################################
#
#      Motivations, qualifications, and strategies of science 
#               communicators on YouTube: 
#         a case study of the french ecosystem
#                 Part 1: creators
#
###############################################################

library(colorspace)

#--------------------------
#     Data reading
#--------------------------

# Loading survey data
datatab <- readRDS("Data/Survey_data.RDS")

# Some objects
nrep <- nrow(datatab)
nindiv <- sum(datatab$institution == "Individual")

#--------------------------
#   Figure 1: description of creators
#--------------------------

x11(width = 15, height = 10)
par(mfrow = c(2,3))

#----- Gender proportion -----
# table
gcount <- table(datatab$gender, exclude = "NA")
gprop <- gcount / sum(gcount)

# Plot
bp <- barplot(100 * gprop, col = c(3, 2, 4), border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1, cex.axis = 1.2,
  main = "Gender", ylim = c(0, 100))
text(bp, 100 * gprop, sprintf("%i %%", round(100 * gprop)), pos = 3, 
  xpd = T, cex = 1.5)
text(par("usr")[1], par("usr")[4], labels = sprintf("n = %i", sum(gcount)),
  adj = c(-0.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Highest degree stratified by expertise -----
# table
degree_count <- table(datatab$expertise, datatab$degree)
degree_prop <- degree_count / sum(degree_count)
colnames(degree_prop)[2:3] <- c("High\nSchool", "High\nSchool +2")

# Barplot
bp <- barplot(100 * degree_prop, col = c(3,4,2), border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1, cex.axis = 1.2,
  main = "Highest degree", ylim = c(0,50))
text(bp, 100 * colSums(degree_prop), sprintf("%i %%", round(100 * colSums(degree_prop))), 
  pos = 3, xpd = T, cex = 1.5)
leg <- legend("topleft", levels(datatab$expertise), fill = c(3,4,2), 
  border = NA, bty = "n", cex = 1.2, 
  title = "Expertise of the subject")
text(leg$rect$left, with(leg$rect, top - h), 
  labels = sprintf("n = %i", sum(degree_count)), adj = c(-0.2, 1), cex = 1.5)
text(par("usr")[1], par("usr")[4], "B", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Professional situation -----
# table: sum will be above 100 since there may be several people involved
pro_count <- table(unlist(datatab$Academia), unlist(datatab$proCat))
npro <- sum(sapply(datatab$proCat, length) > 0)
pro_prop <- pro_count / npro
bypro <- colSums(pro_prop) 

# barplot
bp <- barplot(100 * pro_prop, col = c(2,4), border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1, cex.axis = 1.2,
  main = "Professional category", ylim = c(0, 60))
text(bp, 100 * bypro, sprintf("%i %%", round(100 * bypro)), 
  pos = 3, xpd = T)
leg <- legend("topleft", rownames(pro_prop), fill = c(2,4), 
  title = "Academia", border = NA, bty = "n", cex = 1.2)
text(leg$rect$left, with(leg$rect, top - h), 
  labels = sprintf("n = %i", npro), adj = c(-0.1, 1), cex = 1.5, xpd = T)
text(par("usr")[1], par("usr")[4], "C", adj = c(2, -1.2), xpd = T, cex = 3)


#----- Education in video making -----
# table
edu_video_count <- table(datatab$EducationVideo)
edu_video_prop <- edu_video_count / sum(edu_video_count)

# Barplot
bp <- barplot(100 * edu_video_prop, col = 4, border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1, cex.axis = 1.2,
  main = "Video making education", ylim = c(0,100))
text(bp, 100 * edu_video_prop, sprintf("%i %%", round(100 * edu_video_prop)), 
  pos = 3, xpd = T, cex = 1.5)
text(par("usr")[2], par("usr")[4], labels = sprintf("n = %i", sum(edu_video_count)),
  adj = c(1.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "D", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Number of people behind the channel / institution -----
# Table
npeople_count <- table(datatab$institution, datatab$npeople)
npeople_prop <- npeople_count / sum(npeople_count)
prop_byn <- colSums(npeople_prop)

# Barplot
bp <- barplot(100 * npeople_prop, col = c(4,2), border = NA, 
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = 1, cex.axis = 1.2,
  main = "Number of people", ylim = c(0, 60))
text(bp, 100 * prop_byn, sprintf("%i %%", round(100 * prop_byn)), 
  pos = 3, xpd = T)
leg <- legend("topright", levels(datatab$institution), fill = c(4,2), 
  border = NA, bty = "n", cex = 1.2)
text(leg$rect$left, with(leg$rect, top - h), 
  labels = sprintf("n = %i", sum(npeople_count)), adj = c(-0.2, 1), cex = 1.5)
text(par("usr")[1], par("usr")[4], "E", adj = c(2, -1.2), xpd = T, cex = 3)

#----- For institutions: time spent on the channels -----
# table
tspent_count <- table(datatab$time_spent)
tspent_prop <- tspent_count / sum(tspent_count)

# Barplot
bp <- barplot(100 * tspent_prop, col = 2, border = NA, xlab = "Time spent (%)",
  ylab = "Proportion (%)", cex.lab = 1.3, cex.names = .9, cex.axis = 1.2,
  main = "Time allocated", ylim = c(0, 40))
text(bp, 100 * tspent_prop, sprintf("%i %%", round(100 * tspent_prop)), 
  pos = 3, xpd = T)
text(par("usr")[2], par("usr")[4], labels = sprintf("n = %i", sum(tspent_count)),
  adj = c(1.2, 1.5), cex = 1.5)
text(par("usr")[1], par("usr")[4], "F", adj = c(2, -1.2), xpd = T, cex = 3)

  
dev.print(png, filename = "Figures/Fig1.png", units = "in",
  res = 200)
dev.copy2eps(file = "Figures/Fig1.eps")

#---- SI Table 3 -----
# Counts
t(degree_count)
# Column-wise proportions
t(degree_count / rowSums(degree_count))
# Row totals
rowSums(t(degree_count))

#--------------------------
#   Figure 2: Money, money, money
#--------------------------

# /!\ This figure cannot be reproduced with shared data because some information have been hidden for confidentiality issues

# Initiate plot
x11(width = 10, height = 7)
layout(matrix(c(1,1,2,2,0,3,3,0), nrow = 2, byrow = T))

#----- Balance -----
bal_table <- table(datatab$balance[datatab$institution == "Individual"])
bal_prop <- 100 * bal_table / sum(bal_table)

bp <- barplot(bal_prop, col = 4, border = NA, ylab = "Proportion (%)", 
  cex.lab = 1.3, cex.axis = 1.2, cex.names = 1.3, main = "Balance", ylim = c(0, 50))
text(bp, bal_prop, sprintf("%i %%", round(bal_prop)), pos = 3, cex = 1.5, xpd = T)
text(par("usr")[2], par("usr")[4], labels = sprintf("n = %i", sum(bal_table)),
  adj = c(1.2, 1.5), cex = 1.5, xpd = T)
text(par("usr")[1], par("usr")[4], "A", adj = c(2, -1.2), xpd = T, cex = 3)

#----- Funding sources -----
levs <- levels(datatab$funding[[1]])
prop_fund <- 100 * sapply(levs,
  function(l) mean(sapply(datatab$funding[datatab$institution == "Individual"], 
    function(f) l %in% f))
)
prop_fund <- sort(prop_fund, decreasing = TRUE)
prop_fund <- c(prop_fund[!names(prop_fund) %in% c("None", "Other")],
  prop_fund["None"])

#par(mar = c(7, 4, 4, 2) + .1)
bp <- barplot(prop_fund, col = 4, border = NA, ylab = "Proportion (%)", 
  cex.lab = 1.3, cex.axis = 1.2, cex.names = 1.3, main = "Income sources", 
  las = 3, xpd = T, ylim = c(0, 80))
text(bp, prop_fund, sprintf("%i %%", round(prop_fund)), pos = 3, xpd = T)
text(par("usr")[2], par("usr")[4],
  labels = sprintf("n = %i", sum(!is.na(datatab$funding[datatab$institution == "Individual"]))),
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
h <- hist(log10(income_norm[datatab$institution == "Individual"] + 1), 
  breaks = -1:4, plot = F)
hc <- h$counts
h$counts <- round(100 * h$density, digits = 1)

# plot histogram
plot(h, xaxt = "n", labels = sprintf("%2.0f %%", h$counts), col = 4, border = NA, 
  ylab = "Proportion (%)", main = "Monthly income", xlab = "Normalized income (???)",
  cex.lab = 1.3, cex.axis = 1.2, ylim = c(0, 50))
axis(1, at = -1:4, cex.axis = 1.2,
  labels = formatC(c(0, 10^(0:4)), format = "fg", big.mark = " "))
text(par("usr")[1], par("usr")[4], "C", adj = c(2, -1.2), xpd = T, cex = 3)
text(par("usr")[2], par("usr")[4], labels = sprintf("n = %i", sum(hc)),
  adj = c(1.2, 1.5), cex = 1.5, xpd = T)

dev.print(png, filename = "Figures/Fig2.png", units = "in",
  res = 200)
dev.copy2eps(file = "Figures/Fig2.eps")

#-----------------------------
#  In text: proportion of individual communicators making enough for a living
#-----------------------------

# /!\ Cannot be reproduced with shared data because some information have been hidden for confidentiality issues

indiv_pos <- datatab$institution == "Individual" & datatab$balance == "Positive"
mean(datatab$mainSource[indiv_pos] == c("Yes, and it is enough"),
  na.rm = T)

#-----------------------------
#  Age distribution
#-----------------------------

age_dist <- hist(datatab$Age, breaks = c(0, 17, 24, 34, 44, 54, 64, 100) + .5, plot = F)
age_dist$count / sum(age_dist$count)