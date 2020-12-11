###############################################################
#
#      Motivations, qualifications, and strategies of science 
#               communicators on YouTube: 
#         a case study of the french ecosystem
#          Part 4: Other useful statistics
#
###############################################################

library(irr)

#-----------------------------
#  Data reading and preparation
#-----------------------------

ratings <- read.csv("Data/channels_rated.csv", header = T, sep = ";")

#-----------------------------
#  ICC
#-----------------------------

icc(ratings[,3:5], model = "twoway", type = "agreement")