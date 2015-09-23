## Bodo Winter
## Created April 22, 2015; Updated September 23, 2015
## Analysis of study #1, photo study

##------------------------------------------------------------------
## Preliminaries:
##------------------------------------------------------------------

## Load in inter-rater reliability package:

library(irr)

## Load in data:

setwd("/Users/teeniematlock/Desktop/research/gesture_categorization/photo_experiment/")
TL <- read.csv("tania_lopez_coding_3_14_2015_pure.csv")
JH <- read.csv("MSproduction.csv")
JH <- JH[-19,]			# doublet

## Make the names of the coding schemes the same:

levels(TL$ratingTL) <- levels(JH$ratingJH)



##------------------------------------------------------------------
## Analysis:
##------------------------------------------------------------------

## Check inter-rater reliability:

kappa2(cbind(TL$ratingTL, JH$ratingJH))		# kappa = 0.926

## Using codings from "JH":

(xtab <- table(JH$beg, JH$ratingJH))

## How many conform to hypothesis?

sum(diag(xtab)) / nrow(JH)




