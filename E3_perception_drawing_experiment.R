## Bodo Winter
## Created April 22, 2015; Updated September 23, 2015
## Analysis of drawing data:

##------------------------------------------------------------------
## Preliminaries:
##------------------------------------------------------------------

## Load library for getting xlsx files into R:

library(xlsx)

## Set working directory:

setwd('/Users/teeniematlock/Desktop/research/gesture_categorization/all_analyses_gesture2015/')

## Load in position file (coded by JH):

pos <- read.xlsx('drawing_study_position_JH.xlsx', 1, stringsAsFactors = F)

## Load in the categorical gesture codes (coded by GB, NA, CA):

xdata <- read.csv('drawing_study_categorical_coding.csv')



##------------------------------------------------------------------
## Preprocessing of 'POSITION' data:
##------------------------------------------------------------------

## Make leftmost distance variable to numeric:

pos$LeftMostDist_mm <- as.numeric(pos$LeftMostDist_mm)

## Make the condition column into meaningful labels, first index curve:

pos$index_curve <- as.numeric(substr(pos$condition, 1, 1))

## Re-do range to 1 to 6:

pos$index_curve <- pos$index_curve - 1

## Extract the pinkie curl variable:

pos$pinkie_curl <- as.numeric(substr(pos$condition, 4, 5))

## Convert these numbers to the real rank-ordered variable:

pos$pinkie_curl <- as.numeric(as.factor(pos$pinkie_curl))

## The index_curve values are low are actually more curve... let's reverse this:

index_curve2 <- numeric(nrow(pos))
index_curve2[pos$index_curve==1] <- 6
index_curve2[pos$index_curve==2] <- 5
index_curve2[pos$index_curve==3] <- 4
index_curve2[pos$index_curve==4] <- 3
index_curve2[pos$index_curve==5] <- 2
index_curve2[pos$index_curve==6] <- 1

## Put this back into the file:

pos$index_curve <- index_curve2
rm(index_curve2)

## The pinkie_curl values that are low are also more extended... let's reverse this as well:

pinkie_curl2 <- numeric(nrow(pos))
pinkie_curl2[pos$pinkie_curl==1] <- 6
pinkie_curl2[pos$pinkie_curl==2] <- 5
pinkie_curl2[pos$pinkie_curl==3] <- 4
pinkie_curl2[pos$pinkie_curl==4] <- 3
pinkie_curl2[pos$pinkie_curl==5] <- 2
pinkie_curl2[pos$pinkie_curl==6] <- 1

## Put this back into the file:

pos$pinkie_curl <- pinkie_curl2
rm(pinkie_curl2)


##------------------------------------------------------------------
## Preprocessing of 'SYMMETRY' and 'ROUNDEDNESS' data:
##------------------------------------------------------------------

## Get rid of duplicated SONA IDs (second occurrences; they did it again):

xdata <- xdata[!duplicated(xdata$SONA_ID),]

## Exclude those that are not relevant for our hypothesis (described in text):

xdata <- xdata[which(xdata$WholeHand != 'yes'), ]
xdata <- xdata[which(xdata$BesidesHand != 'yes'), ]

## How much is lost in total?

nrow(xdata)			# 145
168-145				# 23 data points total lost
1-(145/168)			# 13%

## Pinkie curl = 3 is wrongly coded (wrong values on x-axis):

xdata <- xdata[xdata$pinkie_curl != 3,]
xdata[xdata$pinkie_curl == 4,]$pinkie_curl <- 3
xdata[xdata$pinkie_curl == 5,]$pinkie_curl <- 4
xdata[xdata$pinkie_curl == 6,]$pinkie_curl <- 5
xdata[xdata$pinkie_curl == 7,]$pinkie_curl <- 6




##------------------------------------------------------------------
## Analysis of 'SYMMETRY' and 'ROUNDEDNESS':
##------------------------------------------------------------------

## Models:

summary(xmdl.symm <- glm(IndexSymmetrical ~ index_curve + pinkie_curl,
	xdata, family = 'binomial'))
summary(xmdl.round <- glm(Roundness ~ index_curve + pinkie_curl,
	xdata, family = 'binomial'))

## Tests for reporting:

drop1(xmdl.symm, test = 'Chisq')
drop1(xmdl.round, test = 'Chisq')



##------------------------------------------------------------------
## Analysis of 'POSITION' variable:
##------------------------------------------------------------------

## GLM:

summary(xmdl.dist <- lm(LeftMostDist_mm ~ index_curve + pinkie_curl, pos))
anova(xmdl.dist)


