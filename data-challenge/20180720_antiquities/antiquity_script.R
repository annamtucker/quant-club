
# Synopsis ----------------------------------------------------------------
## Examine, visualize, and maybe model some data on USA national monuments

# libraries ---------------------------------------------------------------
library(janitor)  ## A new library with functions for tidying data frames
## vignette at: https://cran.r-project.org/web/packages/janitor/janitor.pdf
library(tidyverse)

# functions ---------------------------------------------------------------

# data --------------------------------------------------------------------
antiquities <- read.csv("C:/Users/ras0099/Desktop/actions_under_antiquities_act.csv",
                        stringsAsFactors = FALSE)
# processing --------------------------------------------------------------
## Examine for structure, errors, and missing data
## Many items are character that presumably should not be, such as acres_affected
## and date
str(antiquities)
summary(antiquities)
head(antiquities)
#adorn_rounding(antiquities)

## Check for NAs and duplication of levels induced by typos or other 
## formatting inconsistencies
levels(factor(antiquities$states))
antiquities$states         ## contains multiple comma-separated values
sum(is.na(antiquities$states))
sum(antiquities$states == "")

levels(factor(antiquities$original_name))
antiquities$original_name
sum(is.na(antiquities$original_name))
sum(antiquities$original_name == "")   ## NAs for this column are ""

levels(factor(antiquities$current_agency))
antiquities$current_agency
sum(is.na(antiquities$current_agency))
sum(antiquities$current_agency == "")

levels(factor(antiquities$action))  ## semantic grain is too fine
antiquities$action
sum(is.na(antiquities$action))
sum(antiquities$action == "")

levels(factor(antiquities$date))  ## NAs coded as "" here too
levels(factor(antiquities$year))
levels(factor(antiquities$pres_or_congress)) ## NAs coded as "" here too
## Also inconsistent spacing between initials and use of middle initials
levels(factor(antiquities$acres_affected)) ##Commas are a problem
                                           ## some values are in wrong units
                                           ## min and max values should be verified
levels(factor(antiquities$ï..current_name)) ## no egregious issues seen

## change goofy column name
antiquities <- clean_names(antiquities) ## remove inappropriate chars
                                        ## and make names unique
colnames(antiquities)[1] <- "current_name"

## Need to fix different treatment of NA values across columns
antiquities <- convert_to_NA(antiquities, "")  ## deprecated
                                               ## dplyr::na_if() also weird today

## use string matching to get coarser semantic grain on "action"
## Can diminish reduce to zero or effectively zero?
## What happens after transfers? Is this a way to hide degazetting?

## what got deleted? Any need to pool?
antiquities$action[agrep("deleted", antiquities$action)] <- "deleted" ## 4 clear-cut cases
antiquities$action[agrep("abolished,", antiquities$action)] <- "deleted"

## Possibly degazetted under the radar after changes
antiquities$action[agrep("redesign", antiquities$action)] <- "redesignated_or_transferred"
antiquities$action[agrep("transf", antiquities$action)] <- "redesignated_or_transferred"
antiquities$action[agrep("now", antiquities$action)] <- "redesignated_or_transferred"
antiquities$action[agrep("rename", antiquities$action)][2] <- "renamed" ## unclear if legal status changed 
antiquities$action[agrep("incor", antiquities$action)] <- "redesignated_or_transferred"
antiquities$action[agrep("modif", antiquities$action)][1] <- "redesignated_or_transferred"

## Size changes; need to check for zeros on "size_change" and "diminished"
antiquities$action[agrep("enlarge", antiquities$action)] <- "enlarged"
antiquities$action[agrep("rename", antiquities$action)][1] <- "enlarged" 
antiquities$action[agrep("surv", antiquities$action)][1] <- "enlarged"
antiquities$action[agrep("surv", antiquities$action)][2] <- "size_change"
antiquities$action[agrep("modif", antiquities$action)][2] <- "size_change"
antiquities$action[agrep("bound", antiquities$action)] <- "size_change" ## call me a cynic but needs checking
## Unclear what amendment of description means
antiquities$action[agrep("amend", antiquities$action)] <- "size_change" ## weak but worth checking
antiquities$action[agrep("diminish", antiquities$action)] <- "diminished"

## Establishment
antiquities$action[agrep("establish", antiquities$action)] <- "established"

## identify and repair invalid "acres_affected" values
antiquities$acres_affected <- gsub(",", "", antiquities$acres_affected)

## Convert square miles to acres: multiply by 640
conversion_needed <- agrep("miles", antiquities$acres_affected)
antiquities$acres_affected <- gsub(" square miles", "", antiquities$acres_affected)
antiquities$acres_affected <- gsub(" sq. miles", "", antiquities$acres_affected)
antiquities$acres_affected <- as.numeric(antiquities$acres_affected)
antiquities$acres_affected[c(conversion_needed)] <-
   antiquities$acres_affected[c(conversion_needed)]*640

## Check acreage of size_change events
## most deletions are small properties
boxplot(log (antiquities$acres_affected) ~ antiquities$action, las = 1,
        ylab = "log(acres affected)", cex.lab = 1.5)
levels(factor(antiquities$action))


# not implemented ---------------------------------------------------------
## Check acreage of size_change events
## enable R to parse dates
## Get consistent spacing of pres names
## Deal with cells containing multiple data
## Assign correct type to data columns

