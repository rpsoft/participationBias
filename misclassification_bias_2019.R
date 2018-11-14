#################################################
# MPH Advanced Epidemiology 2019                #
# Surveys and measurement biases                #
# Megan McMinn <megan.mcminn@glasgow.ac.uk      #
# Shiny App data & code                         #
# R 3.5.1
#################################################

###########################
# Non-participation bias  #
###########################

# Cases of uveal melanoma and the use of mobile phones
# use of mobile phones
# population controls

## This data are taken from a subset of a study exploring the effects of mobile phone use and the odds of being diagnosed with uveal melaoma. 
# 455 cases and 827 controls
# Asked to rate mobile phone use as 'Never', 'Sporodic' and 'Regular' (amongst other questions)

# This subset compares the 'Never' and 'Regular'  groups (so smaller numbers)
stang09_p <- as.data.frame(matrix(c(136, 297, 107, 165), byrow=TRUE, nrow = 2), row.names = c("RegularUse", "NoUse"))
colnames(stang09_p) <- c("Case", "Control")

# Non participants (persons who were invited to participate in the study, but refused) were asked to complete a short questionnaire, which asked
# to classify their mobile phone use as "not' or 'regular'
stang09_np_short <- as.data.frame(matrix(c(3, 72,  7, 212), byrow=TRUE, nrow = 2), row.names = c("RegularUse", "NoUse"))
colnames(stang09_np_short) <- c("Case", "Control")


# Using only data from the Participants of the study, finding borderline significant value, and a <1 OR, 
# suggesting that regular use is assoc with lower odds of developing melanoma
fisher.test(stang09_p) -> f1
round(fisher.test(stang09_p)$estimate,2) -> f1r
f1ll <- round(fisher.test(stang09_p, conf.int = TRUE)$conf.int[1],2)
f1ul <- round(fisher.test(stang09_p, conf.int = TRUE)$conf.int[2],2) 
# 0.71

# Using only the data from the Non-participants, we find an OR > 1, and is not significant. 
f2 <- fisher.test(stang09_np_short)
f2r <- round(fisher.test(stang09_np_short)$estimate,2)
f2ll <- round(fisher.test(stang09_np_short, conf.int = TRUE)$conf.int[1],2)
f2ul <- round(fisher.test(stang09_np_short, conf.int = TRUE)$conf.int[2],2) 
# 1.26

## This shows the impact of non-participation bias

## Combining both participants and non-participants, we see the 'true' OR for this sample is 1.25. 
stang09_full <- stang09_p + stang09_np_short
ff <- fisher.test(stang09_full)  
ffr <- round(fisher.test(stang09_full)$estimate,2)
ffll <- round(fisher.test(stang09_full, conf.int = TRUE)$conf.int[1],2)
fful <- round(fisher.test(stang09_full, conf.int = TRUE)$conf.int[2],2) 
# 1.25

## Discussion can be had on non-participants who refused to take part in the short questionnaire 

