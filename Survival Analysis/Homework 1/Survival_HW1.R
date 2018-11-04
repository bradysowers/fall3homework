# Survival Analysis Homework 1

######################################################################################
# Data set up

library(survival)
library(survminer)
library(muhaz)

katrina <- read.csv("katrina.csv", header = TRUE)

# Changing the survival column so that 1 is an event and 0 is censored
# This way makes more sense and allows the hazard plots to work correctly
katrina$survive=as.numeric(!katrina$survive)

######################################################################################
# Question 1

# Provide summary statistics for each type of pump station failure. 
# What percentage of pumps survived the hurricane? 41% (1-454/770)
# What percentage of pumps are in each failure? (n/770)
# What are the median survival times?
#            n events median 0.95LCL 0.95UCL
# reason=0 316      0     NA      NA      NA
# reason=1 115    115     26      22      32
# reason=2 112    112     45      44      45
# reason=3 111    111     42      41      43
# reason=4 116    116     25      24      25

katrina_fit <- survfit(Surv(time=hour, event=survive == 1) ~ 1, data = katrina)
katrina_fit
summary(katrina_fit)

katrina_reason <- survfit(Surv(time=hour, event=survive == 1) ~ reason, data = katrina)
katrina_reason
summary(katrina_reason)

######################################################################################
# Question 2

# Plot the survival curves for all pumps and the stratified curves (by reason). 
# Discuss anything interesting that you find.
ggsurvplot(katrina_fit, data = katrina, conf.int = FALSE, palette = "grey")
ggsurvplot(katrina_reason, data = katrina, conf.int = FALSE, palette = "grey")

# Reason 1 drops steadily throughout, reason 2 and 3 drop significantly betweeen 
# hours 40-48, and reason 4 drops drastically between hours 23-27

######################################################################################
# Question 3

# Do the four types of pump failures have similar survival curves? 2 & 3 seem similar.
# If not, which ones are statistically different? All of them apparently.

# Per Matt's request, we are excluding reason 0 because those are the ones that survived
# throughout, obviously they will be significantly different that those that didn't

pairwise_survdiff(Surv(time=hour, event=survive == 1) ~ reason, rho=0, data = katrina[katrina$reason != 0,])
pairwise_survdiff(Surv(time=hour, event=survive == 1) ~ reason, rho=1, data = katrina[katrina$reason != 0,])

# rho = 0 is the normal log-rank test, rho = 1 is the weighted test
# not sure which one of those we want to use, but they're all different for both

######################################################################################
# Question 4

# A coworker proposes binning failures into a water-based (flood/surge) group 
# and a mechanical-based (motor/trash) group. 
# Explain why you agree or disagree with this proposal.

# Not sure if we can write code to better answer this question.

# The survival curves for flood/surge (reason 1 & 3) and motor/trash (reason 2 & 4)
# are very different. Thus, we would not want to bin these into the same groups.


######################################################################################
#Question 5

# Create hazard plots and discuss anything interesting that you find.

# here, i am making a new variable hour2, where i'm setting censored observations
# to have hour2 = 49 so that the function doesn't plot them as if they all had
# the event in the last hour

katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$survive == 0, 49, katrina$hour)
katrina_haz <- with(katrina, kphaz.fit(hour2, survive))
kphaz.plot(katrina_haz, main = "hazard function")

# Hazard seems to spike during the times where the reasons were dropping.
# Maybe these are the times when the storm was the worst?
# Not sure if it's possible to make hazard plots for each individual reason

### cumulative hazard ###
ggsurvplot(katrina_fit, fun = "cumhaz", palette = "grey")

# The only intesting thing I'm noting from the cumulative hazard plot is that it's steady
# from hours 27-40. Maybe this was the eye of the storm?