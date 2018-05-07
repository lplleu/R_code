install.packages("multcompView")
library(multcompView)
M1c <- glm(heterobin2~ treatment  + group  + vegheightmax    , family=binomial(link = "logit"), data=df) #
summary(M1c) # this summary gives you the estimates for the model
Anova(M1c, type="III") # this test then examines which variables are signficant in the model
lsM1c<-lsmeans (M1c, pairwise ~ treatment, type = "response") 
lsM1c
cld(lsM1c, alpha = .05)
