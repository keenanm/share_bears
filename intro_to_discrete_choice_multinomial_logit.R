###### The purpose of this script is to serve as simple walkthrough of multinomial logit models with respect to hierarchical and mixed models.
###### Author: Keenan Morrison (with credit to Kenneth Train, author of mlogit)

library(dplyr)
library(mlogit)

data("Heating", package = "mlogit")

# expanding out abbreviations
Heating0 <- Heating %>%
            mutate(expanded_dv = case_when(depvar == 'gc' ~ 'gas central'
                                          ,depvar == 'gr' ~ 'gas room'
                                          ,depvar == 'ec' ~ 'electric central'
                                          ,depvar == 'er' ~ 'electric room'
                                          ,TRUE ~ 'heat pump'))

names(Heating0) <- gsub(x = names(Heating0), pattern = "ic", replacement = "install_cost")  
names(Heating0) <- gsub(x = names(Heating0), pattern = "oc", replacement = "operating_cost")  

Heating0 = Heating0 %>% select(idcase, depvar, expanded_dv, everything())

Heating1 <- mlogit.data(Heating0, shape="wide", choice="depvar", varying=c(4:13))

View(Heating1)

no.int.model <- mlogit(depvar~ install_cost + operating_cost | 0 | 0, data = Heating1)

summary(no.int.model)

# Coefficients are negative, meaning that as the cost of a system rises and the costs of the
# other systems remain the same then the probability of that system being chosen falls.

apply(fitted(no.int.model, outcome=FALSE), 2, mean)
# predicted choice frequencies

# calculate a willingness to pay
wtp <- coef(no.int.model)["operating_cost"]/coef(no.int.model)["install_cost"]
wtp

# add in alternative specific intercepts to the model
alt.mod <- mlogit(depvar~ install_cost + operating_cost | 1 | 0, H, reflevel = 'hp')

summary(alt.mod)
# notice that hp (heat pump serves as baseline, meaning the constant is 0)
# intercept represents the average impact of unincluded factors

apply(fitted(alt.mod, outcome = FALSE), 2, mean)

# Note that they match exactly: alternative-specific constants in a logit model insure that the average probabilities equal the observed shares.

wtp2 <- coef(alt.mod)["operating_cost"]/coef(alt.mod)["install_cost"]
wtp2

# What if we want to know the difference between two levels that are not the baseline
coef(alt.mod)["gc:(intercept)"] - coef(alt.mod)["gr:(intercept)"]

# including income as an individual level variable

inc.mod <- mlogit(depvar~ operating_cost + install_cost | income, H, reflevel="hp")
summary(inc.mod)

# all of the coefficients are negative relative to baseline
# as income rises, the probability that heat pump system will be chosen increases
# none of the alt specific p-values significant

coef(inc.mod)["gc:income"] - coef(inc.mod)["gr:income"] 
# a one unit increase in income bracket increases the probability of choosing central gas over room gas system by 10.8%

lrtest(alt.mod, inc.mod) # Likelihood Ratio test
waldtest(alt.mod, inc.mod) # Wald Test
scoretest(alt.mod, inc.mod) # Rao's Score Test


#################################################
###            
###           Nested logit model
###

# Choice of heating/cooling system vs. Heating Only

data("HC", package = "mlogit")

HC <- mlogit.data(HC, varying = c(2:8, 10:16), choice = "depvar", shape = "wide")
cooling.modes <- index(HC)$alt %in% c('gcc', 'ecc', 'erc', 'hpc')
HC$icca[!cooling.modes] <- HC$occa[!cooling.modes] <- 0

cool.mod.unnested <- mlogit(depvar ~ occa + icca + och + ich, data = HC)
  
# The nested model in this case better reflects a reality where the decision maker is first
# deciding between a heating or cooling system, and then making their choice within those subgroups
cool.mod.nested <- mlogit(depvar ~ occa + icca + och + ich, data = HC, 
                          nests = list(cooling = c("ecc", "erc", "gcc", "hpc"),
                                       noncool = c("ec", "gc", "er"))
                          )

#Likelihood ratio test shows better fit for nested model
lrtest(cool.mod.unnested, cool.mod.nested)

# What happens when we make a poorly nested model?  Let's propose people decide between gas-powered vs. non-gas
# systems, and then within those choose among cooling and heating units.
bad.nested.mod <- mlogit(depvar ~ icca + occa + och + ich, data = HC,
                       nests = list(nongas = c("ec", "ecc", "er", "erc", "hpc"), 
                                    gas = c("gc", "gcc")), 
                       un.nest.el = TRUE)
summary(bad.nested.mod)

lrtest(bad.nested.mod, cool.mod.nested) # unsurprising result

apply(fitted(cool.mod.nested, outcome=FALSE), 2, mean)
summary(cool.mod.nested)


#################################################
###            
###                   Mixed Logit
###

# A mixed logit model or random parameters logit model is a logit model for which the
# parameters are assumed to vary from one individual to another.  This occurs when not
# all individuals are presented with the same set of choices

data("Train", package = "mlogit")
names(Train) <- gsub(x = names(Train), pattern = "_", replacement = ".")
Train = Train %>% select(id, choiceid, choice, price.A, price.B, time.A, time.B, comfort.A, comfort.B, change.A, change.B)
# in this case an individual is actually presented with the same choice up to 16 times with prices varying each time

Train0 <- mlogit.data(Train, shape = "wide", varying = 4:11, choice = "choice",
                  opposite = c("price", "time", "comfort", "change"), id = "id")


Train.ml <- mlogit(choice ~ price + time + change + comfort | 1 | 0, data = Train0)
summary(Train.ml)


Train.mxlc <- mlogit(choice ~ price + time + change + comfort | 1 | 0, data = Train0, panel = TRUE, 
                     rpar = c(time = "n", change = "n", comfort = "ln"), # probability distributions
                     correlation = TRUE, R = 100, halton = NA) # drawing scheme
summary(Train.mxlc)