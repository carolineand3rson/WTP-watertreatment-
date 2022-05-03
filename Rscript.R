library(readxl)
library(dplyr)
WTP <- read_excel("WTP water treatment improvement_April 26, 2022_08.31.xlsx")
# View(WTP_water_treatment_improvement_April_26_2022_08_31)

# Data Cleaning + Wrangling

WTP <- subset(WTP, select = c('Q1', 'Q2', 'Q3', 'Q4_1', 'Q5', 'Q6',
                              'Q7', 'Q8', 'Q9', 'Q10', 'Q11', 'Q12'))

WTP <- WTP %>% rename('WTP'='Q1', 'region'='Q2', 'typicalWaSource' = 'Q3', 
                      'tapQuality' = 'Q4_1',
                      'reuseBottle' = 'Q5', 'waterIntake' = 'Q6', 'choiceDrink'
                      = 'Q7', 'areaSize' = 'Q8', 'householdIncome' = 'Q9',
                      'race' = 'Q10', 'sex' = 'Q11', 'birthYear' = 'Q12')
# Removing the answers from in class survey
N = 15

WTPfull <- tail(WTP, -N)

WTPnoNA <- na.omit(WTPfull)

WTPnoNA$WTP = as.numeric(gsub("\\$", "", WTPnoNA$WTP))

WTPnoNA$tapQuality = as.numeric(WTPnoNA$tapQuality)

WTPnoNA <- WTPnoNA[-3,]
WTPnoNA <- WTPnoNA[-31,]
WTPnoNA <- WTPnoNA[-39,]
WTPnoNA <- WTPnoNA[-47,]
WTPnoNA <- WTPnoNA[-75,]
WTPnoNA <- WTPnoNA[-89,]
WTPnoNA <- WTPnoNA[-119,]
WTPnoNA <- WTPnoNA[-3,]

WTPnoNA$birthYear = as.numeric(WTPnoNA$birthYear)
mean(WTPnoNA$birthYear)


WTPfull <- WTPfull[-31,]
WTPfull <- WTPfull[-34,]
WTPfull <- WTPfull[-55,]
WTPfull <- WTPfull[-75,]
WTPfull <- WTPfull[-80,]
WTPfull <- WTPfull[-94,]
WTPfull <- WTPfull[-108,]

# Exploratory analysis

library(writexl)

# write_xlsx(WTPnoNA, "C:\\RStudio\\WTP-watertreatment-\\wtpcleaned.xlsx")
# write_xlsx(WTP, "C:\\RStudio\\WTP-watertreatment-\\wtpfull.xlsx")


plot(WTPnoNA$WTP, WTPnoNA$tapQuality, xlab = "WTP", ylab= "Tap Water Quality")

WTPnoNA$reuseBottleCat <- as.factor(WTPnoNA$reuseBottle)

mod1 <- lm(formula = WTP ~ tapQuality + choiceDrink + householdIncome, data = WTPnoNA)
summary(mod1)
plot(mod1)

mod2 <- lm(formula = WTP ~ areaSize + householdIncome + race + sex + birthYear, 
           data = WTPnoNA)
summary(mod2)

mod3 <- lm(formula = WTP ~ region + waterIntake + tapQuality + reuseBottle, data = WTPnoNA)
summary(mod3)

mod4 <- lm(formula = WTP ~ reuseBottle, data=WTPnoNA)
summary(mod4)

median(WTPnoNA$tapQuality)
mode(WTPnoNA$tapQuality)

whitewtp <- subset(WTPnoNA$race == White)

WTPnoNA <- WTPnoNA[-65,]

mean(WTPnoNA$WTP)

colorIncome <- viridis(nrow(WTPnoNA))


boxy <- boxplot(WTP ~ householdIncome, data = WTPnoNA, xlab = "Household Income",
                main = "Respondents WTP based on Household Income", col = viridis::viridis(6, alpha=1))

box2 <- boxplot(WTP ~ areaSize, data = WTPnoNA, xlab = "Area Size", 
                main = "Respondents WTP based on Area Size", col = viridis::viridis(5, alpha = 1))

box3 <- boxplot(WTP ~ waterIntake, data = WTPnoNA, xlab = "Water Intake", 
                main = "Respondents WTP based on Daily Water Intake",
                col = viridis::viridis(4, alpha=1))
t.test(WTP ~ householdIncome, data = WTPnoNA)
