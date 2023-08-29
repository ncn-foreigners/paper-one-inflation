library(singleRcapture)

AAA <- estimatePopsize(formula = TOTAL_SUB ~ ., 
                       data = farmsubmission,
                       model = "ztnegbin")

BBB <- countreg::zerotrunc(formula = TOTAL_SUB ~ ., 
                           data = farmsubmission, 
                           dist = "negbin")

AAA <- resid(AAA, "deviance")[,1]
BBB <- resid(BBB, "deviance")

plot(AAA, BBB, 
     xlab = paste0("Proposed approach | Deviance = ", sum(AAA^2)), 
     ylab = paste0("Common approach | Deviance = ",   sum(BBB^2)))

png("figures/deviance - comparison - plot.png")

# Creating a plot
plot(AAA, BBB, 
     xlab = paste0("Proposed approach | Deviance = ", sum(AAA^2)), 
     ylab = paste0("Common approach | Deviance = ",   sum(BBB^2)))

# Closing the graphical device
dev.off() 
