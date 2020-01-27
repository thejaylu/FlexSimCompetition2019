#Circulation util ----
regressionData <- read.csv("RegressionData_Master.csv")
lm.circulateUtil <- lm(CirculationN_Util ~ CirculatingNurse_Count + PreOpNurse_Count +
                         MaintenanceGroup_Count + ScrubNurse_Count + PACUNurse_Count,
                       data = regressionData); summary(lm.circulateUtil)
plot(lm.circulateUtil)
coefficients(lm.circulateUtil)

lm.circulateUtil2 <- lm(CirculationN_Util ~ CirculatingNurse_Count + PreOpNurse_Count + MaintenanceGroup_Count + 
                          ScrubNurse_Count + PACUNurse_Count +
                          CirculatingNurse_Count*PreOpNurse_Count + 
                          CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count+
                          CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count*MaintenanceGroup_Count+
                          CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count*MaintenanceGroup_Count*PACUNurse_Count+
                          PreOpNurse_Count*MaintenanceGroup_Count+
                          PreOpNurse_Count*MaintenanceGroup_Count*ScrubNurse_Count+
                          PreOpNurse_Count*MaintenanceGroup_Count*ScrubNurse_Count*PACUNurse_Count+
                          MaintenanceGroup_Count*ScrubNurse_Count+
                          MaintenanceGroup_Count*ScrubNurse_Count*PACUNurse_Count+
                          ScrubNurse_Count*PACUNurse_Count,
                       data = regressionData); summary(lm.circulateUtil2)
plot(lm.circulateUtil2)
coefficients(lm.circulateUtil2)
anova(lm.circulateUtil2)

lm.circulateUtil_log <- lm(log1p(CirculationN_Util) ~ log1p(CirculatingNurse_Count) + log1p(PreOpNurse_Count) +
                             log1p(MaintenanceGroup_Count) + log1p(ScrubNurse_Count) + log1p(PACUNurse_Count),
                       data = regressionData); summary(lm.circulateUtil)
plot(lm.circulateUtil_log)

#Earnings ----
lm.earnings <- lm(Earnings ~ CirculatingNurse_Count + PreOpNurse_Count + MaintenanceGroup_Count + 
                         ScrubNurse_Count + PACUNurse_Count +
                         CirculatingNurse_Count*PreOpNurse_Count + 
                         CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count+
                         CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count*MaintenanceGroup_Count+
                         CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count*MaintenanceGroup_Count*PACUNurse_Count+
                         PreOpNurse_Count*MaintenanceGroup_Count+
                         PreOpNurse_Count*MaintenanceGroup_Count*ScrubNurse_Count+
                         PreOpNurse_Count*MaintenanceGroup_Count*ScrubNurse_Count*PACUNurse_Count+
                         MaintenanceGroup_Count*ScrubNurse_Count+
                         MaintenanceGroup_Count*ScrubNurse_Count*PACUNurse_Count+
                         ScrubNurse_Count*PACUNurse_Count,
                       data = regressionData); summary(lm.earnings)
plot(lm.earnings)
coefficients(lm.earnings)
anova(lm.earnings)

regressionData2 <- regressionData[-c(240,161,213,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,84,87,90,93,96,99,102,105,108,111,114,117,120,123,126,129,132,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,162,165,168,171,174,177,180,183,186,189,192,195,198,201,204,207,210,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,241,242,243),]
lm.earnings2 <- lm(Earnings ~ CirculatingNurse_Count + PreOpNurse_Count + MaintenanceGroup_Count + 
                    ScrubNurse_Count + PACUNurse_Count +
                    CirculatingNurse_Count*PreOpNurse_Count + 
                    CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count+
                    CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count*MaintenanceGroup_Count+
                    CirculatingNurse_Count*PreOpNurse_Count*ScrubNurse_Count*MaintenanceGroup_Count*PACUNurse_Count+
                    PreOpNurse_Count*MaintenanceGroup_Count+
                    PreOpNurse_Count*MaintenanceGroup_Count*ScrubNurse_Count+
                    PreOpNurse_Count*MaintenanceGroup_Count*ScrubNurse_Count*PACUNurse_Count+
                    MaintenanceGroup_Count*ScrubNurse_Count+
                    MaintenanceGroup_Count*ScrubNurse_Count*PACUNurse_Count+
                    ScrubNurse_Count*PACUNurse_Count,
                  data = regressionData2); summary(lm.earnings2)
plot(lm.earnings2)
coefficients(lm.earnings2)
