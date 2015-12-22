baseball = read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA # New Variable Run Difference (Runs Scored - Runs Allowed)

plot(moneyball$RD, moneyball$W)

WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg) # W = 80.88 + 0.11(RD); if W >= 95, then RD has to be >= 133.4

RunsScoredReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsScoredReg) # RS = -804.63 + 2737.77(OBP) + 1584.91(SLG)

RunsAllowedReg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAllowedReg) # RA = -837.38 + 2913.60(OOBP) + 1514.29(OSLG)


player = c("chavez", "giambi", "menechino", "myers", "pena")
obp = c(0.338, 0.391, 0.369, 0.313, 0.361)
slg = c(0.540, 0.450, 0.374, 0.447, 0.500)
salary = c(1400000, 1065000, 295000, 800000, 300000)

roster = data.frame(player, obp, slg, salary)
str(roster)
plot(roster$obp ~ roster$salary)
plot(roster$slg ~ roster$salary)
x = -804.63 + 2737.77*roster$obp + 1584.91*roster$slg


teamRank = c(1, 2, 3, 3, 4, 4, 4, 4, 5, 5)
win2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
win2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)

cor(teamRank, win2012)
cor(teamRank, win2013)
