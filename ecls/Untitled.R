ecls <- read.csv("ecls.csv")

library(optmatch)
library(RItools)

eclscomplete <- ecls[complete.cases(ecls),]

full_form <- catholic ~ race_white + race_black + race_hispanic + race_asian +
  p5numpla + p5hmage + p5hdage + w3momscr + w3dadscr +
  w3income + w3povrty + p5fstamp + c5r2mtsc

save <- matrix(nrow = 7, ncol = 3)
colnames(save) <- c("Model",
                    "Time",
                    "ESS")

# Model 1: Pairmatching
s1 <- system.time(pm1 <- pairmatch(full_form, data = eclscomplete))
save[1,] <- c("Pair match",
              s1[1],
              effectiveSampleSize(pm1))

# Model 2: Pairmatching 1:2
s2 <- system.time(pm2 <- pairmatch(full_form, controls = 2, data = eclscomplete))
save[2,] <- c("Pair match (1:2)",
              s2[1],
              effectiveSampleSize(pm2))

# Model 3: Unrestricted fullmatch
s3 <- system.time(fm1 <- fullmatch(full_form, data = eclscomplete))
save[3,] <- c("Full match (unres.)",
              s3[1],
              effectiveSampleSize(fm1))

# Model 4: Fullmatch with ExactMatch
s4 <- system.time({
  em <- exactMatch(catholic ~ race, data = eclscomplete)
  dist <- match_on(full_form, within = em, data = eclscomplete)
  fm2 <- fullmatch(dist, data = eclscomplete)
})
save[4,] <- c("Full + Exact",
              s4[1],
              effectiveSampleSize(fm2))

# Model 5: Propensity score pair match
s5 <- system.time({
  psmod <- glm(full_form, data = eclscomplete, family = binomial)
  ps <- predict(psmod, type = "response")
  psm1 <- pairmatch(catholic ~ ps, data = eclscomplete)
})
save[5,] <- c("PS Pair",
              s5[1],
              effectiveSampleSize(psm1))

# Model 6: PS Full match
s6 <- system.time({
  psmod <- glm(full_form, data = eclscomplete, family = binomial)
  ps <- predict(psmod, type = "response")
  psm2 <- fullmatch(catholic ~ ps, data = eclscomplete)
})
save[6,] <- c("PS Full",
              s6[1],
              effectiveSampleSize(psm2))

# Model 7: PS Full + Exact
s7 <- system.time({
  em <- exactMatch(catholic ~ race, data = eclscomplete)
  psmod <- glm(full_form, data = eclscomplete, family = binomial)
  ps <- predict(psmod, type = "response")
  psm3 <- fullmatch(catholic ~ ps, data = eclscomplete, within = em)
})
save[7,] <- c("PSM + Exact",
              s7[1],
              effectiveSampleSize(psm3))



dev.off()
plot(xBalance(full_form, strata = list(unstrat = NULL,
                                                 pair = ~pm1,
                                                 pair2 = ~pm2,
                                                 full = ~fm1,
                                                 full_em = ~fm2,
                                                 psp = ~psm1,
                                                 psf = ~psm2,
                                                 psf_em = ~psm3),
                        data = eclscomplete),
               colors = 1:8,
               absolute = TRUE)


library(lme4)
resp_form <- c5r2mtsc ~ catholic + race_white + race_black + race_hispanic + race_asian +
  p5numpla + p5hmage + p5hdage + w3momscr + w3dadscr + I(w3income/10^3) +
  w3povrty + p5fstamp
c(coef(lm(resp_form, data = eclscomplete))[2],
  fixef(lmer(update(resp_form, . ~ . + (1 | pm1)), data = eclscomplete))[2],
  fixef(lmer(update(resp_form, . ~ . + (1 | pm2)), data = eclscomplete))[2],
  fixef(lmer(update(resp_form, . ~ . + (1 | fm1)), data = eclscomplete))[2],
  fixef(lmer(update(resp_form, . ~ . + (1 | fm2)), data = eclscomplete))[2],
  fixef(lmer(update(resp_form, . ~ . + (1 | psm1)), data = eclscomplete))[2],
  fixef(lmer(update(resp_form, . ~ . + (1 | psm2)), data = eclscomplete))[2],
  fixef(lmer(update(resp_form, . ~ . + (1 | psm3)), data = eclscomplete))[2])
