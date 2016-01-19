source("2015/Scripts/Perch/data_init.R")

spring_lw %<>% mutate(logl=log(tl),
         logw=log(wt))

fall_lw %<>% mutate(logl=log(tl),
                      logw=log(wt))

fit_wp <- lm(logw~logl,data=filter(fall_lw,species=="White Perch"))
fit_yp <- lm(logw~logl,data=filter(fall_lw,species=="Yellow Perch"))
fit_gs <- lm(logw~logl,data=filter(fall_lw,species=="Gizzard Shad"))
fit_ss <- lm(logw~logl,data=filter(fall_lw,species=="Spottail Shiner"))
fit_wb <- lm(logw~logl,data=filter(fall_lw,species=="White Bass"))
fit_rg <- lm(logw~logl,data=filter(fall_lw,species=="Round Goby"))

exp(predict(fit_wp,data.frame(logl=log(46)),interval="confidence"))
exp(predict(fit_yp,data.frame(logl=log(62)),interval="confidence"))
exp(predict(fit_gs,data.frame(logl=log(83)),interval="confidence"))
exp(predict(fit_ss,data.frame(logl=log(18.2)),interval="confidence"))
exp(predict(fit_wb,data.frame(logl=log(62)),interval="confidence"))
exp(predict(fit_rg,data.frame(logl=log(13.7)),interval="confidence"))

