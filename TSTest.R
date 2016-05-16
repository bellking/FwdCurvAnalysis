plot(AirPassengers)

apts <- ts(AirPassengers, frequency=12)
f <- decompose(apts)
plot(f)
fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L)


seat <- as.zoo(log(UKDriverDeaths))
seat <- merge(y = seat, y1 = lag(seat, k = -1),
              y12 = lag(seat, k = -12), all = FALSE)

rr <- rollapply(seat, width = 36,
                FUN = function(z) coef(lm(y ~ y1 + y12, data = as.data.frame(z))),
                by.column = FALSE, align = "right")


seat <- NULL
rr <- NULL

tt <- as.Date("2000-01-01") + c(1, 2, 5, 6, 7, 8, 10)
z <- zoo(seq_along(tt), tt)
g <- zoo(, seq(start(z), end(z), "day"))

zm <- merge(z, g)
rollapply(zm, 3, mean, na.rm = TRUE, fill = NA)

tt <- NULL
z <- NULL
g <- NULL

L2 <- rep(list(-(2:0)), 10)

L2[5] <- list(NULL)
L2[7] <- list(0:1)


str(L2)

y <- 10 * seq(12); k <- 4; d <- 3

y <- timeSequence(from = "2001-01-01", to = "2009-01-01", by = "day")




