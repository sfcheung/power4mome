skip("WIP")

options(power4mome.bz = TRUE)
system.time(
out <- q_power_region_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    R = 199,
    seed = 1234
  )
)
out
summary(out)
plot(out)

options(power4mome.bz = TRUE)
system.time(
out1 <- q_power_region_mediation_simple(
    a = "m",
    b = "m",
    cp = "m",
    reliability = .70,
    number_of_indicators = 6,
    n = 50,
    R = 199,
    seed = 1234
  )
)

out1
summary(out1)
plot(out1)

options(power4mome.bz = TRUE)
system.time(
out2 <- q_power_region_mediation_simple(
    a = "m",
    b = "m",
    cp = "m",
    reliability = c(x = .70,
                    m = .80,
                    y = .60),
    number_of_indicators = c(x = 6,
                             y = 7,
                             m = 5),
    n = 50,
    R = 199,
    seed = 1234
  )
)

out2
summary(out2)
plot(out2)

options(power4mome.bz = TRUE)
system.time(
out3 <- q_power_region_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    R = 199,
    x_fun = list(x = list(rexp_rs)),
    seed = 1234
  )
)
out
summary(out)
plot(out)
