skip("WIP")

# ===== Simple mediation =====

options(power4mome.bz = TRUE)
system.time(
out <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    R = 199,
    seed = 1234,
    mode = "region",
    nrep = 20
  )
)
out
plot(out)
summary(out)

system.time(
outp <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    R = 199,
    seed = 1234,
    nrep = 20
  )
)
outp
expect_error(plot(outp))
expect_error(summary(outp))

options(power4mome.bz = TRUE)
system.time(
out1 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "m",
    reliability = .70,
    number_of_indicators = 6,
    n = 1999,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)

out1
summary(out1)
plot(out1)

options(power4mome.bz = TRUE)
system.time(
outp1 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "m",
    reliability = .70,
    number_of_indicators = 6,
    n = 1999,
    R = 199,
    seed = 1234
  )
)

outp1
rejection_rates(outp1)

options(power4mome.bz = TRUE)
system.time(
out2 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "m",
    reliability = c(x = .70,
                    m = .80,
                    y = .60),
    number_of_indicators = c(x = 6,
                             y = 7,
                             m = 5),
    n = 1000,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)

out2
summary(out2)
plot(out2)

options(power4mome.bz = TRUE)
system.time(
out3 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    R = 199,
    x_fun = list(x = list(rexp_rs)),
    seed = 1234,
    mode = "region"
  )
)
out
summary(out)
plot(out)

# ===== Parallel mediation =====

options(power4mome.bz = TRUE)
system.time(
outp <- q_power_mediation_parallel(
    as = c("s", "m"),
    bs = c("m", "s"),
    cp = "n",
    n = 100,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)
outp
summary(outp)
plot(outp)

options(power4mome.bz = TRUE)
system.time(
outp1 <- q_power_mediation_parallel(
    as = c("m", "m"),
    bs = c("m", "m"),
    cp = "n",
    n = 500,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)
outp1
summary(outp1)
plot(outp1)


options(power4mome.bz = TRUE)
system.time(
outp2 <- q_power_mediation_parallel(
    as = c("m", "m", "m"),
    bs = c("m", "m", "m"),
    cp = "n",
    n = 500,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)
outp2
summary(outp2)
plot(outp2)

options(power4mome.bz = TRUE)
system.time(
outp3 <- q_power_mediation_parallel(
    as = c("m", "m", "m", "m"),
    bs = c("m", "m", "m", "m"),
    cp = "n",
    n = 500,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)
outp3
summary(outp3)
plot(outp3)

# ===== Serial mediation =====

options(power4mome.bz = TRUE)
system.time(
outs <- q_power_mediation_serial(
    ab = c("s", "m", "l"),
    ab_others = "n",
    cp = "s",
    n = 100,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)
outs
summary(outs)
plot(outs)

options(power4mome.bz = TRUE)
system.time(
outs1 <- q_power_mediation_serial(
    ab = c("l", "l", "l", "l"),
    ab_others = "n",
    cp = "s",
    n = 200,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)
outs1
summary(outs1)
plot(outs1)

options(power4mome.bz = TRUE)
system.time(
outs2 <- q_power_mediation_serial(
    ab = c("l", "l", "l", "l"),
    ab_others = "m",
    cp = "m",
    n = 200,
    R = 199,
    seed = 1234,
    mode = "region"
  )
)
outs2
summary(outs2)
plot(outs2)

# ===== Arbitrary model =====

options(power4mome.bz = TRUE)
model <-
"
m1 ~ x
m21 ~ m1
m22 ~ m1
y ~ m21 + m22
"
pop_es <-
"
m1 ~ x: m
m21 ~ m1: m
m22 ~ m1: m
y ~ m21: m
y ~ m22: m
"

system.time(
outa <- q_power_mediation(
    model = model,
    pop_es = pop_es,
    n = 100,
    R = 199,
    test_fun = test_k_indirect_effects,
    test_more_args = list(x = "x",
                          y = "y",
                          omnibus = "all"),
    seed = 1234,
    mode = "region"
  )
)
outa
summary(outa)
plot(outa)
