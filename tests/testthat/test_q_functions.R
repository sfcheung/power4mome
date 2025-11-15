skip("WIP")

options(power4mome.bz = TRUE)
out <- q_power_region_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n_start = 50,
    R = 199,
    nrep = 400,
    seed = 1234,
    parallel = TRUE
  )

out
plot(out)

options(power4mome.bz = TRUE)
out <- q_power_region_mediation_simple(
    a = "m",
    b = "m",
    cp = "m",
    n_start = 50,
    R = 199,
    nrep = 400,
    seed = 1234,
    parallel = TRUE
  )

out
plot(out)

