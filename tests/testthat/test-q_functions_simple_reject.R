skip("WIP")

skip_on_cran()

test_that("q_power_mediation: rejection", {

# ===== Simple mediation =====

system.time(
out <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    nrep = 10,
    R = 199,
    seed = 1234,
    mode = "region",
    max_trials = 1,
    progress = !is_testing(),
    parallel = FALSE,
    simulation_progress = !is_testing(),
    tolerance = .20
  )
)

system.time(
out2 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "s",
    target_power = .80,
    R = 199,
    final_nrep = 60,
    final_R = 199,
    seed = 1234,
    mode = "n",
    progress = !is_testing(),
    control = list(total_nrep = 200),
    parallel = FALSE
  )
)

system.time(
out3 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "s",
    nrep = 10,
    R = 199,
    n = 100,
    seed = 1234,
    mode = "power",
    progress = !is_testing(),
    parallel = FALSE
  )
)

rejection_rates(out)
rejection_rates(out2)
rejection_rates(out3)

chk1 <- rejection_rates(out, all_columns = TRUE)
chk2 <- rejection_rates(out$n_region_from_power, all_columns = TRUE)
expect_equal(chk1$reject,
             chk2$reject)

chk1 <- rejection_rates(out2, all_columns = TRUE)
chk2 <- rejection_rates(out2$n_from_power, all_columns = TRUE)
expect_equal(chk1$reject,
             chk2$reject)

chk1 <- rejection_rates(out3, all_columns = TRUE)
chk2 <- rejection_rates(out3$power4test, all_columns = TRUE)
expect_equal(chk1$reject,
             chk2$reject)
})
