# An algorithm function should named alg_*
# These are the required arguments
# - object
# - x
# - pop_es_names
# - ... # To force other arguments to be named
# - target_power
# - x_max
# - x_min
# - progress
# - x_interval
# - x_include_interval
# - simulation_progress
# - save_sim_all
# - is_by_x
# - object_by_org
# - final_nrep
# - final_R
# - ci_level
# - extendInt
# - max_trials
# - R

# === power_curve ===
# - xs_per_trial
# - nrep0
# - R0
# - power_model
# - start
# - lower_bound
# - upper_bound
# - nls_control
# - nls_args
# - nrep_steps
# - final_R
# - final_xs_per_trial
# - pre_i_xs
# - pre_i_nrep
# - pre_i_R
# - power_min
# - power_max
# - power_tolerance_in_interval
# - power_tolerance_in_final

# === bisection ===
# - digits
# - lower_hard
# - upper_hard
# - extend_maxiter
# - what
# - goal
# - tol
# - variants

# These are the required elements in the output
# - by_x_1
# - fit_1
# - x_tried
# - x_out
# - power_out
# - nrep_out
# - ci_out
# - by_x_out
# - i2
# - ci_hit
# - solution_found
# - what
# - goal

# ** by_x_1 **
# The collection of all values tried and their results
# to be updated when new value is tried.
# Used after the end of the loop.

# ** fit_1 **
# The latest power curve
# To be updated whenever by_x_1 is updated.
# Used after the end of the loop.

# ** x_tried **
# The sample sizes or population values
# examined.

# ** x_out **
# The output in the last trial.

# ** power_out **
# The power in the last trial.

# ** nrep_out **
# The nrep in the last trial.

# ** ci_out **
# The ci in the last trial.

# ** by_x_out **
# The by_* output in the last trial

# ** i2 **
# The row number of the last trial
# in by_x_1.

# ** ci_hit **
# TRUE if the CI in by_x_1 contains the target_power

# ** solution_found **
# TRUE if an acceptable solution is found

# These are the required elements in the output for goal = close_enough
# - tol

# ** tol **
# The tolerance used when goal = close_enough.
# This is required to check whether a value is a solution.