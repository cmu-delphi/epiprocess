test_that("`archive_cases_dv_subset_dt` is formed successfully", {
  expect_true(is_epi_archive(archive_cases_dv_subset_dt))
})

test_that("`delayed_assign_with_unregister_awareness` works as expected on good promises", {
  # Since we're testing environment stuff, use some "my_" prefixes to try to
  # prevent naming coincidences from changing behavior.
  my_eval_env = rlang::new_environment(list(x=40L, n_evals=0L), parent=rlang::base_env())
  my_assign_env = rlang::new_environment()
  delayed_assign_with_unregister_awareness("good1", {
    n_evals <- n_evals + 1L
    x + 2L
  }, my_eval_env, my_assign_env)
  force(my_assign_env[["good1"]])
  force(my_assign_env[["good1"]])
  force(my_assign_env[["good1"]])
  expect_identical(my_assign_env[["good1"]], 42L)
  expect_identical(my_eval_env[["n_evals"]], 1L)
})

test_that("original `delayedAssign` works as expected on good promises", {
  my_eval_env = rlang::new_environment(list(x=40L, n_evals=0L), parent=rlang::base_env())
  my_assign_env = rlang::new_environment()
  delayedAssign("good1", {
    n_evals <- n_evals + 1L
    x + 2L
  }, my_eval_env, my_assign_env)
  force(my_assign_env[["good1"]])
  force(my_assign_env[["good1"]])
  force(my_assign_env[["good1"]])
  expect_identical(my_assign_env[["good1"]], 42L)
  expect_identical(my_eval_env[["n_evals"]], 1L)
})

test_that("`delayed_assign_with_unregister_awareness` doesn't wrap a buggy promise if not unregistering", {
  delayed_assign_with_unregister_awareness("x", Abort("msg", class="original_error_class"))
  expect_error(force(x), class="original_error_class")
})

test_that("`delayed_assign_with_unregister_awareness` doesn't wrap a buggy promise if not unregistering", {
  delayed_assign_with_unregister_awareness("x", Abort("msg", class="original_error_class"))
  # Take advantage of a false positive / hedge against package renaming: make
  # our own `unregister` function to trigger the special error message.
  unregister = function(y) y
  expect_error(unregister(force(x)), class="epiprocess__promise_evaluation_error_during_unregister")
})

test_that("`delayed_assign_with_unregister_awareness` injection support works", {
  my_exprs = rlang::exprs(a = b + c, d = e)
  delayed_assign_with_unregister_awareness(
    "good2", list(!!!my_exprs),
    eval.env=rlang::new_environment(list(b=2L, c=3L, e=4L), rlang::base_env())
  )
  force(good2)
  expect_identical(good2, list(a=5L, d=4L))
})

test_that("`some_package_is_being_unregistered` doesn't fail in response to non-simple calls", {
  # Prerequisite for current implementation to work (testing here to help debug
  # in case some R version doesn't obey):
  expect_false(NA_character_ %in% letters)
  f = function() function() some_package_is_being_unregistered()
  my_expr = rlang::expr(f()())
  # Prerequisite for this to test to actually be testing on non-simple calls:
  expect_false(rlang::is_call_simple(my_expr))
  # Actual test (`FALSE` is correct; `NA` or error is not):
  expect_false(rlang::eval_bare(my_expr))
})
