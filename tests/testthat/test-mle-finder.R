test_that("linalg and optm least-sq coincide", {
          # simulate data
          n_obs <- 32
          n_pred <- 4
          data <- simulate_data(n_obs, n_pred, seed=1918)
          outcome <- data$outcome
          design <- data$design
          # get MLE using analytical formula
          linalg_out <- hiper_glm(design, outcome, model = "linear")
          # get MLE using stats::optim
          optim_out <- hiper_glm(design, outcome, model = "linear", option = list(mle_finder = "BFGS"))
          # compare the two
          expect_true(
            are_all_close(coef(linalg_out), coef(optim_out))
          )
          })


# devtools::test()
