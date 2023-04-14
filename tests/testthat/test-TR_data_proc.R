test_that("Test TR_data_proc", {
  
  library(plyr)
  
  data(lc_data)
  data(pe_data)
  data(wth_data)
  data(sensor_data)
  
  TR_res <- suppressWarnings(TR_data_proc(lc_data = lc_data, pe_data = pe_data,
                         wth_data = wth_data, sensor_data = sensor_data,
                         lastDate = '2018-03-06', skew_test = FALSE))

  # results
  res_raw_1 <- round(TR_res$TR_raw$TR_raw[9:16, 7], 9)

  res_raw_2 <- round(TR_res$TR_raw$TR_raw[9:16, 1000], 9)

  res_smth_1 <- round(TR_res$TR_smth$TR_smth[9:16, 7], 9)

  res_smth_2 <- round(TR_res$TR_smth$TR_smth[9:16, 1000], 9)

  # expectation
  res_raw_1_exp <- c(-0.072000000, -0.019982096, 0.063539859, 0.023909439,
                     -0.023990955, 0.003997874, -0.032000000, -0.092000000)

  res_raw_2_exp <- c(0.007818982, 0.007097146, 0.009995377, 0.004171559,
                     0.009310830, 0.002396387, 0.013252014, 0.005799652)

  res_smth_1_exp <- c(0.001840510, 0.005491953,  0.009301911,  0.009702549,
                      0.004573342,  0.003037703,  0.017749701, -0.014389241)

  res_smth_2_exp <- c(0.007097517, 0.005742810, 0.008706617, 0.003644290,
                      0.008863237, 0.001769410, 0.016620305, 0.005674989)

  # comparison
  expect_equal(res_raw_1, res_raw_1_exp)
  expect_equal(res_raw_2, res_raw_2_exp)
  expect_equal(res_smth_1, res_smth_1_exp)
  expect_equal(res_smth_2, res_smth_2_exp)
  
  expect_equal(2 * 2, 4)
  
})
