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
  
  res_maxTR_raw_1 <- round(TR_res$TR_raw$Max_TR_raw[1:8, 7], 8)
  
  res_maxTR_raw_2 <- round(TR_res$TR_raw$Max_TR_raw[1:8, 20], 8)

  res_smth_1 <- round(TR_res$TR_smth$TR_smth[9:16, 7], 9)

  res_smth_2 <- round(TR_res$TR_smth$TR_smth[9:16, 1000], 9)
  
  res_maxTR_smth_1 <- round(TR_res$TR_smth$Max_TR_smth[1:8, 7], 9)
  
  res_maxTR_smth_2 <- round(TR_res$TR_smth$Max_TR_smth[1:8, 20], 9)
  

  # expectation
  res_raw_1_exp <- c(-0.072000000, -0.019982096, 0.063539859, 0.023909439,
                     -0.023990955, 0.003997874, -0.032000000, -0.092000000)

  res_raw_2_exp <- c(0.007818982, 0.007097146, 0.009995377, 0.004171559,
                     0.009310830, 0.002396387, 0.013252014, 0.005799652)
  
  res_maxTR_raw_1_exp <- c(0.01833608, 0.04786913, 0.05418592, 0.05757049,
                           0.03622473, 0.02550852, 0.12232779, 0.02988043)
  
  res_maxTR_raw_2_exp <- c(0.01798949, 0.02096147, 0.02099034, 0.01741609,
                           0.01849858, 0.01105027, 0.14513267, 0.02207394)

  res_smth_1_exp <- c(0.001840510, 0.005491953,  0.009301911,  0.009702549,
                      0.004573342,  0.003037703,  0.017749701, -0.014389241)

  res_smth_2_exp <- c(0.007097517, 0.005742810, 0.008706617, 0.003644290,
                      0.008863237, 0.001769410, 0.016620305, 0.005674989)
  
  res_maxTR_smth_1_exp <- c(0.007667335, 0.007653379, 0.009976469, 0.004952690,
                            0.009930344, 0.002439110, 0.016926085, 0.009960856)
  
  res_maxTR_smth_2_exp <- c(0.007806514, 0.006720601, 0.009599827, 0.003660202,
                            0.009557078, 0.003009100, 0.016014783, 0.006046211)

  # comparison
  expect_equal(res_raw_1, res_raw_1_exp)
  expect_equal(res_raw_2, res_raw_2_exp)
  
  expect_equal(res_maxTR_raw_1, res_maxTR_raw_1_exp)
  expect_equal(res_maxTR_raw_2, res_maxTR_raw_2_exp)
  
  expect_equal(res_smth_1, res_smth_1_exp)
  expect_equal(res_smth_2, res_smth_2_exp)
  
  expect_equal(res_maxTR_smth_1, res_maxTR_smth_1_exp)
  expect_equal(res_maxTR_smth_2, res_maxTR_smth_2_exp)
  
})
