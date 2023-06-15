## test that expected 5 named current variables are appended to qc_data
test_that("extractEnv adds rs_current data", {
  ##  extract Env variable using example data
  data("TownsvilleReefQC")
  qc_data <- tidyr::unnest(TownsvilleReefQC, cols = c(QC))
  qc_data <- dplyr::ungroup(qc_data)
  qc_data <- dplyr::filter(qc_data, Detection_QC %in% c(1,2))
  qc_data <- dplyr::filter(qc_data, filename == unique(filename)[1])
  qc_data <- dplyr::slice(qc_data, 8:12)
  qc_data1 <- extractEnv(df = qc_data,
             X = "receiver_deployment_longitude", 
             Y = "receiver_deployment_latitude", 
             datetime = "detection_datetime", 
             env_var = "rs_current",
             cache_layers = FALSE,
             crop_layers = TRUE,
             full_timeperiod = TRUE,
             fill_gaps = TRUE,
             folder_name = "test",
             .parallel = TRUE)
  sub <- qc_data1[1, 56:60]
  expect_named(sub, c("rs_gsla", "rs_vcur", "rs_ucur", "rs_current_velocity", "rs_current_bearing"))
})
