## test that extractEnv successfully downloads, fills any data gaps & 
##    appends current variables to QC'd data
test_that("extractEnv adds rs_current data", {
  ##  extract Env variable using example data
  data("TownsvilleReefQC")
  qc_data <- tidyr::unnest(TownsvilleReefQC, cols = c(QC))
  qc_data <- dplyr::ungroup(qc_data)
  qc_data <- dplyr::filter(qc_data, Detection_QC %in% c(1,2))
  qc_data <- dplyr::filter(qc_data, filename == unique(filename)[1])
  qc_data <- dplyr::slice(qc_data, 5:8)
  
  qc_data1 <- extractEnv(df = qc_data,
                         X = "receiver_deployment_longitude", 
                         Y = "receiver_deployment_latitude", 
                         datetime = "detection_datetime", 
                         env_var = "rs_current",
                         cache_layers = FALSE,
                         crop_layers = TRUE,
                         full_timeperiod = FALSE,
                         fill_gaps = TRUE,
                         folder_name = "test",
                         .parallel = TRUE)
  sub <- qc_data1[1, 56:60]
  expect_named(sub, c("rs_gsla", 
                      "rs_vcur", 
                      "rs_ucur", 
                      "rs_current_velocity", 
                      "rs_current_bearing"))
})

test_that("extractEnv adds rs_sst_interpolated data", {
  ##  extract Env variable using example data
  data("TownsvilleReefQC")
  qc_data <- tidyr::unnest(TownsvilleReefQC, cols = c(QC))
  qc_data <- dplyr::ungroup(qc_data)
  qc_data <- dplyr::filter(qc_data, Detection_QC %in% c(1,2))
  qc_data <- dplyr::filter(qc_data, filename == unique(filename)[1])
  qc_data <- dplyr::slice(qc_data, 5:8)
  
  qc_data1 <- extractEnv(df = qc_data,
                         X = "receiver_deployment_longitude", 
                         Y = "receiver_deployment_latitude", 
                         datetime = "detection_datetime", 
                         env_var = "rs_sst_interpolated",
                         cache_layers = FALSE,
                         crop_layers = TRUE,
                         full_timeperiod = FALSE,
                         fill_gaps = TRUE,
                         folder_name = "test",
                         .parallel = FALSE)
  sub <- qc_data1[1, ncol(qc_data1)]
  expect_named(sub, c("rs_sst_interpolated"))
})

test_that("extractEnv adds rs_chl data", {
  ##  extract Env variable using example data
  data("TownsvilleReefQC")
  qc_data <- tidyr::unnest(TownsvilleReefQC, cols = c(QC))
  qc_data <- dplyr::ungroup(qc_data)
  qc_data <- dplyr::filter(qc_data, Detection_QC %in% c(1,2))
  qc_data <- dplyr::filter(qc_data, filename == unique(filename)[1])
  qc_data <- dplyr::slice(qc_data, 5:8)
  
  qc_data1 <- extractEnv(df = qc_data,
                         X = "receiver_deployment_longitude", 
                         Y = "receiver_deployment_latitude", 
                         datetime = "detection_datetime", 
                         env_var = "rs_chl",
                         cache_layers = FALSE,
                         crop_layers = TRUE,
                         full_timeperiod = FALSE,
                         fill_gaps = TRUE,
                         folder_name = "test",
                         .parallel = FALSE)
  sub <- qc_data1[1, ncol(qc_data1)]
  expect_named(sub, c("rs_chl"))
})
