## test that runQC successfully QC's example data
test_that("runQC warns when tag metadata are missing", {
  ##  extract Env variable using example data
  files <- list(det = system.file(file.path("test_data","IMOS_detections.csv"),
                                  package = "remora"),
                rmeta = system.file(file.path("test_data","IMOS_receiver_deployment_metadata.csv"),
                                    package = "remora"),
                tmeta = NULL,
                meas = system.file(file.path("test_data","IMOS_animal_measurements.csv"),
                                   package = "remora"))
  
  expect_warning(qc.out <- runQC(files, .progress = FALSE), "transmitter metadata not supplied, skipping tests for missing metadata records", )
  expect_s3_class(qc.out, "remora_QC")
  expect_s3_class(qc.out, "rowwise_df")
})

test_that("runQC does not error", {
  ##  extract Env variable using example data
  files <- list(det = system.file(file.path("test_data","IMOS_detections.csv"),
                                  package = "remora"),
                rmeta = system.file(file.path("test_data",
                                              "IMOS_receiver_deployment_metadata.csv"),
                                    package = "remora"),
                tmeta = system.file(file.path("test_data",
                                              "IMOS_transmitter_deployment_metadata.csv"),
                                    package = "remora"),
                meas = system.file(file.path("test_data",
                                             "IMOS_animal_measurements.csv"),
                                   package = "remora"))
  
  expect_no_error(qc.out <- runQC(files, .progress = FALSE))
  expect_s3_class(qc.out, "remora_QC")
  expect_s3_class(qc.out, "rowwise_df")
})