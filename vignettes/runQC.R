## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## working when developing vignette
require(tidyverse, quietly = TRUE)
require(remora, quietly = TRUE)

## ----files 1, eval=FALSE------------------------------------------------------
#  files <- list(det = "path_to/IMOS_detections.csv",
#                rmeta = "path_to/IMOS_receiver_deployment_metadata.csv",
#                tmeta = "path_to/IMOS_transmitter_deployment_metadata.csv",
#                meas = "path_to/IMOS_animal_measurements.csv")

## ----files 2, warning=FALSE---------------------------------------------------
files <- list(det = system.file(file.path("test_data","IMOS_detections.csv"), package = "remora"),
              rmeta = system.file(file.path("test_data","IMOS_receiver_deployment_metadata.csv"),
                    package = "remora"),
              tmeta = system.file(file.path("test_data","IMOS_transmitter_deployment_metadata.csv"),
                    package = "remora"),
              meas = system.file(file.path("test_data","IMOS_animal_measurements.csv"),
                    package = "remora"))

## ----runQC, eval = FALSE, message=FALSE---------------------------------------
#  tag_qc <- runQC(files, .parallel = TRUE, .progress = FALSE)

## ----save QC, eval=FALSE, echo=FALSE, message=FALSE---------------------------
#  tag_qc <- runQC(files, .parallel = TRUE, .progress = FALSE)
#  save(tag_qc, file = "R/tag_qc.rda")

## ----QC output, echo = FALSE--------------------------------------------------
load("R/tag_qc.rda")
tag_qc

## ----grab dQC, message=FALSE--------------------------------------------------
dQC <- grabQC(tag_qc, what = "dQC", flag = c("valid","likely valid"))
dQC

## ----grab dQC2, message=FALSE-------------------------------------------------
dQC <- grabQC(tag_qc, what = "dQC")
dQC

## ----grab meta, message=FALSE-------------------------------------------------
tag_meta <- grabQC(tag_qc, what = "tag_meta")
rec_meta <- grabQC(tag_qc, what = "rec_meta")
meas <- grabQC(tag_qc, what = "meas")

## ----unnest tag_qc, message=FALSE---------------------------------------------
require(tidyverse)
qc <- tag_qc %>%
  unnest(cols = QC) %>%
  ungroup()

qc

## ----plotQC, eval=FALSE, message=FALSE, warning=FALSE, fig.align='center'-----
#  plotQC(tag_qc)

