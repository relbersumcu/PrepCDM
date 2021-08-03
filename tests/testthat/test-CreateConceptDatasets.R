test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test_that("Correct output", {
  library(PrepCDM)
  library(sqldf)
  library(Rcpp)

          FILE <- fread("Events.csv")[,.(Coding_system,Code,sheet)]

          expect_equal(nrow(FILE), 14392)
          Events <- readRDS("Events.rds")
          expect_equal(nrow(Events), 3580)

          CreateConceptDatasets(

            codesheet = FILE,
            c.voc = "Coding_system",
            c.concept = "sheet",
            c.codes = "Code",
            file = Events,
            f.code = "event_code",
            f.voc =  "event_record_vocabulary",
            c.startwith = c("ICD10/CM","ICD9CM","ICD9"),
            path = "Output",
            method = "SQL"

          )


          for(i in unique(FILE[["sheet"]])){
            OUTPUT <- readRDS(paste0("Output/",i,".rds"))[,.(person_id, sheet,event_record_vocabulary, start_date_record,event_code, event_code_2)]
            TEST <- readRDS(paste0("Test/",i,".rds"))[,.(person_id, sheet,event_record_vocabulary, start_date_record,event_code, event_code_2)]

            #expect_equal(sum((nrow(OUTPUT) == nrow(TEST)) == F), 0)
            #expect_equal(sum((ncol(OUTPUT) == ncol(TEST)) == F) > 0, 0)
            #expect_equal(sum((colnames(OUTPUT) == colnames(TEST)) == F), 0)
            expect_equal(sum((OUTPUT == TEST) == F), 0)


          }





})
