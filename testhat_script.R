library(testthat)


test_folders <- c("~/coding_interview/ADS_Coding_Assignment/question_1_sdtm", "~/coding_interview/ADS_Coding_Assignment/question_2_adam", "~/coding_interview/ADS_Coding_Assignment/question_3_tlg")

for (folder in test_folders) {
  
  # print a status update to R console
  cat("Running tests and saving logs for:", folder, "...\n")
  
  # construct the exact file path for the log
  log_filepath <- file.path(folder, "test_results.txt")
  
  # open a connection to create the text file
  log_file <- file(log_filepath, open = "wt")
  
  # tell testthat to send the output to this specific file
  file_reporter <- SummaryReporter$new(file = log_file)
  
  test_dir(folder, reporter = file_reporter)
  
  close(log_file)
}
