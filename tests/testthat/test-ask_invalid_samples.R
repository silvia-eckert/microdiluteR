# Test case 1
test_that("Test whether ask_invalid_samples() stores well positions from user prompt as character vector.", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- "A2 B1 C4 3\n"
  write(input, f)
  
  # Call function
  result <- ask_invalid_samples()
  
  # Test cases
  expect_equal(result, c("A2", "B1", "C4", "3"))
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
