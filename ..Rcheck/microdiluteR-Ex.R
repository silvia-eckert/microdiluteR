pkgname <- "microdiluteR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "microdiluteR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('microdiluteR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bma")
### * bma

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bma
### Title: Absorption values from six broth microdilution assays conducted
###   on 96-well plates
### Aliases: bma
### Keywords: datasets

### ** Examples

data(bma)
attr(bma, "metadata")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bma", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_well_positions")
### * check_well_positions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_well_positions
### Title: Check monotonicity of well positions across groups
### Aliases: check_well_positions check_monotonicity

### ** Examples

# Generate example data
set.seed(123)
df <- data.frame(Position = rep(1:21, 2),
                 Value = c(1:21, sample(1:21,21, TRUE)),
                 Timepoint = rep(paste0("T",1:3),14),
                 Validity = "valid",
                 Group_1 = rep(LETTERS[1:2], each=21),
                 Group_2 = rep(letters[1:14], each = 3))
# All groups behave monotonically
check_well_positions(df[df$Group_1 == "A",],
                     x_var = "Timepoint",
                     y_var = "Value",
                     grouping = c("Group_1", "Group_2"))
# Six groups behave non-monotonically
check_well_positions(df[df$Group_1 == "B",],
                     x_var = "Timepoint",
                     y_var = "Value",
                     grouping = c("Group_1", "Group_2"))
# Check if a vector is monotonically increasing (will return TRUE)
check_monotonicity(c(1, 2, 3, 4, 5))
# Check if a vector is monotonically decreasing (will return FALSE)
check_monotonicity(c(5, 80, 3, 2, 1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_well_positions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_plates_via_params")
### * tidy_plates_via_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_plates_via_params
### Title: Tidy multiple 96-well plates via parameters
### Aliases: tidy_plates_via_params

### ** Examples

# Load example data
data(bma)
# Add metadata from user parameters
bma_tidy <- tidy_plates_via_params(input_data = bma,
                                   direction = "horizontal",
                                   group_IDs = paste0("Group_", letters[1:2]),
                                   experiment_names = c("Experiment 1", "Experiment 2"),
                                   validity_method = "threshold",
                                   threshold = 1,
                                   treatment_labels = LETTERS[1:8],
                                   concentration_levels = seq(from=80, to=10, length.out=8))
bma_tidy # View tidy data



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_plates_via_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_single_plate")
### * tidy_single_plate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_single_plate
### Title: Tidy single 96-well plate via parameters
### Aliases: tidy_single_plate

### ** Examples

# Load example data
data(bma)
# Add metadata from user parameters
bma_tidy <- tidy_single_plate(input_data = bma[1],
                       direction = "horizontal",
                       group_ID = "Group A",
                       experiment_name = "Experiment 1",
                       validity_method = "threshold",
                       threshold = 1,
                       treatment_labels = LETTERS[1:8],
                       concentration_levels = seq(from=80, to=10, length.out=8))
bma_tidy # View tidy data



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_single_plate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("validate_cells")
### * validate_cells

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: validate_cells
### Title: Check validity of each cell in data frame.
### Aliases: validate_cells apply_validation_method ask_validity_method
###   ask_threshold ask_invalid_samples update_validity

### ** Examples

df <- data.frame(Position = c("pos1", "pos2", "pos2", "pos4", "pos4"),
                 Value = c(1, 2, 3, 4, 5),
                 Validity = c("valid", "valid", "valid", "valid", "valid"),
                 A = c("a1", "a2", "a3", "a1", "a2"),
                 B = c("b1", "b2", "b3", "b1", "b2"),
                 C = c("c1", "c2", "c3", "c1", "c2"))
updated_df <- update_validity(df,
                              well_positions = "pos2",
                              group_levels = list(A = c("a2", "a3"), B = c("b2", "b3")))
updated_df



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("validate_cells", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
