library(dplyr)

data_dir <- 'dat/ICPSR_37913/'

load_file <- function(file_number) {
  encoded_file_number <- sprintf("%04d", file_number)
  subdir <- paste0("DS", encoded_file_number, "/")
  filename <- paste0("37913-", encoded_file_number, '-Data.rda')
  filepath <- paste0(
    data_dir, subdir, filename
  )
  e <- new.env()
  load(file=filepath, envir=e)
  return(get(ls(e)[1], envir=e))
}

file_numbers <- list(
  attendance=c(1, 2),
  truancy=c(3, 4),
  suspension=c(5, 6),
  achievement=c(7, 8),
  student_climate=c(9),
  staff_climate=c(10),
  mid_implementation_survey=c(11),
  implementation_rating=c(12)
)


school_covariates <- c(
  "FRL",           # % of students elibible for Free or Reduced Price Lunch
  "PUPTCH",        # Student to full-time teacher ratio
  "PCT_BL",        # % Black
  "PCT_HS",        # % Hispanic
  "PCT_WH",        # % White
  "SMALLSCHOOL",   # Indicator, does school have <250 students in grades 6-7
  "LARGESCHOOL",   # Indicator, does school have >700 students in grades 6-7
  "GRDRNG_ELEM",   # Indicator, does school serve elementary grades
  "GRDRNG_HS"      # Indicator, does school serve secondary (high school) grades
)
school_design_vars <- c(
  "COHORT",               # cohort (1 or 2)
  "TREATMENT",            # treatment (0 or 1)
  "PID",                  # pair ID, a string 'pair##'
  "SCHOOLID",             # pair ID and treatment indicator
  paste0("PAIR", 1:24)    # one-hot encoded pair indicators
)

load_school_info <- function() {
  df <- load_file(file_numbers[["attendance"]][1])
  
  # select only the covars and design vars
  return (df %>% select(all_of(c(school_covariates, school_design_vars))))
}

load_school_level_outcome <- function (outcome) {
  year_1 <- load_file(file_numbers[[outcome]][1])
  year_2 <- load_file(file_numbers[[outcome]][2])
  
  # include all schools from year 1 since some dropped out
  # in year two (pair 14)
  merge_cols <- c(school_design_vars)
  if (outcome == "attendance") {
    merge_cols <- c(merge_cols, "ATTEND_Y0")
  } else if (outcome == "truancy") {
    merge_cols <- c(merge_cols, "TRUANCY_Y0")
  }
  df <- merge(
    year_1,
    year_2,
    by=merge_cols,
    all=TRUE,
    suffixes=c("_Y1", "_Y2")
    )
  return(df)
}

load_school_outcomes <- function() {
  school_outcome_sources <- c("attendance", "truancy", "suspension")
  
  # Load all school-level datasets
  school_dfs <- lapply(school_outcome_sources, load_school_level_outcome)
  
  # Merge datasets
  merged_outcomes <- Reduce(function(x, y) {
    merge(x, y, by=school_design_vars, all=TRUE)
  }, school_dfs)
  
  # Add metadata
  return(merged_outcomes)
}

rm_deleted_demographic_cols <- function(df) {
  to_delete <- c(
      "NATAM", "BLACK", "HISP", "PACIFIC",
      "MULTIRX", "SPED", "POVERTY", "ELL"
  )
  
  # Remove columns that exist in the dataframe
  cols_to_delete <- intersect(to_delete, names(df))
  
  if (length(cols_to_delete) > 0) {
    df <- df %>% select(-all_of(cols_to_delete))
  }
  
  return(df)
}

load_achievement <- function() {
  helper <- function (year) {
    rm_deleted_demographic_cols(
      load_file(file_numbers[["achievement"]][year])
      )
  }
  year_1 <- helper(1)
  year_2 <- helper(2)
  
  # only the 7th and 8th graders in the year 1 df could have had baseline
  # scores from one year prior measured. restrict just to those students.
  year_1 <- year_1 %>% filter(GRADE=="07" | GRADE == "08")
  
  # similarly, only the 8th graders in year 2 could have had
  # baseline scores measured from two years prior.
  year_2 <- year_2 %>% filter(GRADE=="08")
  
  df <- bind_rows(year_1, year_2)
  
  return(df)
}

load_implementation_quality <- function() {
  df <- load_file(12)
  
  # link to usual design variables, PID and TREATMENT
  df <- df %>% rename(
    COHORT=GROUP
  ) %>% mutate(
    pair_id=SCHOOL_CODE - (COHORT * 100)
  )
}