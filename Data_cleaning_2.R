## Clean and Load data -----------------------------------------------------
# Import google sheet data and get authorization --------------------------

sheet <- read_csv("data/Instructor-led Trainings Scheduling & Outreach - Scheduling.csv") %>% 
  janitor::clean_names()


# Select data needed ------------------------------------------------------

col_index <- which(sheet[1, ] == "REMAINING")[1] # see where the word "remaining" is

sheet_selected <- sheet %>%
  select(1:(col_index-1)) %>%  # select needed columns
  filter(row_number() < which(training == "TOTAL")[1]) #select needed rows

## generate new column name based on the first row
new_col_name <- sapply(1:ncol(sheet_selected), function(i) {
  val <- sheet_selected[1,i]
  
  # Check for NA values
  if (is.na(val)) {
    return(names(sheet_selected)[i])
  }
  
  # Check for numbers from 1 to 10
  if (str_detect(val, "^([1-9]|10)$")) {
    return(paste0("region ", val))
  } else {
    return(names(sheet_selected)[i])
  }
}) # if the row is number from 1-10 then use the number, otherwise copy the column name

# Assign the new values to the first row

colnames(sheet_selected) <- new_col_name

# Generate a dataset for Dashboard ----------------------------------------

df_plotting <- sheet_selected %>% 
  slice(-1) %>% # remove the first row of the dataset
  select("training", starts_with("region")) %>% # select training and region data
  fill(!!names(sheet_selected)[1], .direction = "down") %>% #To fill NA values in the first column with the value from the row above
  filter(str_detect(as.character(.[[1]]), "[0-9]"))


df_s <- df_plotting %>% 
  pivot_longer(cols = starts_with("region"),
               names_to = "region",
               values_to = "states"
  )

df_s <- df_s %>% 
  select(training, states) %>% 
  na.omit() %>% 
  group_by(training, states) %>% 
  summarise(n_each_course_state = n())
