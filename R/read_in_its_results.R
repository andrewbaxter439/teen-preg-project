library(tidyverse)

filenames <- dir("Data/its_outputs")

results <- map(
  filenames,
  ~ readxl::read_xlsx(
    paste0("Data/its_outputs/", .x),
    sheet = "model",
    col_types = c("text", rep("numeric", 4))
  ) %>%
    mutate(model = str_extract(.x, ".*(?=\\.xlsx)")) %>%
    filter(str_detect(Coefficient, "England .* 1999")) %>%
    mutate(Coefficient = str_extract(Coefficient, "level|trend")) %>%
    nest()
) %>%
  transpose() %>%
  flatten_df() %>%
  relocate(model) %>%
  mutate(model = factor(model, levels = paste("model", 1:10))) %>%
  arrange(model) %>%
  mutate(
    comparison = ifelse(model %in% c("model 1", "model 2", "model 5"), 0, 1),
    primary = ifelse(
      model %in% c("model 3", "model 4"),
      "Primary comparison",
      "Robustness and sensitivity"
    )
  )

meta_analyses <- results %>%
  group_by(Coefficient, comparison) %>%
  group_map( ~  list(
    re = metagen(
      Estimate,
      `Standard Error`,
      comb.random = TRUE,
      data = .,
      prediction = FALSE,
      studlab = str_to_sentence(model),
      byvar = primary
    ),
    Coefficient = unique(pull(., Coefficient))
  ), .keep = TRUE)