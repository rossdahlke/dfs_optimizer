# n students
n <- 8

# m courses
m <- 8
capacity <- rep.int(1, m)

# preference data
weight_id_key <- team %>% 
  distinct(id) %>% 
  mutate(weight_id = rank(id))

position_key <- tibble(position_n = 1:8,
                       dfs_position = c("PG", "SG", "SF", "PF", "C", "G", "F", "UTIL"))

team_positions <- team %>% 
  select(id, pg, sg, sf, pf, c, g, f, util) %>% 
  tidyr::pivot_longer(cols = c(pg, sg, sf, pf, c, g, f, util), names_to = "position") %>% 
  group_by(id) %>% 
  mutate(position_n = seq(1, 8, 1)) %>% 
  ungroup() %>% 
  filter(value == 1) %>% 
  select(id, position_n) %>%
  mutate(weight = 1) %>% 
  left_join(weight_id_key) %>% 
  select(-id)

weights <- expand.grid(weight_id = team_positions %>% distinct(weight_id) %>% pull(weight_id), position_n = 1:8) %>% 
  left_join(team_positions) %>% 
  mutate(weight = tidyr::replace_na(weight, -10000))

get_weight <- function(this_weight_id, this_position_n){
  weights %>% 
    filter(weight_id == this_weight_id,
           position_n == this_position_n) %>% 
    pull(weight)
}


eligible_positions <- function(this_weight_id){
  team_positions %>% 
    filter(weight_id == this_weight_id) %>% 
    group_by(weight_id) %>% 
    summarize(positions = paste(position_n, collapse = ",")) %>% 
    pull(positions)
}


library(ompr)
model <- MIPModel() %>%
  # 1 iff student i is assigned to course m
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  # maximize the preferences
  set_objective(sum_expr(get_weight(i, j) * x[i, j], i = 1:n, j = 1:m)) %>%
  # we cannot exceed the capacity of a course
  add_constraint(sum_expr(x[i, j], i = 1:n) <= capacity[j], j = 1:m) %>% 
  # each student needs to be assigned to one course
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n)
model

library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j) %>% 
  rowwise() %>% 
  mutate(weight = get_weight(as.numeric(i), as.numeric(j)), 
         eligible_positions = eligible_positions(i)) %>% 
  ungroup() %>% 
  rename(weight_id = i,
         position_n = j) %>% 
  left_join(weight_id_key) %>% 
  left_join(position_key) %>% 
  select(id, dfs_position)
