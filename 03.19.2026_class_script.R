# 03.19.2026
# JMN
# Unit 4: Fisheries
# 4.1 Joins and Shapes

library(tidyverse)

# practice joins and binds with practice data
# make data frames were gonna use
data1 = data.frame(ID = 1:2,
                    X1 = c("a1", "a2"))
data2 = data.frame(ID = 2:3,
                    X2 = c("b1, b2"))

### left_join() preserves data from your first (left) data and add on additional information from your second (right) data
# 3 equivalent ways to perform the left_join():
  # without specifying the joining varibale:
  data12_left = left_join(data1, data2)
  # explicitly specifying the joing variable:
  data12_left = left_join(data1, data2, by = "ID")
  # with piping
  data12_left = data1 %>%
  left_join(data2, by = "ID")

data12_left

### right_join() is equivalent to left_join(), but reverses the order of the two data frames
  # all rows in the right column of the second data are preserved and the first data is added on
data12_right = right_join(data1, data2)
data12_right

### inner_join() will join data together but only keeps data with matches
data12_inner = data1 %>% 
  inner_join(data2, by = "ID")
data12_inner

### full_join() combines everything from both sets of data and fills in blanks with NAs
data12_full = data1 %>%
  full_join(data2, by = "ID")
data12_full

### semi_join() is a filtering function, so no new columns are created in data1, but rows are removed from data1 based on data2
data12_semi = data1 %>% 
  semi_join(data2, by = "ID")
data12_semi

### anti_join() is a filtering function that only retains infromation from data1 that did NOT match in data2
data12_anti = anti_join(data1, data2, by = "ID")
data12_anti


#### switching between long and wide data frames
# make larger data frame (this is the wide format)
  # wide formats are good for modeling
survey = data.frame(quadrat_id = c(101,102,103, 104),
                    barnacle = c(2,11, 8, 27),
                    chiton = c(1, 0, 0 , 2),
                    mussel = c(0,1,1,4))
  # long formats are better for plotting
long = survey %>% 
  pivot_longer(cols = c("barnacle", "chiton", "mussel"), names_to = "taxa", values_to = "counts")
head(long)
  # back to wide format from long
wide = long %>% 
  pivot_wider(names_from = taxa, values_from = counts)
head(wide)

# Exercise 1.2
ggplot(data = wide) + 
  geom_point(aes(x = quadrat_id, y = barnacle), color = "grey") + 
  geom_point(aes(x = quadrat_id, y = chiton), color = "yellowgreen") + 
  geom_point(aes(x = quadrat_id, y = mussel), color = "cyan")

ggplot(data = long) +
  geom_point(aes(x = quadrat_id, y = counts, color = taxa))
