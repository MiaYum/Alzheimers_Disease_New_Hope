agedataprocess
================
Aiying Huang
2023-12-07

``` r
library(tidyverse)

# 定义一个处理数据的函数
process_data <- function(input_file) {
  # 读取数据
  data <- read.csv(input_file)
  
  # 使用pivot_longer将数据从宽格式转换为长格式
  data_long <- pivot_longer(data, 
                            cols = -Label..Grouping.,  # 仅排除第二列以后的列
                            names_to = "State_Data", 
                            values_to = "Value") %>%
    select(State_Data, Value)
  
  # 初始化结果数据框
  result <- data.frame(State = character(),
                       Estimate = character(),
                       Percent = character(),
                       stringsAsFactors = FALSE)
  
  # 初始化用于跟踪已出现的州名的列表
  seen_states <- c()
  
  # 遍历数据
  for (i in 1:nrow(data_long)) {
    state_data <- data_long$State_Data[i]
    value <- data_long$Value[i]
    
    # 提取州名
    state <- sub("\\.\\..*", "", state_data)
    
    # 检查州名是否已经在列表中
    if (!(state %in% seen_states)) {
      seen_states <- c(seen_states, state)  # 将州名添加到已出现列表中
      
      # 查找估计值和百分比对应的条目
      estimate_idx <- which(grepl("Estimate", data_long$State_Data) & grepl(state, data_long$State_Data))[1]
      percent_idx <- which(grepl("Percent", data_long$State_Data) & grepl(state, data_long$State_Data))[1]
  
      # 提取估计值和百分比
      estimate <- data_long$Value[estimate_idx]
      percent <- data_long$Value[percent_idx]
  
      # 将数据添加到结果数据框
      result <- rbind(result, data.frame(State = state,  Estimate = estimate, Percent = percent, stringsAsFactors = FALSE))
    }
  }
  
  return(result)
}


# 处理不同年份的数据文件并保存结果
data_2021 <- process_data("./Data/age_over_65/2021.csv")
write.csv(data_2021, file = "./Data/age_over_65/result_2021.csv", row.names = FALSE)

data_2019 <- process_data("./Data/age_over_65/2019.csv")
write.csv(data_2019, file = "./Data/age_over_65/result_2019.csv", row.names = FALSE)

data_2018 <- process_data("./Data/age_over_65/2018.csv")
write.csv(data_2018, file = "./Data/age_over_65/result_2018.csv", row.names = FALSE)
```
