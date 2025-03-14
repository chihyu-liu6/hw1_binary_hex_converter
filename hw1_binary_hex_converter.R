library(tidyverse)

# 輸入十進位數字
decimal_input <- as.integer(readline(prompt = "請輸入一個 0 到 255 的十進位數字: "))

# 檢查是否為 NA 和輸入範圍
if (is.na(decimal_input) || decimal_input < 0 || decimal_input > 255) {
  stop("輸入必須是有效的數字，並且必須在 0 到 255 之間。")
}

# 將十進位數字轉換為二進位
to_binary <- function(x) {
  binary_output <- ""
  for (i in 7:0) {  # 使用 'i' 作為循環變量
    # 檢查 x 的第 i 位是否為 1
    if ((x & (1 < i)) != 0) {  # 正確的位移運算
      binary_output <- paste0(binary_output, "1")
    } else {
      binary_output <- paste0(binary_output, "0")
    }
  }
  return(binary_output)
}

# 呼叫函數並顯示結果
binary_output <- to_binary(decimal_input)
cat("二進位:", binary_output, "\n")

# 直接將十進位數字轉換為十六進位
decimal_to_hexadecimal <- function(x) {
  hex_chars <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")
  hex_output <- ""
  
  while (x > 0) {
    remainder <- x %% 16
    hex_output <- paste0(hex_chars[remainder + 1], hex_output)  # +1 因為 R 的索引從 1 開始
    x <- x %/% 16
  }
  
  # 確保至少有一位輸出
  if (nchar(hex_output) == 0) {
    hex_output <- "0"
  }
  
  return(hex_output)
}
# 呼叫函數並顯示結果
hex_output <- decimal_to_hexadecimal(decimal_input)
cat("十六進位:", hex_output, "\n")

