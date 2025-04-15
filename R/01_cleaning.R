# 01_cleaning.R
# Script làm sạch dữ liệu

# Load các thư viện cần thiết
library(tidyverse)
library(readxl)
library(writexl)
library(VIM)

# Load các hàm tiện ích
source("R/utils.R")

# Định nghĩa đường dẫn
path_data <- "data/raw/SDR2024-data.xlsx"
path_group <- "data/raw/CLASS.xlsx"

# Tạo thư mục kết quả nếu chưa tồn tại
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# -------------------- Đọc dữ liệu --------------------
message('Đang đọc dữ liệu...')
# Đọc tất cả các sheet từ file Excel
sheets_list <- excel_sheets(path_data)
data <- lapply(sheets_list, function(sheet) {
  read_excel(path_data, sheet = sheet)
})
names(data) <- sheets_list

# Đọc dữ liệu nhóm
group_sheets <- excel_sheets(path_group)
group_data <- lapply(group_sheets, function(sheet) {
  read_excel(path_group, sheet = sheet)
})
names(group_data) <- group_sheets

# -------------------- Trích xuất và hợp nhất dữ liệu --------------------
message('Đang trích xuất và hợp nhất dữ liệu...')

# Lấy các bảng dữ liệu riêng biệt
df_raw <- data[["Raw Data - Panel"]]
df_backdated <- data[["Backdated SDG Index"]]
df_overview <- data[["Overview"]]
df_fulldb <- data[["Full Database"]]

# Xử lý dữ liệu nhóm thu nhập
df_group <- group_data[["List of economies"]] %>% 
  select(`Economy`, `Income group`) %>%
  rename(Country = `Economy`)

# Sửa nhóm thu nhập cho Venezuela
df_group <- df_group %>%
  mutate(`Income group` = ifelse(Country == "Venezuela, RB", "Upper middle income", `Income group`))

# Lọc dữ liệu chỉ số SDG
df_backdated <- df_backdated %>% select(id, Country, year, `SDG Index Score`)
df_overview <- df_overview %>% select(`2024 SDG Index Score`, Country)

# Hợp nhất dữ liệu
df_merged <- left_join(df_raw, df_backdated, by = c("Country", "year"))

df_merged <- df_merged %>% 
  select(-id.x, -id.y, -indexreg)

# Hợp nhất chỉ số SDG 2024
df_merged <- left_join(df_merged, df_overview, by = "Country")

# Điền giá trị thiếu trong 'SDG Index Score' nơi year == 2024
df_merged <- df_merged %>%
  mutate(`SDG Index Score` = ifelse(is.na(`SDG Index Score`) & year == 2024, 
                                   `2024 SDG Index Score`, 
                                   `SDG Index Score`))

# Loại bỏ cột thừa
df_merged <- df_merged %>% select(-`2024 SDG Index Score`)

# Loại bỏ dòng không có chỉ số SDG
df_merged <- df_merged %>% filter(!is.na(`SDG Index Score`))

# Cập nhật giá trị năm 2024 với giá trị từ năm tham chiếu
numeric_cols <- df_merged %>% select_if(is.numeric) %>% colnames()
message('Đang cập nhật giá trị cho năm 2024...')

for (i in 1:nrow(df_merged)) { # nolint: seq_linter.
  if (df_merged$year[i] == 2024) {
    current_country <- df_merged$Country[i]
    for (col in numeric_cols) {
      year_col_name <- paste("Year:", col)
      if (year_col_name %in% colnames(df_fulldb)) {
        match_row <- df_fulldb %>% filter(Country == current_country)
        if (nrow(match_row) > 0) {
          year_value <- match_row[[year_col_name]]
          if (!is.na(year_value)) {
            value <- df_merged %>% 
              filter(Country == current_country, year == year_value) %>%
              select(all_of(col))
            if (nrow(value) > 0 && !is.na(value[[col]][1])) {
              df_merged[i, col] <- value[[col]][1]
            }
          }
        }
      }
    }
  }
}

# -------------------- Tiền xử lý dữ liệu --------------------
message('Đang tiền xử lý dữ liệu...')

# Phân tích dữ liệu thiếu
missing_df <- analyze_missing_data(df_merged)
write.csv(missing_df, "output/tables/missing_values_analysis.csv", row.names = FALSE)

# Xác định cột quan trọng có tỷ lệ thiếu cao
important_cols <- identify_important_high_missing_cols(df_merged, target_column = 'SDG Index Score')
message(paste("Cột quan trọng có tỷ lệ thiếu >60%:", paste(important_cols, collapse = ", ")))

# Loại bỏ cột không quan trọng có tỷ lệ thiếu cao
result <- drop_non_important_high_missing(df_merged, important_cols)
df_dropped <- result$data
dropped_columns <- result$dropped_columns
message(paste("Loại bỏ cột:", paste(dropped_columns, collapse = ", ")))

# Lưu danh sách cột đã loại bỏ
writeLines(dropped_columns, "output/tables/dropped_columns.txt")

# Thống kê tỷ lệ thiếu theo cột và năm
percent_null_per_column <- colMeans(is.na(df_dropped))
percent_null_by_year <- df_dropped %>%
  filter(year %in% c(2023, 2024)) %>%
  group_by(year) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100))

write.csv(as.data.frame(percent_null_per_column), 
          "output/tables/missing_percent_per_column.csv")
write.csv(percent_null_by_year, 
          "output/tables/missing_percent_by_year.csv")

# Hợp nhất với dữ liệu nhóm thu nhập
df_dropped_with_group <- left_join(df_dropped, df_group, by = "Country")

# Sửa nhóm thu nhập cho một số quốc gia
df_dropped_with_group <- df_dropped_with_group %>%
  mutate(`Income group` = case_when(
    Country %in% c('Sao Tome and Principe', "Cote d'Ivoire") ~ 'Lower middle income',
    TRUE ~ `Income group`
  ))

message(paste("Số dòng thiếu nhóm thu nhập:", sum(is.na(df_dropped_with_group$`Income group`))))

# -------------------- Điền giá trị thiếu theo nhóm thu nhập --------------------
message('Đang điền giá trị thiếu theo nhóm thu nhập...')

country_groups <- c('Low income', 'Lower middle income', 'Upper middle income', 'High income')

# Điền giá trị thiếu theo nhóm thu nhập
columns_to_fill <- list(
  median = c('sdg2_stunting', 'sdg3_uhc', 'sdg8_accounts', 'sdg11_slums', 
             'sdg16_u5reg', 'sdg16_clabor', 'sdg16_justice', 'sdg17_statperf'),
  mean = c('sdg3_pollmort', 'sdg9_roads', 'sdg11_transport', 'sdg12_ewaste')
)

# Điền giá trị thiếu cho các cột dùng median
for (col in columns_to_fill$median) {
  for (group in country_groups) {
    df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 
                                                  'Income group', 
                                                  group, 
                                                  col, 
                                                  method = 'median')
  }
}

# Điền giá trị thiếu cho các cột dùng mean
for (col in columns_to_fill$mean) {
  for (group in country_groups) {
    df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 
                                                  'Income group', 
                                                  group, 
                                                  col, 
                                                  method = 'mean')
  }
}

# Xử lý riêng cho sdg4_earlyedu
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'High income', 'sdg4_earlyedu', method = 'median')
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'Low income', 'sdg4_earlyedu', method = 'median')
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'Lower middle income', 'sdg4_earlyedu', method = 'median')
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'Upper middle income', 'sdg4_earlyedu', method = 'mean')

# Xử lý riêng cho sdg4_literacy
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'High income', 'sdg4_literacy', method = 'mean')
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'Upper middle income', 'sdg4_literacy', method = 'mean')
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'Lower middle income', 'sdg4_literacy', method = 'median')
df_dropped_with_group <- fill_missing_by_group(df_dropped_with_group, 'Income group', 'Low income', 'sdg4_literacy', method = 'median')

# Kiểm tra lại số giá trị thiếu sau khi điền
message(paste("Số giá trị thiếu trong sdg2_stunting:", sum(is.na(df_dropped_with_group$sdg2_stunting))))

# Loại bỏ cột nhóm thu nhập
df_dropped_processed <- df_dropped_with_group %>% select(-`Income group`)

# -------------------- Áp dụng nội suy KNN --------------------
message('Đang áp dụng nội suy KNN cho các giá trị thiếu còn lại...')

# Áp dụng nội suy KNN
df_cleaned <- knn_imputation(df_dropped_processed, k = 5)

# Kiểm tra số giá trị thiếu sau khi nội suy
message(paste("Số giá trị thiếu sau khi nội suy:", sum(is.na(df_cleaned))))

# -------------------- Lưu dữ liệu đã làm sạch --------------------
message('Đang lưu dữ liệu đã làm sạch...')

# Lưu dữ liệu đã làm sạch
write.csv(df_cleaned, "data/processed/SDR_data_cleaned.csv", row.names = FALSE)
saveRDS(df_cleaned, "data/processed/SDR_data_cleaned.rds")

# Lưu dữ liệu gốc đã trích xuất để sử dụng sau
saveRDS(df_merged, "data/processed/SDR_data_merged.rds")
saveRDS(df_group, "data/processed/country_income_groups.rds")

message('Hoàn thành tiền xử lý dữ liệu!')