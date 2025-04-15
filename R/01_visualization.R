# 02_eda.R
# Script phân tích khám phá dữ liệu (EDA)

# Load các thư viện cần thiết
library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(patchwork)
library(corrplot)

# Load các hàm tiện ích
source("R/utils.R")

# -------------------- Đọc dữ liệu đã làm sạch --------------------
message("Đang đọc dữ liệu đã làm sạch...")
df_cleaned <- readRDS("data/processed/SDR_data_cleaned.rds")
df_group <- readRDS("data/processed/country_income_groups.rds")

# Kết hợp dữ liệu với nhóm thu nhập
df_with_group <- left_join(df_cleaned, df_group, by = "Country")

# -------------------- Phân tích tổng quan --------------------
message("Đang phân tích tổng quan...")

# Thống kê mô tả
summary_stats <- summary(df_cleaned)
write.csv(as.data.frame(summary_stats), "output/tables/summary_statistics.csv")

# Số lượng quốc gia theo nhóm thu nhập
countries_by_group <- df_with_group %>%
    filter(year == 2024) %>%
    group_by(`Income group`) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

# Lưu thống kê
write.csv(countries_by_group, "output/tables/countries_by_income_group.csv", row.names = FALSE)

# Biểu đồ số lượng quốc gia theo nhóm thu nhập
p_countries_by_group <- ggplot(countries_by_group, aes(x = reorder(`Income group`, -count), y = count, fill = `Income group`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = count), vjust = -0.5) +
    scale_fill_viridis_d() +
    labs(
        title = "Số lượng quốc gia theo nhóm thu nhập",
        x = "Nhóm thu nhập",
        y = "Số lượng quốc gia"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("output/figures/countries_by_income_group.png", p_countries_by_group, width = 10, height = 6)

# -------------------- Phân tích biến mục tiêu --------------------
message("Đang phân tích biến mục tiêu (SDG Index Score)...")

# Phân phối chỉ số SDG theo năm
p_sdg_by_year <- ggplot(df_cleaned, aes(x = factor(year), y = `SDG Index Score`)) +
    geom_boxplot(fill = "lightblue") +
    labs(
        title = "Phân phối Chỉ số SDG theo năm",
        x = "Năm",
        y = "Chỉ số SDG"
    ) +
    theme_minimal()

ggsave("output/figures/sdg_distribution_by_year.png", p_sdg_by_year, width = 10, height = 6)

# Phân phối chỉ số SDG theo nhóm thu nhập năm 2024
p_sdg_by_income <- ggplot(
    df_with_group %>% filter(year == 2024),
    aes(
        x = reorder(`Income group`, -`SDG Index Score`, median),
        y = `SDG Index Score`,
        fill = `Income group`
    )
) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    labs(
        title = "Phân phối Chỉ số SDG theo nhóm thu nhập (2024)",
        x = "Nhóm thu nhập",
        y = "Chỉ số SDG"
    ) +
    theme_minimal()

ggsave("output/figures/sdg_by_income_group_2024.png", p_sdg_by_income, width = 10, height = 6)

# Xu hướng chỉ số SDG theo thời gian và nhóm thu nhập
sdg_trends <- df_with_group %>%
    group_by(year, `Income group`) %>%
    summarise(
        avg_sdg = mean(`SDG Index Score`, na.rm = TRUE),
        .groups = "drop"
    )

p_sdg_trends <- ggplot(sdg_trends, aes(x = year, y = avg_sdg, color = `Income group`, group = `Income group`)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    labs(
        title = "Xu hướng Chỉ số SDG theo Nhóm thu nhập",
        x = "Năm",
        y = "Chỉ số SDG trung bình"
    ) +
    theme_minimal()
ggsave("output/figures/sdg_trends_by_income_group.png", p_sdg_trends, width = 10, height = 6)    

# -------------------- Phân tích tương quan --------------------
message("Đang phân tích tương quan giữa các biến...")

# Chọn các biến số cho phân tích tương quan
numeric_vars <- df_cleaned %>%
  filter(year == 2024) %>%
  select(`SDG Index Score`, `SDG1 Score`, `SDG2 Score`, `SDG3 Score`, `SDG4 Score`, 
         `SDG5 Score`, `SDG6 Score`, `SDG7 Score`, `SDG8 Score`, `SDG9 Score`, 
         `SDG10 Score`, `SDG11 Score`, `SDG12 Score`, `SDG13 Score`, `SDG14 Score`, 
         `SDG15 Score`, `SDG16 Score`, `SDG17 Score`)

# Tính ma trận tương quan
corr_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Lưu ma trận tương quan
write.csv(corr_matrix, "output/tables/correlation_matrix.csv")

# Vẽ biểu đồ tương quan
png("output/figures/correlation_plot.png", width = 1000, height = 1000, res = 100)
corrplot(corr_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.7, tl.cex = 0.8)
dev.off()

# -------------------- Phân tích các SDG riêng lẻ --------------------
message("Đang phân tích các mục tiêu SDG riêng lẻ...")

# Tạo dữ liệu dạng dài cho các điểm SDG
sdg_long <- df_with_group %>%
  filter(year == 2024) %>%
  select(Country, `Income group`, matches("SDG\\d+ Score")) %>%
  pivot_longer(
    cols = matches("SDG\\d+ Score"),
    names_to = "SDG",
    values_to = "Score"
  ) %>%
  mutate(SDG = str_extract(SDG, "SDG\\d+"))

# Điểm trung bình của từng SDG theo nhóm thu nhập
sdg_by_income <- sdg_long %>%
  group_by(`Income group`, SDG) %>%
  summarise(avg_score = mean(Score, na.rm = TRUE),
            .groups = 'drop')

# Biểu đồ điểm trung bình của từng SDG theo nhóm thu nhập
p_sdg_by_income <- ggplot(sdg_by_income, aes(x = SDG, y = avg_score, fill = `Income group`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d() +
  labs(title = "Điểm trung bình của từng SDG theo nhóm thu nhập (2024)",
       x = "Mục tiêu phát triển bền vững",
       y = "Điểm trung bình") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/sdg_by_income_group_detailed.png", p_sdg_by_income, width = 12, height = 7)

# Top 10 và bottom 10 quốc gia theo chỉ số SDG năm 2024
top10_countries <- df_cleaned %>%
  filter(year == 2024) %>%
  arrange(desc(`SDG Index Score`)) %>%
  head(10) %>%
  select(Country, `SDG Index Score`)

bottom10_countries <- df_cleaned %>%
  filter(year == 2024) %>%
  arrange(`SDG Index Score`) %>%
  head(10) %>%
  select(Country, `SDG Index Score`)

# Lưu kết quả
write.csv(top10_countries, "output/tables/top10_countries_2024.csv", row.names = FALSE)
write.csv(bottom10_countries, "output/tables/bottom10_countries_2024.csv", row.names = FALSE)

# Biểu đồ top 10 và bottom 10
p_top10 <- ggplot(top10_countries, aes(x = reorder(Country, `SDG Index Score`), y = `SDG Index Score`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 quốc gia có chỉ số SDG cao nhất (2024)",
       x = "Quốc gia",
       y = "Chỉ số SDG") +
  theme_minimal()

p_bottom10 <- ggplot(bottom10_countries, aes(x = reorder(Country, -`SDG Index Score`), y = `SDG Index Score`)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "10 quốc gia có chỉ số SDG thấp nhất (2024)",
       x = "Quốc gia",
       y = "Chỉ số SDG") +
  theme_minimal()

# Kết hợp hai biểu đồ
p_combined <- p_top10 / p_bottom10
ggsave("output/figures/top_bottom_countries_2024.png", p_combined, width = 10, height = 10)

# -------------------- Phân tích khoảng cách (gap) --------------------
message("Đang phân tích khoảng cách trong việc đạt được các SDG...")

# Tính khoảng cách trung bình cho mỗi SDG (lý tưởng là 100)
sdg_gaps <- sdg_long %>%
  group_by(SDG) %>%
  summarise(avg_score = mean(Score, na.rm = TRUE),
            gap = 100 - avg_score,
            .groups = 'drop') %>%
  arrange(desc(gap))

# Lưu kết quả
write.csv(sdg_gaps, "output/tables/sdg_gaps.csv", row.names = FALSE)

# Biểu đồ khoảng cách
p_gaps <- ggplot(sdg_gaps, aes(x = reorder(SDG, -gap), y = gap)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = round(gap, 1)), vjust = -0.5, size = 3) +
  labs(title = "Khoảng cách trong việc đạt được các SDG (2024)",
       subtitle = "Khoảng cách = 100 - Điểm trung bình",
       x = "Mục tiêu phát triển bền vững",
       y = "Khoảng cách") +
  theme_minimal()
ggsave("output/figures/sdg_gaps.png", p_gaps, width = 10, height = 6)

# -------------------- Phân tích biến động --------------------
message("Đang phân tích biến động chỉ số SDG theo thời gian...")

# Tính sự thay đổi từ năm đầu tiên đến năm gần nhất
change_over_time <- df_cleaned %>%
  group_by(Country) %>%
  filter(n() > 1) %>%
  summarise(
    first_year = min(year),
    last_year = max(year),
    first_score = `SDG Index Score`[year == first_year],
    last_score = `SDG Index Score`[year == last_year],
    change = last_score - first_score,
    .groups = 'drop'
  ) %>%
  filter(!is.na(change)) %>%
  arrange(desc(change))

# Lưu kết quả
write.csv(change_over_time, "output/tables/sdg_change_over_time.csv", row.names = FALSE)

# Top 10 quốc gia có sự cải thiện tốt nhất
top_improvers <- change_over_time %>%
  arrange(desc(change)) %>%
  head(10)

# Top 10 quốc gia có xu hướng giảm nhiều nhất
top_decliners <- change_over_time %>%
  arrange(change) %>%
  head(10)

# Biểu đồ cho top improvers và decliners
p_improvers <- ggplot(top_improvers, aes(x = reorder(Country, change), y = change)) +
  geom_bar(stat = "identity", fill = "green3") +
  geom_text(aes(label = round(change, 1)), hjust = -0.2) +
  coord_flip() +
  labs(title = "10 quốc gia có sự cải thiện SDG tốt nhất",
       subtitle = paste("Từ", min(top_improvers$first_year), "đến", max(top_improvers$last_year)),
       x = "Quốc gia",
       y = "Thay đổi trong Chỉ số SDG") +
  theme_minimal()

p_decliners <- ggplot(top_decliners, aes(x = reorder(Country, -change), y = change)) +
  geom_bar(stat = "identity", fill = "red3") +
  geom_text(aes(label = round(change, 1)), hjust = 1.2) +
  coord_flip() +
  labs(title = "10 quốc gia có sự suy giảm SDG nhiều nhất",
       subtitle = paste("Từ", min(top_decliners$first_year), "đến", max(top_decliners$last_year)),
       x = "Quốc gia",
       y = "Thay đổi trong Chỉ số SDG") +
  theme_minimal()

# Kết hợp hai biểu đồ
p_change_combined <- p_improvers / p_decliners
ggsave("output/figures/top_improvers_decliners.png", p_change_combined, width = 10, height = 10)

# -------------------- Kết thúc phân tích --------------------
message("Phân tích khám phá dữ liệu hoàn thành!")