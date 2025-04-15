# utils.R
# Các hàm tiện ích dùng chung trong dự án

# Hàm phân tích dữ liệu thiếu
analyze_missing_data <- function(df) {
    missing_percent <- colMeans(is.na(df)) * 100 # nolint
    missing_df <- data.frame(
        column = names(missing_percent), # nolint
        percent_missing = missing_percent
    ) %>%
        arrange(desc(percent_missing)) # nolint

    return(missing_df)
}

# Hàm xác định cột quan trọng có tỷ lệ thiếu cao
identify_important_high_missing_cols <- function(df, target_column,
                                                 missing_threshold = 0.6,
                                                 correlation_threshold = 0.7) {
    # Tính phần trăm giá trị thiếu
    missing_percent <- colMeans(is.na(df))

    # Tính tương quan với biến mục tiêu (chỉ cho cột số)
    numeric_cols <- df %>%
        select_if(is.numeric) %>%
        colnames()
    correlations <- cor(df[numeric_cols], use = "pairwise.complete.obs")[, target_column]
    correlations <- abs(correlations)

    # Xác định cột quan trọng có tỷ lệ thiếu cao
    high_missing_cols <- names(missing_percent[missing_percent > missing_threshold])
    important_cols <- names(correlations[correlations > correlation_threshold])

    # Giao của hai tập hợp
    important_high_missing <- intersect(high_missing_cols, important_cols)

    return(important_high_missing)
}

# Hàm loại bỏ các cột không quan trọng có tỷ lệ thiếu cao
drop_non_important_high_missing <- function(df, important_cols, missing_threshold = 0.6) {
    # Tính phần trăm giá trị thiếu cho mỗi cột
    missing_percent <- colMeans(is.na(df))

    # Cột có tỷ lệ thiếu > ngưỡng
    high_missing_cols <- names(missing_percent[missing_percent > missing_threshold]) # nolint: line_length_linter.

    # Loại trừ cột quan trọng khỏi danh sách loại bỏ
    cols_to_drop <- setdiff(high_missing_cols, important_cols)

    # Loại bỏ cột
    df_dropped <- df %>% select(-all_of(cols_to_drop)) # nolint

    return(list(
        data = df_dropped,
        dropped_columns = cols_to_drop
    ))
}

# Hàm điền giá trị thiếu theo nhóm
fill_missing_by_group <- function(df, group_col, group_value, target_col, method = "mean") {
    if (!method %in% c("mean", "median")) {
        stop("Method must be 'mean' or 'median'")
    }

    # Lọc dữ liệu cho nhóm
    group_mask <- df[[group_col]] == group_value

    # Tính giá trị trung bình hoặc trung vị cho nhóm
    if (method == "mean") {
        fill_value <- mean(df[[target_col]][group_mask], na.rm = TRUE)
    } else {
        fill_value <- median(df[[target_col]][group_mask], na.rm = TRUE)
    }

    # Điền giá trị thiếu cho nhóm
    df[[target_col]][group_mask & is.na(df[[target_col]])] <- fill_value

    return(df)
}

# Hàm nội suy KNN
knn_imputation <- function(data, k = 5) {
    df <- data

    # Loại bỏ cột toàn NA
    df <- df %>% select_if(~ !all(is.na(.)))

    # Xác định cột số
    numeric_cols <- df %>%
        select_if(is.numeric) %>%
        colnames()

    # Trích xuất và nội suy các cột số
    numeric_df <- df[numeric_cols]
    imputed_data <- VIM::kNN(numeric_df, k = k)

    # Chỉ giữ lại dữ liệu đã nội suy không có chỉ báo nội suy
    imputed_cols <- numeric_cols[numeric_cols %in% colnames(imputed_data)]
    df[imputed_cols] <- imputed_data[imputed_cols]

    return(df)
}

# Hàm vẽ đồ thị phân phối theo cột
plot_distribution_by_column <- function(df, col, group_col, title = NULL) {
    if (is.null(title)) {
        title <- paste("Average", col, "by", group_col, "Over Time")
    }

    avg_df <- df %>%
        group_by(year, !!sym(group_col)) %>%
        summarise(avg_val = mean(!!sym(col), na.rm = TRUE), .groups = "drop")

    plot <- ggplot(avg_df, aes(x = year, y = avg_val, color = !!sym(group_col), group = !!sym(group_col))) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        labs(
            title = title,
            y = paste("Average", col),
            x = "Year"
        ) +
        theme_minimal() +
        theme(legend.title = element_text(face = "bold"))

    return(plot)
}

# Hàm tính và trực quan hóa tương quan
calculate_correlation <- function(df, target_column) {
    # Tạo bản sao của DataFrame và loại bỏ cột target
    df_analysis <- df %>%
        select(-all_of(target_column)) %>%
        select_if(is.numeric)

    # Tính ma trận tương quan với cột target (giữ lại target trong tính toán tương quan)
    correlation_with_target <- cor(df %>% select_if(is.numeric),
        use = "pairwise.complete.obs"
    )[, target_column]

    # Loại bỏ chính target column khỏi kết quả
    correlation_with_target <- correlation_with_target[names(correlation_with_target) != target_column]

    # Sắp xếp tương quan
    correlation_with_target <- sort(correlation_with_target, decreasing = TRUE)

    return(correlation_with_target)
}

# Hàm vẽ biểu đồ tương quan
plot_correlation <- function(df, feature, target_column, correlation, type = "positive") {
    if (type == "positive") {
        color <- "green"
        line_color <- "darkgreen"
    } else {
        color <- "red"
        line_color <- "darkred"
    }

    plot <- ggplot(df, aes_string(x = feature, y = target_column)) +
        geom_point(alpha = 0.5, color = color) +
        geom_smooth(method = "lm", color = line_color) +
        labs(
            title = paste(
                ifelse(type == "positive", "Positive", "Negative"),
                "Correlation:", feature, "vs", target_column
            ),
            subtitle = paste("Corr:", round(correlation, 2))
        ) +
        theme_minimal()

    return(plot)
}
