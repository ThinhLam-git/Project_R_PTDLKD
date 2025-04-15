# Dự án phân tích dữ liệu SDG

Dự án này thực hiện tiền xử lý, phân tích và mô hình hóa dữ liệu SDG (Sustainable Development Goals) từ bộ dữ liệu SDR2024.

## Cấu trúc dự án

```
├── README.md                   # Giới thiệu tổng quan về project
├── sdg_analysis.Rproj          # File mở project trong RStudio
├── requirements.txt            # Ghi chú các package sử dụng
│
├── data/                       # Chứa dữ liệu đầu vào / ra
│   ├── raw/                    # Dữ liệu gốc (không chỉnh sửa)
│   └── processed/              # Dữ liệu đã làm sạch
│
├── R/                          # Các script xử lý và phân tích
│   ├── 01_cleaning.R           # Làm sạch dữ liệu
│   ├── 02_eda.R                # Phân tích dữ liệu (EDA)
│   ├── 03_modeling.R           # Huấn luyện mô hình
│   ├── 04_evaluation.R         # Đánh giá mô hình
│   └── utils.R                 # Hàm phụ trợ dùng chung
│
├── output/                     # Kết quả phân tích, bảng, biểu đồ
│   ├── figures/
│   └── tables/
│
├── reports/                    # Báo cáo
│   └── report.Rmd              # R Markdown cho báo cáo cuối cùng
│
└── models/                     # Lưu mô hình đã train (RDS)
```

## Cài đặt và sử dụng

1. Clone repository này về máy của bạn
2. Mở file sdg_analysis.Rproj trong RStudio
3. Cài đặt các packages cần thiết: `Rscript -e "source('R/00_setup.R')"`
4. Chạy quy trình xử lý dữ liệu theo thứ tự:
   - `Rscript R/01_cleaning.R`
   - `Rscript R/02_eda.R`
   - `Rscript R/03_modeling.R`
   - `Rscript R/04_evaluation.R`

## Nguồn dữ liệu

- SDR2024-data.xlsx: Dữ liệu SDG Index từ năm 2010-2024
- CLASS.xlsx: Phân loại quốc gia theo nhóm thu nhập