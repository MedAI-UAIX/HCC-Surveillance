
import pandas as pd
import numpy as np
from pathlib import Path


# ==========================================
# 1. 文件路径
# ==========================================

input_file = Path(r"D:\Python_Learning\score code.csv")
output_file = Path(r"D:\Python_Learning\score_results.csv")


# ==========================================
# 2. aMAP score
# ==========================================

def calculate_amap(age, sex, bilirubin, albumin, platelets):

    score = (
        0.06 * age
        + 0.89 * sex
        + 0.48 * (
            0.66 * np.log10(bilirubin)
            - 0.085 * albumin
        )
        - 0.01 * platelets
        + 7.4
    ) / 14.77 * 100

    return round(float(score), 2)


# ==========================================
# 3. PAGE-B score
# ==========================================

def calculate_page_b(age, sex, platelets):

    # Age
    if age < 30:
        age_score = 0
    elif age < 40:
        age_score = 2
    elif age < 50:
        age_score = 4
    elif age < 60:
        age_score = 6
    elif age < 70:
        age_score = 8
    else:
        age_score = 10

    # Gender: Male=1, Female=0
    sex_score = 6 if sex == 1 else 0

    # Platelets
    if platelets >= 200:
        platelet_score = 0
    elif platelets >= 100:
        platelet_score = 6
    else:
        platelet_score = 9

    return age_score + sex_score + platelet_score


# ==========================================
# 4. mPAGE-B score
# ==========================================

def calculate_mpage_b(age, sex, platelets, albumin):

    # Age
    if age < 30:
        age_score = 0
    elif age < 40:
        age_score = 3
    elif age < 50:
        age_score = 5
    elif age < 60:
        age_score = 7
    elif age < 70:
        age_score = 9
    else:
        age_score = 11

    # Gender
    sex_score = 2 if sex == 1 else 0

    # Platelets
    if platelets >= 250:
        platelet_score = 0
    elif platelets >= 200:
        platelet_score = 2
    elif platelets >= 150:
        platelet_score = 3
    elif platelets >= 100:
        platelet_score = 4
    else:
        platelet_score = 5

    # Albumin
    if albumin >= 40:
        albumin_score = 0
    elif albumin >= 35:
        albumin_score = 1
    elif albumin >= 30:
        albumin_score = 2
    else:
        albumin_score = 3

    return (
        age_score
        + sex_score
        + platelet_score
        + albumin_score
    )


# ==========================================
# 5. 读取 CSV 数据
# ==========================================

def read_csv_file(file_path):

    for encoding in ["utf-8-sig", "gb18030"]:
        try:
            return pd.read_csv(
                file_path,
                encoding=encoding,
                dtype=str,
                keep_default_na=True
            )
        except UnicodeDecodeError:
            continue

    raise ValueError("CSV 文件编码无法识别")


def main():

    if not input_file.exists():
        raise FileNotFoundError(
            f"找不到 CSV 文件: {input_file}"
        )

    df = read_csv_file(input_file)

    # 清理列名两侧空格
    df.columns = df.columns.str.strip()

    # 检查必要变量
    required_columns = [
        "age", "gender", "ALB", "TBIL", "PLT"
    ]

    missing_columns = [
        col for col in required_columns
        if col not in df.columns
    ]

    if missing_columns:
        raise ValueError(
            f"CSV 缺少必要变量: {missing_columns}\n"
            f"实际列名: {df.columns.tolist()}"
        )

    # 将评分所需变量转换为数值
    numeric_data = df[required_columns].apply(
        pd.to_numeric,
        errors="coerce"
    )

    # 检查数据有效性
    valid = (
        numeric_data.notna().all(axis=1)
        & numeric_data["age"].ge(0)
        & numeric_data["gender"].isin([0, 1])
        & numeric_data["ALB"].gt(0)
        & numeric_data["TBIL"].gt(0)
        & numeric_data["PLT"].ge(0)
    )

    # 创建评分结果列
    df["aMAP_score"] = np.nan
    df["PAGE_B_score"] = pd.Series(
        pd.NA, index=df.index, dtype="Int64"
    )
    df["mPAGE_B_score"] = pd.Series(
        pd.NA, index=df.index, dtype="Int64"
    )

    # ======================================
    # 6. 批量计算三种评分
    # ======================================

    for idx in df.index[valid]:

        age = numeric_data.at[idx, "age"]
        sex = int(numeric_data.at[idx, "gender"])
        albumin = numeric_data.at[idx, "ALB"]
        bilirubin = numeric_data.at[idx, "TBIL"]
        platelets = numeric_data.at[idx, "PLT"]

        df.at[idx, "aMAP_score"] = calculate_amap(
            age, sex, bilirubin, albumin, platelets
        )

        df.at[idx, "PAGE_B_score"] = calculate_page_b(
            age, sex, platelets
        )

        df.at[idx, "mPAGE_B_score"] = calculate_mpage_b(
            age, sex, platelets, albumin
        )

    # ======================================
    # 7. 导出 CSV
    # ======================================

    df.to_csv(
        output_file,
        index=False,
        encoding="utf-8-sig"
    )

    print("\n评分计算完成！")
    print(f"原始患者数: {len(df)}")
    print(f"成功计算人数: {int(valid.sum())}")
    print(f"无效或缺失人数: {int((~valid).sum())}")
    print(f"结果文件: {output_file}")


# ==========================================
# 8. 运行程序
# ==========================================

if __name__ == "__main__":
    main()
