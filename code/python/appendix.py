from PIL import Image
import os

# =========================
# 1. 路径设置
# =========================
input_dir = "./merged"               # 原图所在文件夹
output_dir = "./merged/combined"     # 输出文件夹
os.makedirs(output_dir, exist_ok=True)

farm_ids = range(11, 21)   # Farm11 到 Farm20

# 文件名格式
file_templates = [
    "farm{farm_id}_n_I_plot_merged.png",
    "farm{farm_id}_E_plot_merged.png",
    "farm{farm_id}_exposure_plot_merged.png"
]

# =========================
# 2. 参数设置
# =========================
# 三张图都裁掉右侧图例
crop_keep_ratio = 0.80   # 可按需要调整：0.78 / 0.82 / 0.85

# 图片之间的垂直间距
vertical_gap = 20

# 最终拼图背景色：白色
background_color = (255, 255, 255)

# 如果原图是 RGBA，并且黑色只是透明底显示效果，
# 可以把 use_alpha_composite 设为 True
use_alpha_composite = False

# =========================
# 3. 函数
# =========================
def crop_right_legend(img, keep_ratio=0.80):
    """
    裁掉右边图例区域，只保留左边 keep_ratio 比例
    """
    w, h = img.size
    new_w = int(w * keep_ratio)
    return img.crop((0, 0, new_w, h))


def paste_on_white_if_needed(img):
    """
    如果图片带透明通道，则先铺到白底上；
    否则直接转 RGB
    """
    if img.mode in ("RGBA", "LA") or (img.mode == "P" and "transparency" in img.info):
        rgba = img.convert("RGBA")
        white_bg = Image.new("RGBA", rgba.size, (255, 255, 255, 255))
        merged = Image.alpha_composite(white_bg, rgba)
        return merged.convert("RGB")
    else:
        return img.convert("RGB")


def merge_three_images_vertical(img1, img2, img3, gap=20, bg_color=(255, 255, 255)):
    """
    将三张图竖向拼接，底层为白色背景
    """
    w1, h1 = img1.size
    w2, h2 = img2.size
    w3, h3 = img3.size

    final_width = max(w1, w2, w3)
    final_height = h1 + h2 + h3 + gap * 2

    canvas = Image.new("RGB", (final_width, final_height), bg_color)

    y = 0
    canvas.paste(img1, (0, y))
    y += h1 + gap

    canvas.paste(img2, (0, y))
    y += h2 + gap

    canvas.paste(img3, (0, y))

    return canvas


# =========================
# 4. 主循环
# =========================
for farm_id in farm_ids:
    file_paths = [
        os.path.join(input_dir, tpl.format(farm_id=farm_id))
        for tpl in file_templates
    ]

    missing_files = [fp for fp in file_paths if not os.path.exists(fp)]
    if missing_files:
        print(f"[跳过] Farm{farm_id} 缺少文件：")
        for mf in missing_files:
            print("   ", mf)
        continue

    # 读取原图
    raw_img1 = Image.open(file_paths[0])
    raw_img2 = Image.open(file_paths[1])
    raw_img3 = Image.open(file_paths[2])

    # 如果有透明背景，先铺成白底
    img1 = paste_on_white_if_needed(raw_img1)
    img2 = paste_on_white_if_needed(raw_img2)
    img3 = paste_on_white_if_needed(raw_img3)

    # 三张图都裁掉右边图例
    img1_cropped = crop_right_legend(img1, keep_ratio=crop_keep_ratio)
    img2_cropped = crop_right_legend(img2, keep_ratio=crop_keep_ratio)
    img3_cropped = crop_right_legend(img3, keep_ratio=crop_keep_ratio)

    # 竖向拼接，白底
    merged_img = merge_three_images_vertical(
        img1_cropped,
        img2_cropped,
        img3_cropped,
        gap=vertical_gap,
        bg_color=background_color
    )

    # 保存
    output_path = os.path.join(output_dir, f"farm{farm_id}_combined_vertical_no_legend_whitebg.png")
    merged_img.save(output_path, dpi=(300, 300))
    print(f"[完成] 已保存: {output_path}")

print("全部处理完成。")