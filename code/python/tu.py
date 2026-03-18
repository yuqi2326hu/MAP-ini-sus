from PIL import Image, ImageDraw, ImageFont
from pathlib import Path
import re

print("Pillow OK")

# =========================
# 参数区
# =========================

ROOT = Path(".")
OUT_DIR = ROOT / "merged"
OUT_DIR.mkdir(exist_ok=True)

# ✅ 顺序改为：weekly, monthly, seasonal, annually
FREQS = ["wk", "mo", "se", "an"]

PLOT_TYPES = ["n_I_plot", "E_plot", "exposure_plot"]

# --- 裁剪比例分开控制 ---
CROP_RIGHT_RATIO = 0.88  # 针对 n_I_plot 和 E_plot
CROP_EXPOSURE_RATIO = 0.86  # 针对 exposure_plot 专门设置
# -----------------------

GRID_GAP = 0
GRID_GAP_Y = 80

LABEL_FONT_SIZE = 100
LABEL_COLOR = (0, 0, 0, 255)
LABEL_SHIFT_X = 80
LABEL_IN_GAP_Y_RATIO = 0.1

LEGEND_HEIGHT_RATIO = 0.3
LEGEND_RIGHT_MARGIN = 100

# ===== 标题样式参数 =====
TITLE_FONT_SIZE = 82
TITLE_TEXT_COLOR = (0, 0, 0, 255)
TITLE_BG_COLOR = (255, 255, 255, 255)
TITLE_PADDING = 12
TITLE_OFFSET_X = 1200
TITLE_OFFSET_Y = 28


# =========================
# 工具函数
# =========================

def crop_custom(img: Image.Image, ratio: float) -> Image.Image:
    """根据传入的比例执行裁剪"""
    img = img.convert("RGBA")
    w, h = img.size
    return img.crop((0, 0, int(w * ratio), h))


def overlay_images(bg: Image.Image, fg: Image.Image) -> Image.Image:
    return Image.alpha_composite(bg.convert("RGBA"), fg.convert("RGBA"))


def grid_2x2(images, gap=0):
    ims = [im.convert("RGBA") for im in images]

    col_w0 = max(ims[0].width, ims[2].width)
    col_w1 = max(ims[1].width, ims[3].width)
    row_h0 = max(ims[0].height, ims[1].height)
    row_h1 = max(ims[2].height, ims[3].height)

    canvas = Image.new(
        "RGBA",
        (col_w0 + gap + col_w1, row_h0 + GRID_GAP_Y + row_h1),
        (255, 255, 255, 0)
    )

    canvas.alpha_composite(ims[0], (0, 0))
    canvas.alpha_composite(ims[1], (col_w0 + gap, 0))
    canvas.alpha_composite(ims[2], (0, row_h0 + GRID_GAP_Y))
    canvas.alpha_composite(ims[3], (col_w0 + gap, row_h0 + GRID_GAP_Y))

    return canvas, col_w0, col_w1, row_h0, row_h1


def append_legend_right(main_img: Image.Image, legend_img: Image.Image):
    main_img = main_img.convert("RGBA")
    legend_img = legend_img.convert("RGBA")

    mw, mh = main_img.size
    lw, lh = legend_img.size

    target_h = int(mh * LEGEND_HEIGHT_RATIO)
    scale = target_h / lh
    target_w = int(lw * scale)

    legend_resized = legend_img.resize((target_w, target_h), Image.LANCZOS)

    canvas = Image.new(
        "RGBA",
        (mw + LEGEND_RIGHT_MARGIN + target_w, max(mh, target_h)),
        (255, 255, 255, 0)
    )

    canvas.alpha_composite(main_img, (0, 0))
    canvas.alpha_composite(
        legend_resized,
        (mw + LEGEND_RIGHT_MARGIN, (mh - target_h) // 2)
    )

    return canvas


def replace_farm_titles(
        img, col_w0, col_w1, row_h0, row_h1, farm_id
):
    draw = ImageDraw.Draw(img)

    try:
        font = ImageFont.truetype("arial.ttf", TITLE_FONT_SIZE)
    except IOError:
        font = ImageFont.load_default()

    # ✅ 与 FREQS 完全一致的顺序
    titles = [
        f"Weekly-Farm{farm_id}",
        f"Monthly-Farm{farm_id}",
        f"Seasonal-Farm{farm_id}",
        f"Annually-Farm{farm_id}",
    ]

    positions = [
        (0, 0),
        (col_w0, 0),
        (0, row_h0 + GRID_GAP_Y),
        (col_w0, row_h0 + GRID_GAP_Y),
    ]

    for (x0, y0), title in zip(positions, titles):
        tx = x0 + TITLE_OFFSET_X
        ty = y0 + TITLE_OFFSET_Y

        bbox = draw.textbbox((0, 0), title, font=font)
        tw = bbox[2] - bbox[0]
        th = bbox[3] - bbox[1]

        draw.rectangle(
            [
                tx - TITLE_PADDING,
                ty - TITLE_PADDING,
                tx + tw + TITLE_PADDING,
                ty + th + TITLE_PADDING,
            ],
            fill=TITLE_BG_COLOR
        )

        draw.text(
            (tx, ty),
            title,
            font=font,
            fill=TITLE_TEXT_COLOR
        )

    return img


# =========================
# 扫描文件
# =========================

pattern = re.compile(
    r"(an|se|mo|wk|true)_(n_I_plot|E_plot|exposure_plot)_farm(\d+)\.png"
)

index = {}
for f in ROOT.glob("*.png"):
    m = pattern.match(f.name)
    if not m:
        continue
    freq, plot_type, farm = m.groups()
    index.setdefault(farm, {}).setdefault(plot_type, {})[freq] = f

# 预加载图例
legend_li = Image.open(ROOT / "li1.png").convert("RGBA")
legend_li1 = Image.open(ROOT / "li1.png").convert("RGBA")

# =========================
# 主流程
# =========================

for farm, farm_data in sorted(index.items(), key=lambda x: int(x[0])):

    for plot_type in PLOT_TYPES:

        if plot_type not in farm_data:
            continue

        merged_imgs = []

        # 确定当前 plot_type 使用的裁剪比例
        current_ratio = CROP_EXPOSURE_RATIO if plot_type == "exposure_plot" else CROP_RIGHT_RATIO

        for freq in FREQS:
            freq_path = farm_data[plot_type].get(freq)
            true_path = farm_data[plot_type].get("true")

            if not freq_path or not true_path:
                break

            img_freq = crop_custom(Image.open(freq_path), current_ratio)
            img_true = crop_custom(Image.open(true_path), current_ratio)
            merged_imgs.append(overlay_images(img_true, img_freq))

        if len(merged_imgs) != 4:
            continue

        final_img, c0, c1, r0, r1 = grid_2x2(merged_imgs, GRID_GAP)

        # 替换 Farm 标题
        final_img = replace_farm_titles(
            final_img, c0, c1, r0, r1, farm
        )

        # 根据类型选择图例
        current_legend = legend_li if plot_type == "exposure_plot" else legend_li1

        final_img = append_legend_right(final_img, current_legend)

        out = OUT_DIR / f"farm{farm}_{plot_type}_merged.png"
        final_img.save(out)
        print(f"[OK] {out.name} (Used ratio: {current_ratio})")
