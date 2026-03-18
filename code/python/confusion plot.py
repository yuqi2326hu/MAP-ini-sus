import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# =========================
# 1. State names and colors
# =========================
state_map = {
    'susceptible': 'S',
    'transient': 'Itr',
    'latent': 'L',
    'low': 'Il',
    'high': 'Ih'
}

full_name_map = {
    'susceptible': 'susceptible (S)',
    'transient': 'transient shedding (Itr)',
    'latent': 'latent infection (L)',
    'low': 'low shedding (Il)',
    'high': 'high shedding (Ih)'
}

color_map = {
    'susceptible': '#4C78A8',
    'transient': '#F58518',
    'latent': '#54A24B',
    'low': '#E45756',
    'high': '#72B7B2'
}

classified_order = ['susceptible', 'transient', 'latent', 'low', 'high']
true_order = ['susceptible', 'transient', 'latent', 'low', 'high']


# =========================================
# 2. Input confusion tables as pandas DataFrame
#    Rows = true states
#    Columns = classified states
# =========================================
annual = pd.DataFrame(
    [
        [10124,    0,    0,    0,   0],
        [    0, 1209,    0,   77,   0],
        [ 2310,    0,  308,    0,   0],
        [    0,  186,    0, 2960, 121],
        [    0,   50,    0, 1212, 101],
    ],
    index=['susceptible', 'transient', 'latent', 'low', 'high'],
    columns=['susceptible', 'transient', 'latent', 'low', 'high']
)

seasonal = pd.DataFrame(
    [
        [25409,    0,    0,    0,   0],
        [    0, 2600,    0,  232,   0],
        [ 4002,    0, 4111,    0,   0],
        [    0,  259,    0, 9279, 118],
        [    0,   24,    0, 2055, 125],
    ],
    index=['susceptible', 'transient', 'latent', 'low', 'high'],
    columns=['susceptible', 'transient', 'latent', 'low', 'high']
)

monthly = pd.DataFrame(
    [
        [70694,    0,   572,     0,   0],
        [    0, 7264,     0,   139,   0],
        [ 2066,    0, 22575,     0,   0],
        [    0,  340,     0, 27726, 859],
        [    0,  154,     0,  4465, 207],
    ],
    index=['susceptible', 'transient', 'latent', 'low', 'high'],
    columns=['susceptible', 'transient', 'latent', 'low', 'high']
)

weekly = pd.DataFrame(
    [
        [267899,     0,   810,      0,    0],
        [     0, 26788,     0,    368,    0],
        [  3878,     0, 91750,      0,    0],
        [     0,   650,     0, 109629, 1384],
        [     0,   353,     0,  14812,  754],
    ],
    index=['susceptible', 'transient', 'latent', 'low', 'high'],
    columns=['susceptible', 'transient', 'latent', 'low', 'high']
)

tables = {
    'Annual testing frequency': annual,
    'Seasonal testing frequency': seasonal,
    'Monthly testing frequency': monthly,
    'Weekly testing frequency': weekly
}


# =========================
# 3. Plot one subplot
# =========================
def plot_one_panel(ax, df, title):
    x = np.arange(len(classified_order))
    x_labels = [state_map[s] for s in classified_order]
    bottom = np.zeros(len(classified_order))

    for true_state in true_order:
        values = df.loc[true_state, classified_order].values
        ax.bar(
            x,
            values,
            bottom=bottom,
            color=color_map[true_state],
            edgecolor='black',
            linewidth=0.6,
            width=0.8,
            label=full_name_map[true_state]
        )
        bottom += values

    ax.set_title(title, fontsize=14, pad=10)
    ax.set_xticks(x)
    ax.set_xticklabels(x_labels, fontsize=11)
    ax.set_xlabel('Classified state', fontsize=12)
    ax.set_ylabel('Number', fontsize=12)
    ax.ticklabel_format(style='plain', axis='y')

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)


# =========================
# 4. Create 2x2 combined figure
# =========================
fig, axes = plt.subplots(2, 2, figsize=(16, 11))
axes = axes.flatten()

for ax, (title, df) in zip(axes, tables.items()):
    plot_one_panel(ax, df, title)

# 只保留一次图例
handles, labels = axes[0].get_legend_handles_labels()
fig.legend(
    handles,
    labels,
    title='True state',
    loc='center left',
    bbox_to_anchor=(0.92, 0.5),
    frameon=True,
    fontsize=11,
    title_fontsize=12
)

# 给总图留出右侧图例空间
plt.tight_layout(rect=[0, 0, 0.88, 1])

# 保存总图
plt.savefig('combined_testing_frequency_confusion_bars.png', dpi=300, bbox_inches='tight')

plt.show()