"""Generate thumbnail images for the AWS RDS Benchmarking project card.

Outputs two 1500x1000 PNGs (3:2 aspect, per DESIGN_NOTES.md):
  RDSBenchmarking.png            - Architecture diagram (project card thumbnail)
  RDSBenchmarking_scorecard.png  - Results scorecard (embedded inside the page)
"""

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

# Palette
AWS_NAVY = "#232F3E"
AWS_ORANGE = "#FF9900"
AWS_DB_BLUE = "#4054A6"  # muted royal/database blue used across both diagrams
LIGHT_BG = "#FAFAFA"
TEXT_DARK = "#1A1A1A"
DARK_GREY = "#555555"
WIN_GREEN = "#0E8A3E"
SUB_LIGHT = "#D8DCEC"  # light label on AWS_DB_BLUE
SUB_NAVY = "#C7CDD8"   # light label on AWS_NAVY

FIG_W, FIG_H = 15, 10  # dpi=100 -> 1500x1000

# Resolve assets/img/ relative to this script's location, so the script keeps
# working if the repo is moved or cloned to a new machine.
OUT_DIR = str(Path(__file__).resolve().parent.parent / "assets" / "img")


def rounded_box(ax, x, y, w, h, fc, radius=0.04):
    ax.add_patch(
        FancyBboxPatch(
            (x, y), w, h,
            boxstyle=f"round,pad=0,rounding_size={radius}",
            linewidth=0, edgecolor="none", facecolor=fc,
        )
    )


def make_architecture():
    """Bigger boxes, bigger text, tighter margins."""
    fig, ax = plt.subplots(figsize=(FIG_W, FIG_H), dpi=100)
    ax.set_xlim(0, 15)
    ax.set_ylim(0, 10)
    ax.set_facecolor(LIGHT_BG)
    fig.patch.set_facecolor(LIGHT_BG)
    ax.axis("off")

    # EC2 driver - widened and pulled up
    rounded_box(ax, 4.6, 7.5, 5.8, 2.2, AWS_NAVY, radius=0.18)
    ax.text(7.5, 8.95, "EC2 t2.micro", ha="center", va="center",
            fontsize=30, color="white", fontweight="bold")
    ax.text(7.5, 8.05, "HammerDB 4.12  ·  TPC-C", ha="center", va="center",
            fontsize=23, color=SUB_NAVY)

    # CloudWatch callout - to the right of EC2, same vertical band
    rounded_box(ax, 11.1, 7.8, 3.6, 1.7, AWS_ORANGE, radius=0.15)
    ax.text(12.9, 8.85, "CloudWatch", ha="center", va="center",
            fontsize=22, color=AWS_NAVY, fontweight="bold")
    ax.text(12.9, 8.2, "CPU · IOPS · Memory", ha="center", va="center",
            fontsize=17, color=AWS_NAVY)
    # Dotted line to EC2
    ax.plot([11.1, 10.4], [8.65, 8.65], linestyle=":", color=AWS_ORANGE, linewidth=4)

    # PostgreSQL RDS - bigger and pulled lower-left
    rounded_box(ax, 0.5, 1.6, 6.4, 3.4, AWS_DB_BLUE, radius=0.18)
    ax.text(3.7, 4.35, "RDS", ha="center", va="center",
            fontsize=24, color=SUB_LIGHT, fontweight="bold")
    ax.text(3.7, 3.35, "PostgreSQL 16.4", ha="center", va="center",
            fontsize=34, color="white", fontweight="bold")
    ax.text(3.7, 2.35, "db.t4g.micro  ·  gp3 20 GiB", ha="center", va="center",
            fontsize=19, color=SUB_LIGHT)

    # MySQL RDS - bigger and pulled lower-right
    rounded_box(ax, 8.1, 1.6, 6.4, 3.4, AWS_DB_BLUE, radius=0.18)
    ax.text(11.3, 4.35, "RDS", ha="center", va="center",
            fontsize=24, color=SUB_LIGHT, fontweight="bold")
    ax.text(11.3, 3.35, "MySQL 8.0.40", ha="center", va="center",
            fontsize=34, color="white", fontweight="bold")
    ax.text(11.3, 2.35, "db.t4g.micro  ·  gp3 20 GiB", ha="center", va="center",
            fontsize=19, color=SUB_LIGHT)

    # Arrows from EC2 down to each RDS - thicker
    arrow_kw = dict(arrowstyle="-|>,head_length=16,head_width=11",
                    color=AWS_NAVY, linewidth=4.5, mutation_scale=1)
    ax.add_patch(FancyArrowPatch((5.8, 7.5), (3.7, 5.0), **arrow_kw))
    ax.add_patch(FancyArrowPatch((9.2, 7.5), (11.3, 5.0), **arrow_kw))

    # Footer
    ax.text(7.5, 0.55,
            "Benchmark driver on EC2 · two RDS engines · identical configuration",
            ha="center", va="center", fontsize=19, color=DARK_GREY, style="italic")

    plt.savefig(f"{OUT_DIR}/RDSBenchmarking.png", dpi=100,
                bbox_inches="tight", facecolor=LIGHT_BG)
    plt.close(fig)


def make_scorecard():
    """Side-by-side metric comparison, matched to architecture blue + darker labels."""
    fig, ax = plt.subplots(figsize=(FIG_W, FIG_H), dpi=100)
    ax.set_xlim(0, 15)
    ax.set_ylim(0, 10)
    ax.set_facecolor(LIGHT_BG)
    fig.patch.set_facecolor(LIGHT_BG)
    ax.axis("off")

    # Title strip
    ax.text(7.5, 9.1, "AWS RDS Benchmark Results", ha="center", va="center",
            fontsize=26, color=AWS_NAVY, fontweight="bold")
    ax.text(7.5, 8.45, "HammerDB TPC-C  ·  10 virtual users  ·  5 min",
            ha="center", va="center", fontsize=15, color=DARK_GREY)

    # Column header cards - both AWS_DB_BLUE to match the thumbnail
    rounded_box(ax, 1.0, 6.4, 6.2, 1.4, AWS_DB_BLUE, radius=0.12)
    ax.text(4.1, 7.1, "PostgreSQL 16.4", ha="center", va="center",
            fontsize=22, color="white", fontweight="bold")

    rounded_box(ax, 7.8, 6.4, 6.2, 1.4, AWS_DB_BLUE, radius=0.12)
    ax.text(10.9, 7.1, "MySQL 8.0.40", ha="center", va="center",
            fontsize=22, color="white", fontweight="bold")

    # Metric rows: (label, postgres_value, mysql_value, winner)
    metrics = [
        ("Transactions per minute", "888", "4,058", "mysql"),
        ("New-order latency (ms)", "448.5", "179.6", "mysql"),
        ("Total transactions", "32,176", "60,928", "mysql"),
    ]

    row_top = 5.7
    row_h = 1.55
    for i, (label, pg, my, winner) in enumerate(metrics):
        y = row_top - i * row_h
        bg = "#FFFFFF" if i % 2 == 0 else "#ECECEC"
        rounded_box(ax, 1.0, y - 1.1, 13.0, 1.3, bg, radius=0.05)
        ax.text(7.5, y - 0.15, label, ha="center", va="center",
                fontsize=14, color=DARK_GREY)
        pg_color = WIN_GREEN if winner == "postgres" else TEXT_DARK
        my_color = WIN_GREEN if winner == "mysql" else TEXT_DARK
        ax.text(4.1, y - 0.6, pg, ha="center", va="center",
                fontsize=30, color=pg_color, fontweight="bold")
        ax.text(10.9, y - 0.6, my, ha="center", va="center",
                fontsize=30, color=my_color, fontweight="bold")

    # Footer
    ax.text(7.5, 0.4,
            "Identical RDS configuration  ·  db.t4g.micro  ·  gp3 20 GiB",
            ha="center", va="center", fontsize=13, color=DARK_GREY, style="italic")

    plt.savefig(f"{OUT_DIR}/RDSBenchmarking_scorecard.png", dpi=100,
                bbox_inches="tight", facecolor=LIGHT_BG)
    plt.close(fig)


if __name__ == "__main__":
    make_architecture()
    make_scorecard()
    print("Wrote RDSBenchmarking.png and RDSBenchmarking_scorecard.png")
