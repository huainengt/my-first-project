# 1. 高级多组分布与显著性图
# 生成更复杂的模拟数据：模拟4个基因在3种细胞系中的表达
set.seed(456)
complex_exp_data <- expand_grid(
  Cell_Line = c("HeLa", "HEK293", "A549"),
  Gene = paste0("Gene_", LETTERS[1:4]),
  Replicate = 1:15
) %>%
  mutate(
    Expression = rnorm(n(),
                       mean = c(8, 5, 12, 7)[as.numeric(factor(Gene))] * 
                         c(1, 1.3, 0.8)[as.numeric(factor(Cell_Line))],
                       sd = c(1, 1.2, 0.9, 1.1)[as.numeric(factor(Gene))]
    ),
    Treatment = sample(c("DMSO", "Compound_X"), n(), replace = TRUE)
  )

# 绘制带详细统计的高级分布图
p_advanced <- ggplot(complex_exp_data, 
                     aes(x = Cell_Line, y = Expression, fill = Cell_Line)) +
  # 组合多个几何层增加信息密度
  geom_violin(alpha = 0.4, trim = FALSE, width = 0.8, color = NA) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA, 
               position = position_dodge(0.9)) +
  geom_jitter(aes(color = Treatment), width = 0.05, size = 1.2, alpha = 0.6) +
  
  # 分面展示不同基因
  facet_wrap(~ Gene, scales = "free_y", nrow = 1) +
  
  # 使用Nature推荐的色盲友好配色
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C")) +
  scale_color_manual(values = c("#D62728", "#9467BD")) +
  
  # 添加统计比较（所有细胞系间的成对比较）
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(c("HeLa", "HEK293"), c("HeLa", "A549"), c("HEK293", "A549")),
    label = "p.format",  # 显示具体p值而非星号
    label.y = rep(c(22, 24, 26), 4),  # 调整标签位置避免重叠
    size = 3
  ) +
  
  # 标题与标签
  labs(
    title = "Multi-Gene Expression Profiling Across Cell Lines",
    subtitle = "Violin plots show distribution density; boxplots show quartiles\nIndividual points colored by treatment condition",
    x = "Cell Line",
    y = "Expression Level (Log2 Normalized Counts)",
    fill = "Cell Line",
    color = "Treatment"
  ) +
  
  # 高级主题设置
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5,
                                 margin = margin(b = 15)),
    axis.title = element_text(size = 11, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(12, "points"),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

print(p_advanced)

# ==高质量图件输出 ===
library(Cairo)  # 高质量图形设备

# 1. PDF输出（矢量格式，适合出版）
pdf_file <- "Gene_Expression_Plot.pdf"
cairo_pdf(pdf_file, 
          width = 14, 
          height = 8,
          pointsize = 12)
print(p_advanced)
dev.off()
message("✓ PDF已保存: ", pdf_file)

# 2. TIFF输出（高质量位图，适合期刊）
tiff_file <- "Gene_Expression_Plot.tiff"
tiff(tiff_file,
     width = 2800,  # 高分辨率
     height = 1600,
     units = "px",
     res = 300,      # 300 DPI
     compression = "lzw")  # 无损压缩
print(p_advanced)
dev.off()
message("✓ TIFF已保存: ", tiff_file)

# 3. PNG输出（网页使用）
png_file <- "Gene_Expression_Plot.png"
png(png_file,
    width = 2000,
    height = 1200,
    units = "px",
    res = 250,
    type = "cairo")  # 使用Cairo渲染器
print(p_advanced)
dev.off()
message("✓ PNG已保存: ", png_file)

# 4. SVG输出（矢量格式，可编辑）
svg_file <- "Gene_Expression_Plot.svg"
svg(svg_file,
    width = 14,
    height = 8)
print(p_advanced)
dev.off()
message("✓ SVG已保存: ", svg_file)

# 显示图件信息
cat("\n=== 图件输出信息 ===\n")
cat("1. PDF  (矢量图): ", pdf_file, "\n")
cat("   尺寸: 14×8英寸, 适合出版印刷\n\n")
cat("2. TIFF (高质量位图): ", tiff_file, "\n")
cat("   分辨率: 300 DPI, 2800×1600像素\n")
cat("   压缩: LZW无损压缩\n\n")
cat("3. PNG  (网页使用): ", png_file, "\n")
cat("   分辨率: 250 DPI, 2000×1200像素\n\n")
cat("4. SVG  (可编辑矢量): ", svg_file, "\n")
cat("   可用Inkscape或Adobe Illustrator编辑\n")




# 2. 蜂群图优化版 (n≈30/组) ----
library(ggbeeswarm)
set.seed(456)

data_medium <- expand_grid(
  Cell_Line = c("HeLa", "HEK293", "A549"),
  Gene = paste0("Gene_", LETTERS[1:4]),
  Replicate = 1:30
) %>%
  mutate(
    Treatment = sample(c("DMSO", "Compound_X"), n(), replace = TRUE),
    base_mean = c(8, 5, 12, 7)[as.numeric(factor(Gene))],
    cell_factor = c(1.0, 1.3, 0.8)[as.numeric(factor(Cell_Line))],
    treat_effect = ifelse(Treatment == "Compound_X", 
                          c(1.5, 1.0, 2.0, 0.8)[as.numeric(factor(Gene))], 0),
    # 添加亚群效应，增加数据异质性
    subgroup = sample(1:3, n(), replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    subgroup_effect = c(0, 0.7, -0.5)[subgroup],
    Expression = rnorm(n(),
                       mean = base_mean * cell_factor + treat_effect + subgroup_effect,
                       sd = c(1.1, 1.3, 0.9, 1.2)[as.numeric(factor(Gene))])
  )

p2 <- ggplot(data_medium, aes(x = Cell_Line, y = Expression, fill = Cell_Line)) +
  # 半透明小提琴图显示分布
  geom_violin(alpha = 0.25, width = 0.7, trim = FALSE, color = "gray50", 
              linewidth = 0.3, scale = "width") +
  
  # 箱线图 - 窄宽度显示关键统计量
  geom_boxplot(width = 0.12, alpha = 0.8, outlier.shape = NA, 
               color = "gray30", linewidth = 0.4) +
  
  # 蜂群图 - 优化排列避免重叠
  geom_beeswarm(
    aes(color = Treatment, shape = Treatment),
    size = 1.8,
    alpha = 0.75,
    cex = 3.2,           # 控制点间距
    priority = "density", # 按密度排列
    method = "swarm",    # 蜂群排列算法
    corral = "gutter",   # 控制点不超出边界
    corral.width = 0.8,
    dodge.width = 0.7
  ) +
  
  # 添加中位数线
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.5,
    size = 0.5,
    color = "gray20",
    show.legend = FALSE
  ) +
  
  facet_wrap(~ Gene, nrow = 1, scales = "free_y") +
  
  # 配色方案
  scale_fill_manual(values = c("HeLa" = "#59A14F", "HEK293" = "#EDC948", "A549" = "#B07AA1")) +
  scale_color_manual(values = c("DMSO" = "#4E79A7", "Compound_X" = "#E15759")) +
  scale_shape_manual(values = c("DMSO" = 16, "Compound_X" = 18)) +
  
  # 统计检验
  stat_compare_means(
    comparisons = list(c("HeLa", "HEK293"), c("HeLa", "A549"), c("HEK293", "A549")),
    method = "wilcox.test",  # 非参数检验
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    ),
    size = 4,
    tip.length = 0.02,
    bracket.size = 0.5,
    step.increase = 0.15
  ) +
  
  labs(
    title = "蜂群图可视化: 中等样本量的清晰展示",
    subtitle = "geom_beeswarm()确保数据点不重叠，清晰显示分布模式",
    x = "细胞系",
    y = "表达水平 (Log₂)",
    caption = "Wilcoxon检验 | *p<0.05, **p<0.01, ***p<0.001, ****p<0.0001"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
    legend.position = "right",
    legend.box.background = element_rect(color = "gray85", fill = "white"),
    strip.background = element_rect(fill = "#F0F0F0", color = "gray80"),
    panel.grid.major.x = element_blank()
  ) +
  coord_cartesian(clip = "off")

print(p2)
ggsave("方案2_蜂群图.png", p2, width = 16, height = 8, dpi = 300)







# 3. Sina图优化版 (n≈50/组) ----
library(ggforce)
set.seed(789)

data_sina <- expand_grid(
  Cell_Line = c("HeLa", "HEK293", "A549"),
  Gene = paste0("Gene_", LETTERS[1:4]),
  Replicate = 1:50
) %>%
  mutate(
    Treatment = sample(c("DMSO", "Compound_X"), n(), replace = TRUE),
    base_mean = c(8, 5, 12, 7)[as.numeric(factor(Gene))],
    cell_factor = c(1.0, 1.3, 0.8)[as.numeric(factor(Cell_Line))],
    treat_effect = ifelse(Treatment == "Compound_X", 
                          c(2.0, 1.5, 2.8, 1.2)[as.numeric(factor(Gene))], 0),
    # 创建多峰分布
    distribution_mode = sample(1:2, n(), replace = TRUE, prob = c(0.7, 0.3)),
    mode_shift = ifelse(distribution_mode == 2, 2.5, 0),
    Expression = rnorm(n(),
                       mean = base_mean * cell_factor + treat_effect + mode_shift,
                       sd = c(1.2, 1.4, 1.0, 1.3)[as.numeric(factor(Gene))])
  )

p3 <- ggplot(data_sina, aes(x = Cell_Line, y = Expression, fill = Cell_Line)) +
  # Sina图核心 - 显示分布密度
  geom_sina(
    aes(color = Treatment),
    size = 1.5,
    alpha = 0.65,
    scale = "width",      # 按宽度缩放
    maxwidth = 0.75,      # 最大宽度
    bin_limit = 1.2,      # 分箱限制
    method = "density",   # 按密度排列
    jitter_y = FALSE      # 不在Y轴方向抖动
  ) +
  
  # 箱线图叠加
  geom_boxplot(
    width = 0.15,
    alpha = 0.85,
    outlier.shape = NA,
    color = "gray25",
    linewidth = 0.4,
    position = position_dodge(0)
  ) +
  
  # 添加密度曲线轮廓
  geom_violin(
    alpha = 0.1,
    color = NA,
    trim = FALSE,
    scale = "width",
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  
  facet_wrap(~ Gene, nrow = 1, scales = "free_y") +
  
  # 高级配色
  scale_fill_viridis_d(option = "C", alpha = 0.4, begin = 0.2, end = 0.8) +
  scale_color_viridis_d(option = "D", begin = 0.3, end = 0.9) +
  
  # 显著性标记
  stat_compare_means(
    comparisons = list(c("HeLa", "HEK293"), c("HeLa", "A549"), c("HEK293", "A549")),
    method = "t.test",
    label = "p.format",
    size = 3.5,
    tip.length = 0.015,
    label.y = c(22, 24, 26, 28),
    bracket.nudge.y = 0.5
  ) +
  
  # 添加分布峰度指示
  geom_text(data = data_sina %>% 
              group_by(Cell_Line, Gene) %>% 
              summarise(
                skewness = moments::skewness(Expression),
                kurtosis = moments::kurtosis(Expression),
                .groups = "drop"
              ),
            aes(label = sprintf("S:%.2f\nK:%.2f", skewness, kurtosis),
                x = as.numeric(factor(Cell_Line)) + 0.3,
                y = max(data_sina$Expression) * 0.9),
            size = 2.8, color = "gray40", hjust = 0, vjust = 1) +
  
  labs(
    title = "Sina图: 分布密度与个体数据的融合展示",
    subtitle = "结合了小提琴图的密度信息与散点图的个体数据，适合中等偏大样本",
    x = "细胞系",
    y = "表达水平 (Log₂)",
    caption = "S:偏度, K:峰度 | 虚线表示四分位数"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  coord_cartesian(clip = "off")

print(p3)
ggsave("方案3_Sina图.png", p3, width = 16, height = 8, dpi = 300)









# 4. 分半小提琴图优化版 (n≈25/处理组) ----
# 安装并加载gghalves包
if (!require("gghalves")) {
  devtools::install_github("erocoar/gghalves")
}
library(gghalves)

set.seed(101112)
data_split <- expand_grid(
  Cell_Line = c("HeLa", "HEK293", "A549"),
  Gene = paste0("Gene_", LETTERS[1:4]),
  Treatment = c("DMSO", "Compound_X"), # 明确两个处理组
  Replicate = 1:25
) %>%
  mutate(
    base_mean = c(8, 5, 12, 7)[as.numeric(factor(Gene))],
    cell_factor = c(1.0, 1.3, 0.8)[as.numeric(factor(Cell_Line))],
    # 基因特异性处理效应
    treat_effect = case_when(
      Treatment == "Compound_X" & Gene == "Gene_A" ~ 2.5,
      Treatment == "Compound_X" & Gene == "Gene_B" ~ 1.8,
      Treatment == "Compound_X" & Gene == "Gene_C" ~ 3.2,
      Treatment == "Compound_X" & Gene == "Gene_D" ~ 1.5,
      TRUE ~ 0
    ),
    Expression = rnorm(n(),
                       mean = base_mean * cell_factor + treat_effect,
                       sd = c(1.0, 1.2, 0.9, 1.1)[as.numeric(factor(Gene))])
  )

p4 <- ggplot(data_split, aes(x = Cell_Line, y = Expression)) +
  # 分半小提琴图 - 左半边
  geom_half_violin(
    aes(fill = Treatment), 
    side = "l", 
    alpha = 0.7, 
    trim = TRUE,
    scale = "width",
    width = 0.5,
    position = position_nudge(x = -0.18),
    color = "gray40",
    linewidth = 0.3,
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  
  # 分半小提琴图 - 右半边
  geom_half_violin(
    aes(fill = Treatment), 
    side = "r", 
    alpha = 0.7, 
    trim = TRUE,
    scale = "width",
    width = 0.5,
    position = position_nudge(x = 0.18),
    color = "gray40",
    linewidth = 0.3,
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  
  # 箱线图
  geom_boxplot(
    aes(fill = Treatment),
    width = 0.2,
    alpha = 0.9,
    outlier.shape = NA,
    position = position_dodge(0),
    color = "gray30",
    linewidth = 0.4
  ) +
  
  # 中位数点
  stat_summary(
    aes(group = Treatment),
    fun = median,
    geom = "point",
    shape = 21,
    size = 2.5,
    color = "white",
    fill = "gray20",
    position = position_dodge(0),
    show.legend = FALSE
  ) +
  
  # 散点数据
  geom_point(
    aes(color = Treatment, shape = Treatment),
    position = position_jitterdodge(
      jitter.width = 0.08,
      jitter.height = 0,
      dodge.width = 0.7
    ),
    size = 1.5,
    alpha = 0.6,
    stroke = 0.5
  ) +
  
  facet_wrap(~ Gene, nrow = 1, scales = "free_y") +
  
  # 对比色方案
  scale_fill_manual(values = c("DMSO" = alpha("#5D69B1", 0.6), 
                               "Compound_X" = alpha("#E58606", 0.6))) +
  scale_color_manual(values = c("DMSO" = "#5D69B1", "Compound_X" = "#E58606")) +
  scale_shape_manual(values = c("DMSO" = 16, "Compound_X" = 17)) +
  
  # 组内差异统计检验
  stat_compare_means(
    aes(group = Treatment),  # 在每个细胞系内比较两种处理
    method = "t.test",
    label = "p.signif",
    label.y.npc = 0.92,
    size = 4,
    show.legend = FALSE,
    symnum.args = list(
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("***", "**", "*", "ns")
    )
  ) +
  
  # 组间差异统计检验
  stat_compare_means(
    aes(group = Cell_Line),  # 比较不同细胞系
    method = "anova",
    label = "p.format",
    label.y.npc = 0.98,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  labs(
    title = "分半小提琴图: 组内处理效应对比分析",
    subtitle = "左右分半展示不同处理条件下的表达分布，直观比较处理效应",
    x = "细胞系",
    y = "表达水平 (Log₂)",
    fill = "处理条件",
    color = "处理条件",
    shape = "处理条件",
    caption = "左/右: DMSO vs Compound_X | *p<0.05, **p<0.01, ***p<0.001"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(12, "points")
  ) +
  coord_cartesian(clip = "off")

print(p4)
ggsave("方案4_分半小提琴图.png", p4, width = 16, height = 9, dpi = 300)





# 5. 雨云图优化版 (n≈35/组) ----
set.seed(131415)
data_rain <- expand_grid(
  Cell_Line = c("HeLa", "HEK293", "A549"),
  Gene = paste0("Gene_", LETTERS[1:4]),
  Replicate = 1:35
) %>%
  mutate(
    Treatment = sample(c("DMSO", "Compound_X"), n(), replace = TRUE),
    base_mean = c(8, 5, 12, 7)[as.numeric(factor(Gene))],
    cell_factor = c(1.0, 1.3, 0.8)[as.numeric(factor(Cell_Line))],
    treat_effect = ifelse(Treatment == "Compound_X", 
                          c(1.7, 1.3, 2.3, 1.1)[as.numeric(factor(Gene))], 0),
    # 增加异质性
    rand_effect = rnorm(n(), 0, 0.8),
    Expression = rnorm(n(),
                       mean = base_mean * cell_factor + treat_effect + rand_effect,
                       sd = c(1.1, 1.3, 0.95, 1.2)[as.numeric(factor(Gene))])
  )

p5 <- ggplot(data_rain, aes(x = Cell_Line, y = Expression)) +
  # 1. 半边小提琴图 (左侧)
  geom_half_violin(
    aes(fill = Cell_Line),
    side = "l",
    alpha = 0.5,
    trim = FALSE,
    width = 0.7,
    position = position_nudge(x = -0.22),
    color = "gray50",
    linewidth = 0.3,
    scale = "width"
  ) +
  
  # 2. 箱线图 (居中)
  geom_boxplot(
    aes(fill = Cell_Line),
    width = 0.3,
    alpha = 0.85,
    outlier.shape = NA,
    position = position_nudge(x = 0),
    color = "gray30",
    linewidth = 0.4
  ) +
  
  # 3. 散点图 (右侧分布)
  geom_point(
    aes(color = Treatment, shape = Treatment),
    position = position_jitter(
      width = 0.12,
      height = 0
    ),
    size = 1.8,
    alpha = 0.8,
    stroke = 0.6
  ) +
  
  # 4. 均值±标准差误差线
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    width = 0.08,
    color = "gray20",
    linewidth = 0.5,
    position = position_nudge(x = 0.1)
  ) +
  
  # 5. 均值点
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 2,
    fill = "white",
    color = "gray20",
    stroke = 1,
    position = position_nudge(x = 0.1)
  ) +
  
  facet_wrap(~ Gene, nrow = 1, scales = "free_y") +
  
  # 配色
  scale_fill_manual(values = c("HeLa" = "#A0CBE8", "HEK293" = "#FFBE7D", "A549" = "#F1CE63")) +
  scale_color_manual(values = c("DMSO" = "#4E79A7", "Compound_X" = "#E15759")) +
  scale_shape_manual(values = c("DMSO" = 16, "Compound_X" = 17)) +
  
  # 统计检验
  stat_compare_means(
    comparisons = list(c("HeLa", "HEK293"), c("HeLa", "A549"), c("HEK293", "A549")),
    method = "t.test",
    label = "p.format",
    size = 3.2,
    tip.length = 0.02,
    bracket.size = 0.4,
    step.increase = 0.14,
    label.y = c(20, 22, 24)
  ) +
  
  # 添加描述统计
  geom_text(data = data_rain %>% 
              group_by(Cell_Line, Gene) %>% 
              summarise(
                mean_val = mean(Expression),
                sd_val = sd(Expression),
                se_val = sd_val/sqrt(n()),
                .groups = "drop"
              ),
            aes(label = sprintf("μ=%.1f\n±%.2f", mean_val, sd_val),
                y = min(data_rain$Expression) * 0.95,
                x = as.numeric(factor(Cell_Line))),
            size = 2.8, color = "gray40", vjust = 1) +
  
  labs(
    title = "雨云图: 综合数据可视化方案",
    subtitle = "半边小提琴(分布) + 箱线图(统计) + 散点(个体) + 误差线(变异)",
    x = "细胞系",
    y = "表达水平 (Log₂)",
    caption = "误差线: 均值±标准差 | μ: 平均值, ±: 标准差"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.8, size = 11),
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(clip = "off", ylim = c(min(data_rain$Expression) * 1.1, NA))

print(p5)
ggsave("方案5_雨云图.png", p5, width = 16, height = 9, dpi = 300)






# =====================================================================
# 方案6：符合《Nature》发表要求的大样本基因表达可视化方案
# 设计原则：简洁、清晰、高信息密度、严格遵守技术规范
# =====================================================================

# ---------------------------
# 1. 加载与安装必要R包
# ---------------------------
required_packages <- c("ggplot2", "ggpubr", "dplyr", "tidyr", "RColorBrewer")
new_pkgs <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# ---------------------------
# 2. 生成模拟的大样本基因表达数据
# ---------------------------
set.seed(123456) # 确保结果可重复
n_per_group <- 150 # 每组样本量，可调整

sim_data <- expand.grid(
  Cell_Line = factor(c("HeLa", "HEK293", "A549"), levels = c("HeLa", "HEK293", "A549")),
  Gene = factor(paste0("Gene_", LETTERS[1:4]), levels = paste0("Gene_", LETTERS[1:4])),
  Rep = 1:n_per_group
) %>%
  mutate(
    # 模拟表达量：基准值 + 细胞系效应 + 基因-细胞系互作 + 随机误差
    base_value = case_when(
      Gene == "Gene_A" ~ 10,
      Gene == "Gene_B" ~ 7,
      Gene == "Gene_C" ~ 14,
      Gene == "Gene_D" ~ 9
    ),
    cell_effect = case_when(
      Cell_Line == "HeLa" ~ 1.0,
      Cell_Line == "HEK293" ~ 1.4,
      Cell_Line == "A549" ~ 0.7
    ),
    interaction_noise = rnorm(n(), mean = 0, sd = 0.8), # 交互作用噪声
    Expression = base_value * cell_effect + interaction_noise + rnorm(n(), sd = 1.5)
  )

# ---------------------------
# 3. 定义《Nature》风格主题
# 核心：无衬线字体、纯净背景、精确的线条控制[citation:2]
# ---------------------------
theme_nature <- function(base_size = 7) { # Nature正文常用较小字号
  theme_minimal(base_family = "Times New Roman") %+replace% # 使用Times New Roman[citation:5]
    theme(
      # 文本样式
      text = element_text(size = base_size, colour = "black"),
      axis.title = element_text(size = base_size, face = "plain"),
      axis.text = element_text(size = base_size - 1, colour = "black"),
      strip.text = element_text(size = base_size, face = "bold", margin = margin(b=3)),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5, margin = margin(b=4)),
      plot.caption = element_text(size = base_size - 2, colour = "grey40", hjust = 0),
      
      # 轴线与刻度 (线条宽度严格控制在0.25-1 pt之间)[citation:1]
      axis.line = element_line(colour = "black", linewidth = 0.18), # ~0.5 pt
      axis.ticks = element_line(colour = "black", linewidth = 0.18),
      panel.grid.major = element_blank(), # 清除网格，保持简洁
      panel.grid.minor = element_blank(),
      
      # 图例与边距
      legend.title = element_text(size = base_size - 1),
      legend.text = element_text(size = base_size - 1),
      legend.position = "top", # 图例置于顶部，节省空间[citation:2]
      legend.key.size = unit(3, "mm"),
      legend.spacing.x = unit(1, "mm"),
      
      plot.margin = margin(4, 4, 4, 4, "mm"), # 紧凑边距
      panel.spacing = unit(2, "mm"), # 分面板间距
      
      # 分面面板
      strip.background = element_rect(fill = "grey95", colour = "grey80", linewidth = 0.18),
      panel.border = element_blank()
    )
}

# ---------------------------
# 4. 创建符合规范的雨云图 (Raincloud Plot)
# 组合：半小提琴图(分布) + 箱线图(统计) + 散点(个体)
# ---------------------------
# 4.1 准备分面标签 (更专业)
facet_labels <- c("Gene_A" = "Gene A", "Gene_B" = "Gene B", 
                  "Gene_C" = "Gene C", "Gene_D" = "Gene D")

# 4.2 构建图形图层
p_nature <- ggplot(sim_data, aes(x = Cell_Line, y = Expression, fill = Cell_Line)) +
  
  # A. 分布图层：半小提琴图
  ggforce::geom_sina(
    aes(colour = Cell_Line), # 点与填充色一致但可调透明度
    scale = "width", 
    maxwidth = 0.7, 
    alpha = 0.15, # 极低的点透明度，避免大样本重叠混乱
    size = 0.2, # 极小的点，突出分布而非个体
    show.legend = FALSE # 颜色已在填充图例中体现
  ) +
  
  # B. 统计摘要图层：箱线图
  geom_boxplot(
    width = 0.2,
    alpha = 0.8,
    outlier.shape = NA, # 不显示异常值，由散点图层展示
    colour = "black",
    linewidth = 0.18, # 0.5 pt
    fatten = 1.2 # 中位线稍粗
  ) +
  
  # C. 添加中位数点 (更清晰)
  stat_summary(
    fun = median,
    geom = "point",
    shape = 21,
    size = 1.2,
    colour = "white",
    fill = "black",
    show.legend = FALSE
  ) +
  
  # 分面
  facet_wrap(~Gene, nrow = 1, labeller = labeller(Gene = facet_labels)) +
  
  # ---------------------------
# 5. 配色与标签
# 原则：专业、对比度高、色盲友好[citation:4]
# ---------------------------
scale_fill_manual(
  name = "Cell Line", # 图例标题
  values = c("#1F77B4", "#FF7F0E", "#2CA02C"), # Tableau色盲友好配色
  guide = guide_legend(nrow = 1, title.position = "top")
) +
  scale_colour_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C"), guide = "none") +
  
  # 坐标轴与标题
  labs(
    x = NULL, # 坐标轴名称可通过图例或分面理解，此处省略使图更简洁
    y = "Expression Level (Log₂ Normalized Counts)",
    title = NULL # 《Nature》图表通常不自带标题，标题在文中或图注中
  ) +
  
  # 应用《Nature》主题
  theme_nature(base_size = 7) +
  
  # 微调：确保图形元素不重叠
  coord_cartesian(clip = "off")

# ---------------------------
# 6. 添加统计显著性标注 (按需)
# 原则：清晰、不喧宾夺主
# ---------------------------
# 计算并添加成对比较的p值（使用更保守的检验方法，如Wilcoxon）
p_nature <- p_nature + 
  stat_compare_means(
    comparisons = list(c("HeLa", "HEK293"), c("HeLa", "A549"), c("HEK293", "A549")),
    method = "wilcox.test",
    label = "p.format", # 显示具体p值，而非星号，更精确[citation:2]
    size = 1.8, # 小字号
    bracket.size = 0.18, # 0.5 pt
    tip.length = 0.01,
    step.increase = 0.08,
    vjust = 0.5,
    hide.ns = FALSE # 显示不显著的结果
  )

# ---------------------------
# 7. 按《Nature》规范导出图像
# 关键：格式、尺寸、分辨率[citation:1][citation:8]
# ---------------------------
# 7.1 设置输出尺寸 (单位：mm)
final_width_mm <- 180   # 《Nature》双栏最大宽度[citation:1]
final_height_mm <- 120  # 根据内容调整高度
dpi <- 600              # 组合图要求≥600 DPI[citation:8]

# 单位转换
mm_to_in <- function(mm) mm / 25.4
width_in <- mm_to_in(final_width_mm)
height_in <- mm_to_in(final_height_mm)

# 7.2 导出为TIFF格式 (位图，通用)
ggsave(
  filename = "Figure6_GeneExpression_NatureStyle.tiff",
  plot = p_nature,
  device = "tiff",
  width = width_in,
  height = height_in,
  units = "in",
  dpi = dpi,
  compression = "lzw", # 无损压缩，减小文件体积
  bg = "white" # 确保背景为纯白
)

# 7.3 导出为EPS格式 (矢量图，印刷首选)[citation:8]
ggsave(
  filename = "Figure6_GeneExpression_NatureStyle.eps",
  plot = p_nature,
  device = cairo_ps, # 使用Cairo设备以嵌入字体
  width = width_in,
  height = height_in,
  units = "in",
  bg = "white"
)

cat("==============================================\n")
cat("✅ 符合《Nature》规范的方案6图形已生成并保存。\n")
cat("   文件1: Figure6_GeneExpression_NatureStyle.tiff (600 DPI TIFF)\n")
cat("   文件2: Figure6_GeneExpression_NatureStyle.eps  (矢量 EPS)\n")
cat(paste0("   尺寸: ", final_width_mm, " mm × ", final_height_mm, " mm\n"))
cat("==============================================\n")

# 7.4 在R中预览图形 (可选)
print(p_nature)