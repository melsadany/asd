################################################################################
#                               basic re-do analysis                           #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/asd/PS-VC-disc"
setwd(project.dir)
################################################################################
################################################################################
# read data
norm <- readxl::read_xlsx("data/raw/submission_1/tables/supplemental_tables_2e.xlsx", 
                          sheet = 1, skip = 1)

################################################################################
# compare discrepency based on autism status
p1 <- norm %>% 
  select(asd, vc_index, processing_speed_index, fsiq_index) %>%
  drop_na() %>%
  mutate(VC_PS = vc_index - processing_speed_index) %>%
  ggplot(aes(x=asd, y = VC_PS, fill = asd)) +
  geom_violin(show.legend = F) + geom_boxplot(width = 0.2, fill = "white") +
  ggpubr::stat_compare_means(color = six.colors[4]) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = six.colors[c(3,1)]) +
  labs(y = "VC index - PS index")
norm %>%
  select(asd) %>%
  ggplot(aes(x=asd)) +
  geom_bar()
p2 <- norm %>% 
  select(asd, vc_index, processing_speed_index, fsiq_index) %>%
  drop_na() %>%
  ggplot(aes(x=processing_speed_index, y = vc_index, color = asd)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", show.legend = F) +
  ggpubr::stat_cor(show.legend = F)+
  scale_color_manual(values = six.colors[c(3,1)])
p2
tt <- norm %>% 
  select(asd, vc_index, processing_speed_index, fsiq_index) %>%
  drop_na() %>%
  mutate(PS_VC = vc_index - processing_speed_index,
         vc_g = ifelse(vc_index > median(vc_index), "H_VC", "L_VC"),
         ps_g = ifelse(processing_speed_index > median(processing_speed_index), "H_PS", "L_PS"),
         disc = ifelse(PS_VC > 0 , "VC>PS", "PS>=VC")) 
t <- fisher.test(table(tt$disc, tt$asd))
p3 <- tt %>%
  group_by(asd) %>%
  ggplot(aes(x=asd, fill = disc))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = six.colors) +
  labs(subtitle = paste0("Fisher's Exact test: ", "\n",
                         "   OR: ", round(t$estimate, 4),"\n",
                         "   pval: ", round(t$p.value, 4), "\n",
                         "   95% CI: ", round(t$conf.int[1],3), " : ", round(t$conf.int[2],3)))
patchwork::wrap_plots(p1,p2,p3, nrow = 1)
ggsave("figs/B-BC-additional-figs.png", bg = "white",
       width = 9, height = 5, units = "in", dpi = 360)
################################################################################
# check age, and sex correlations to the discrepency
sex.d <- norm %>%
  select(vc_index, processing_speed_index, sex) %>%
  mutate(VC_PS = vc_index - processing_speed_index) %>%
  ggplot(aes(x=sex, y = VC_PS, fill = sex))+
  geom_violin(show.legend = F) + geom_boxplot(width = 0.2, fill = "white") +
  ggpubr::stat_compare_means(color = six.colors[4]) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = boxplot.colors) +
  labs(y = "VC index - PS index")
age.d <- norm %>%
  select(vc_index, processing_speed_index, age, sex) %>%
  mutate(VC_PS = vc_index - processing_speed_index,
         age = as.numeric(age)) %>%
  drop_na() %>%
  # filter(age < 20) %>%
  ggplot(aes(x=age, y = VC_PS)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", show.legend = F, color =six.colors[4]) +
  labs(y = "VC index - PS index")
patchwork::wrap_plots(sex.d, age.d)  
ggsave("figs/B-BC-age-sex-w-discrepency.png", bg = "white",
       width = 6, height = 5, units = "in", dpi = 360)
# lm
cc <- norm %>%
  select(vc_index, processing_speed_index, age, sex) %>%
  mutate(VC_PS = vc_index - processing_speed_index,
         age = as.numeric(age))
df2 <- rbind(jtools::summ(lm(VC_PS ~ sex, data = cc), confint = T, pval = T)$coeftable,
             jtools::summ(lm(VC_PS ~ age, data = cc), confint = T, pval = T)$coeftable) %>%
  as.data.frame() %>%
  rownames_to_column("variable")
write_csv(df2, "data/derivatives/lm-summ-age-or-sex-predicting-VC-PS.csv")
################################################################################
# binnig by IQ group and showing count of how many with VC>PS
# >70, 70-119, >=120
kk <- norm %>%
  select(fsiq_index, vc_index, processing_speed_index) %>%
  mutate(VC_PS = vc_index - processing_speed_index,
         IQ_bin = case_when(fsiq_index >= 0.58 ~ "High IQ",
                            fsiq_index < 0.58 & fsiq_index > -0.57 ~ "Average IQ",
                            fsiq_index < -0.57 ~ "Low IQ"),
         IQ_bin = factor(IQ_bin, levels = c("High IQ", "Average IQ", "Low IQ")))
p5 <- kk %>%
  ggplot(aes(x = IQ_bin, y = VC_PS, fill = IQ_bin)) +
  geom_violin(show.legend = F) + geom_boxplot(width = 0.2, fill = "white") +
  ggpubr::stat_compare_means(color = six.colors[4]) +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = six.colors[c(4:6)]) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  labs(y = "VC index - PS index",
       caption = paste0("n(samples): ", nrow(kk), "\n",
                        "categorizing based on quantiles", "\n",
                        "    n(samples with High IQ): ", sum(kk$IQ_bin=="High IQ"), "\n",
                        "    n(samples with Average IQ): ", sum(kk$IQ_bin=="Average IQ"), "\n",
                        "    n(samples with Low IQ): ", sum(kk$IQ_bin=="Low IQ")))
p5
p6 <- norm %>%
  select(fsiq_index, vc_index, processing_speed_index) %>%
  mutate(VC_PS = vc_index - processing_speed_index,
         IQ_bin = case_when(fsiq_index >= 1.3 ~ "High IQ",
                            fsiq_index < 1.3 & fsiq_index > -2 ~ "Average IQ",
                            fsiq_index < -2 ~ "Low IQ"),
         IQ_bin = factor(IQ_bin, levels = c("High IQ", "Average IQ", "Low IQ"))) %>%
  ggplot(aes(x = fsiq_index, y = VC_PS)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", show.legend = F, color = six.colors[4]) +
  # facet_wrap(~IQ_bin, scales = "free") +
  ggpubr::stat_cor(show.legend = F, method = "pearson", color = six.colors[4])+
  labs(y = "VC index - PS index")
patchwork::wrap_plots(p5,p6)
ggsave("figs/B-BC-IQ-w-discrepency.png", bg = "white",
       width = 7, height = 5, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
################################################################################
# ABCD section
nih <- read_delim("~/LSS/jmichaelson-sdata/ABCD/abcd_release_2_0/data/cognitive/abcd_tbss01.txt")
asd <- read_csv("~/LSS/jmichaelson-sdata/ABCD/abcd_release_5_0/core/abcd-general/abcd_p_screen.csv") %>%
  select(1,2,asd = scrn_asd) %>%
  mutate(asd = ifelse(asd == 1, T, F))
demo <- read_csv("~/LSS/jmichaelson-wdata/msmuhammad/data/ABCD/abcd5/age-sex-by-eventname.csv") %>%
  filter(grepl("baseline", eventname)) %>%
  select(-eventname) %>%
  left_join(asd %>% select(IID = src_subject_id, asd_dx = asd))
################################################################################
# clean nih and corrected
nih.clean <- nih %>%
  select(IID = src_subject_id, sex, age = interview_age, eventname, 
         PV = nihtbx_picvocab_uncorrected, pattern = nihtbx_pattern_uncorrected, 
         tot = nihtbx_totalcomp_uncorrected, reading = nihtbx_reading_uncorrected) %>%
  mutate_at(.vars = vars(PV, pattern, reading, tot, age), .funs = function(x) as.numeric(x)) %>%
  filter(grepl("baseline", eventname)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(age, PV, pattern, reading, tot), 
            .funs = function(x) scale(x, scale = T, center = T)[,1])
nih.corrected <- nih.clean %>%
  mutate_at(.vars = vars(PV, pattern, reading, tot), .funs = function(x) {
    residuals(lm(x ~ age + sex,
                 data = nih.clean %>%
                   mutate(x=as.numeric(x))))
  }) %>%
  mutate(VC_PS = (0.5*PV+0.5*reading)-pattern) %>%
  filter(abs(VC_PS)<10) %>%
  left_join(demo %>% select(IID, asd_dx))
################################################################################
# plot correlation with asd_dx
p7 <- nih.corrected %>%
  drop_na(asd_dx) %>%
  ggplot(aes(x=asd_dx, y = VC_PS, fill = asd_dx))+
  geom_violin(show.legend = F) + geom_boxplot(width = 0.2, fill = "white") +
  ggpubr::stat_compare_means(color = six.colors[4]) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = six.colors[c(3,1)]) +
  labs(y = "(PV + reading)/2 - pattern")
p8 <- nih.corrected %>%
  drop_na(asd_dx) %>%
  ggplot(aes(x=tot, y = VC_PS, color = asd_dx))+
  geom_point(alpha = .5) +
  # geom_smooth(method = "lm", show.legend = F) +
  ggpubr::stat_cor(show.legend = F)+
  scale_color_manual(values = six.colors[c(3,1)]) +
  labs(y = "(PV + reading)/2 - pattern")
p8
p9 <- nih.corrected %>%
  drop_na(asd_dx) %>%
  ggplot(aes(x=asd_dx, fill = sex))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = six.colors[c(3,1)])
p10 <- nih.corrected %>%
  drop_na(asd_dx) %>%
  mutate(VC = 0.5*PV + 0.5*reading,
         PS = pattern) %>%
  ggplot(aes(x=VC, y = PS, color = asd_dx))+
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", show.legend = F) +
  ggpubr::stat_cor(show.legend = F)+
  scale_color_manual(values = six.colors[c(3,1)])
p10
patchwork::wrap_plots(p7,p8,p9,p10, nrow = 1)
ggsave("figs/ABCD-additional-figs.png", bg = "white",
       width = 12, height = 5, units = "in", dpi = 360)
################################################################################


################################################################################
