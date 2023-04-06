###############################################################################
### BAYES THEORY EXAMPLE 
# https://www.youtube.com/watch?v=NIqeFYUhSzU&t=254s
###############################################################################
library(tidyverse)
library(ggplot2)

## PRIOR (Hypothesis, previous assumptions, etc.)
# Disease ZOMBIE affects 10% of people where you live
x = 1:10
y = 1:10
df = data.frame(expand.grid(x, y))
colnames(df) = c("x", "y")

df.hypoethesis = df %>%
  mutate(infected = case_when(
    y == 1 ~ 'Yes',
    TRUE ~ 'No'
  ))

ggplot() + 
  geom_point(data = df.hypoethesis, aes(x = x, y = y, color = infected), size = 10) +
  scale_color_manual(values = c('#00BFC4','#F8766D')) + 
  geom_hline(yintercept = 1.5, linetype = "dashed") +
  scale_x_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  theme_classic() +
  ggtitle("Bayes Zombie Example") +
  guides(color = guide_legend(title = "Infected")) +
  theme(axis.title.x = element_blank()
        , axis.title.y =  element_blank()
        , plot.title = element_text(hjust = 0.5)
  )

## LIKELIHOOD/DATA (evidence, new information, etc.)
# You are bitten and test positive for being INFECTED
# This test is accurate 80% of the time.
df.evidence = df %>%
  mutate(test = case_when(
    x > 8 ~ '1',
    TRUE ~ '0'
  ))
ggplot() + 
  geom_point(data = df.evidence, aes(x = x, y = y, color = test), size = 10) +
  scale_color_manual(values = c('#C77CFF','#7CAE00')) + 
  geom_vline(xintercept = 8.5, linetype = "dashed") +
  scale_x_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  theme_classic() +
  ggtitle("Bayes Zombie Example") +
  guides(color = guide_legend(title = "Test")) +
  theme(axis.title.x = element_blank()
        , axis.title.y =  element_blank()
        , plot.title = element_text(hjust = 0.5)
  )


## POSTERIOR (Update prior information based on data)
# What is the probability you are infected, given you test positive?
# Update prior based on evidence 
df.update = df %>%
  mutate(update = case_when(
    x < 9 & y == 1 ~ 'TP',
    x >=9 & y == 1 ~ 'FN',
    x < 9 & y > 1 ~ 'TN',
    x >=9 & y > 1 ~ 'FP'
  ))

ggplot() + 
  geom_point(data = df.update, aes(x = x, y = y, color = update), size = 10) +
  geom_vline(xintercept = 8.5, linetype = "dashed") +
  geom_hline(yintercept = 1.5, linetype = "dashed") +
  scale_color_manual(values = c('#C77CFF','#7CAE00','#00BFC4','#F8766D')) + 
  scale_x_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  theme_classic() +
  ggtitle("Bayes Zombie Example") +
  guides(color = guide_legend(title = "Outcome")) +
  theme(axis.title.x = element_blank()
        , axis.title.y =  element_blank()
        , plot.title = element_text(hjust = 0.5)
        )

ggplot() + 
  geom_point(data = df.update, aes(x = x, y = y, color = update), size = 10) +
  geom_vline(xintercept = 8.5, linetype = "dashed") +
  geom_hline(yintercept = 1.5, linetype = "dashed") +
  scale_color_manual(values = c('gray','#7CAE00','gray','#F8766D')) + 
  scale_x_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  theme_classic() +
  ggtitle("Bayes Zombie Example") +
  guides(color = guide_legend(title = "Credible")) +
  theme(axis.title.x = element_blank()
        , axis.title.y =  element_blank()
        , plot.title = element_text(hjust = 0.5)
  )


posterior = 8 / (8 + 18)
print(posterior)

