#https://www.statology.org/import-excel-into-r/#:~:text=The%20easiest%20way%20to%20import,function%20from%20the%20readxl%20package.&text=where%3A,sheet%3A%20The%20sheet%20to%20read.
#install and load readxl package
install.packages('readxl')

library(readxl)

class1_dfcclass1_df <- read_excel('C:\\Users\\ameya\\Documents\\IUB\\Introduction to Statistics\\Project\\S520_Project_Data.xlsx', sheet = 'Class 1')
class2_df <- read_excel('C:\\Users\\ameya\\Documents\\IUB\\Introduction to Statistics\\Project\\S520_Project_Data.xlsx', sheet = 'Class 2')

midterm_exam_score = class1_df$`Midterm Exam Score`
midterm_exam_total_marks = 51
midterm_exam_percentage = midterm_exam_score * 100 / midterm_exam_total_marks
midterm_exam_percentage

#https://www.statology.org/r-add-a-column-to-dataframe/
class1_df$'Midterm Exam Score (percentage)' <- midterm_exam_percentage

#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
boxplot(class1_df$'Midterm Exam Score (percentage)',
        class2_df$'Midterm Exam Score (percentage)',
        names = c("Class 1","Class 2"),
        col = c("orchid2", "purple2"),
        outcol = c("orchid2", "purple2"),
        ylab = 'Percentage', main = 'Boxplot of Class 1 Vs Class 2 Performance in Midterm Exam')





library(readxl)

class1_df <- read_excel('C:\\Users\\ameya\\Documents\\IUB\\Introduction to Statistics\\Project\\S520_Project_Data.xlsx', sheet = 'Class 1')
class2_df <- read_excel('C:\\Users\\ameya\\Documents\\IUB\\Introduction to Statistics\\Project\\S520_Project_Data.xlsx', sheet = 'Class 2')

midterm_exam_score = class1_df$`Midterm Exam Score`
midterm_exam_total_marks = 51
midterm_exam_percentage = midterm_exam_score * 100 / midterm_exam_total_marks
midterm_exam_percentage

#https://www.statology.org/r-add-a-column-to-dataframe/
class1_df$'Midterm Exam Score (percentage)' <- midterm_exam_percentage

female_score = subset(class1_df, Sex == "Female")$'Problem Sets Score (percentage)'
male_score = subset(class1_df, Sex == "Male")$'Problem Sets Score (percentage)'

boxplot(female_score,
        male_score,
        names = c("Female","Male"),
        col = c("yellowgreen", "tan1"),
        outcol = c("yellowgreen", "tan1"),
        ylab = 'Percentage', main = 'Boxplot of Class 1 Female Vs Male Performance in Problem Sets')

sd(female_score)
sd(male_score)

qqnorm(female_score, main = "Normal Q-Q Plot of Problem Set Scores of Female Students")
qqnorm(male_score, main = "Normal Q-Q Plot of Problem Set Scores of Male Students")

qqnorm(log(female_score), main = "Normal Q-Q Plot of Log of Problem Set Scores of Female Students")
qqnorm(log(male_score), main = "Normal Q-Q Plot of Log of Problem Set Scores of Male Students")

qqnorm(sqrt(female_score), main = "Normal Q-Q Plot of Problem Set Scores of Female Students")
qqnorm(sqrt(male_score), main = "Normal Q-Q Plot of Problem Set Scores of Male Students")

mean_male_score = mean(male_score)
mean_male_score
mean_female_score = mean(female_score)
mean_female_score
mean_problem_set_score = mean(class1_df$`Problem Sets Score (percentage)`)
mean_problem_set_score

SST = sum((class1_df$`Problem Sets Score (percentage)` - mean_problem_set_score) ^ 2)
SST

N = length(class1_df$`Problem Sets Score (percentage)`)
N
n_female = length(female_score)
n_female
n_male = length(male_score)
n_male
total_df = N - 1
total_df
between_df = 1
between_df
within_df = N -2
within_df

SSB = (n_female * (mean_female_score - mean_problem_set_score) ^ 2) + (n_male * (mean_male_score - mean_problem_set_score) ^ 2)
SSB
between_mean_square = SSB / between_df
between_mean_square

SSW = sum((female_score - mean_female_score) ^ 2) + sum((male_score - mean_male_score) ^ 2)
SSW
within_mean_square = SSW / within_df
within_mean_square


SST
SSB + SSW
between_mean_square
within_mean_square

F = between_mean_square / within_mean_square
F
p = 1 - pf(F, df1 = between_df, df2 = within_df)
p

group = factor(c(rep(1,length(female_score)), rep(2,length(male_score))))
problem_set_scores = c(female_score, male_score)
anova(lm(problem_set_scores ~ group))

x_bar = mean(class1_df$`Problem Sets Score (percentage)`)
y_bar = mean(class1_df$`Midterm Exam Score (percentage)`)
sx = sd(class1_df$`Problem Sets Score (percentage)`)
sy = sd(class1_df$`Midterm Exam Score (percentage)`)
r = cor(class1_df$`Problem Sets Score (percentage)`, class1_df$`Midterm Exam Score (percentage)`)

r
x_bar
y_bar
sx
sy

beta = r * (sy / sx)
beta

n = length(class1_df$`Problem Sets Score (percentage)`)

mse.txx = ((1 - (r^2)) / (n - 2)) * (sy ^ 2 / sx ^ 2)
mse.txx

t = beta / sqrt(mse.txx)
t

pvalue = 2 * (1 - pt(abs(t), df = n-2))
pvalue

x_bar = mean(class2_df$`Problem Sets Score (percentage)`)
y_bar = mean(class2_df$`Midterm Exam Score (percentage)`)
sx = sd(class2_df$`Problem Sets Score (percentage)`)
sy = sd(class2_df$`Midterm Exam Score (percentage)`)
r = cor(class2_df$`Problem Sets Score (percentage)`, class2_df$`Midterm Exam Score (percentage)`)

r
x_bar
y_bar
sx
sy

beta = r * (sy / sx)
beta

n = length(class1_df$`Problem Sets Score (percentage)`)

mse.txx = ((1 - (r^2)) / (n - 2)) * (sy ^ 2 / sx ^ 2)
mse.txx

t = beta / sqrt(mse.txx)
t

pvalue = 2 * (1 - pt(abs(t), df = n-2))
pvalue

b = r * (sy / sx)
b

a = y_bar - (b * x_bar)
a

predictMidtermScore = function(a, b, problemSetScore){
  b * problemSetScore + a
}

predictMidtermScore(a, b, 91)

qt(0.975, df = n - 2) * sqrt(mse.txx)

b_min = beta - qt(0.975, df = n - 2) * sqrt(mse.txx)
b_min
b_max = beta + qt(0.975, df = n - 2) * sqrt(mse.txx)
b_max

a_min = y_bar - (b_min * x_bar)
a_min
a_max = y_bar - (b_max * x_bar)
a_max

predictMidtermScore(a_min, b_min, 91)
predictMidtermScore(a_max, b_max, 91)
