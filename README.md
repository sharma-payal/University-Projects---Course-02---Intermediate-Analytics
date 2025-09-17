
*Introduction:*

This project focuses on the application and understanding of two fundamental statistical methods: the chi-square test and Analysis of Variance (ANOVA). 
Through this work, I aimed to develop a comprehensive understanding of these techniques, their assumptions, and their practical implementation. While initial examples often assume ideal conditions, I also explored the real-world considerations necessary for applying these tests, such as validating assumptions and interpreting results accurately. The project involved conducting multiple tests, including 5 chi-square tests, 3 one-way ANOVA tests, and 2 two-way ANOVA tests. Each test was analyzed in detail, with a focus on the steps required for execution, the structure of data tables, and the interpretation of results using the R programming language. This exploration not only enhanced my technical skills but also deepened my appreciation for the nuances of statistical analysis.

**Chi-Square Test**

- Understanding:
The chi-square test is a non-parametric statistical tool used to examine relationships between categorical variables. It is particularly useful for assessing whether observed frequencies differ significantly from expected frequencies under a given hypothesis. For instance, it can be applied to determine if the distribution of preferences for a product varies across different demographic groups.
Key Assumptions:
1.	Categorical Data: Both variables must be categorical, meaning they represent distinct groups or categories.
2.	Independence: Observations must be independent of each other; the outcome of one observation should not influence another.
3.	Mutually Exclusive Categories: Each data point should belong to only one category in the contingency table.
4.	Expected Cell Frequencies: At least 80% of the cells in the contingency table should have an expected frequency of 5 or greater to ensure the validity of the test.
   
- Applications:
The chi-square test is widely used in fields such as social sciences, marketing, and biology to test hypotheses about distributions and associations. For example, it can help determine if there is a significant relationship between gender and voting preferences in an election.

**One-Way ANOVA**

- Understanding:
One-way ANOVA is a parametric test used to compare the means of three or more groups to determine if there are statistically significant differences among them. It is an extension of the t-test, which is limited to comparing only two groups. One-way ANOVA is particularly useful when analyzing the impact of a single categorical independent variable on a continuous dependent variable.
Key Assumptions:
1.	Normality: The dependent variable should be normally distributed within each group.
2.	Homogeneity of Variance: The variance among the groups should be approximately equal.
3.	Independence: Observations must be independent, meaning the data points in one group do not influence those in another.

   
*Post Hoc Analysis*
When ANOVA indicates significant differences, post hoc tests such as the Scheffé test and Tukey’s HSD (Honestly Significant Difference) are used to identify which specific groups differ.
•	Scheffé Test: This test is highly flexible and allows for comparisons of any combination of group means, including complex contrasts. It is particularly useful when sample sizes are unequal or when conducting non-pairwise comparisons.
•	Tukey’s HSD: This test is more focused on pairwise comparisons and is ideal when sample sizes are equal. It controls the family-wise error rate, making it a robust choice for multiple comparisons.
Applications:
One-way ANOVA is commonly used in experimental research, such as testing the effectiveness of different teaching methods on student performance or comparing the yields of various crop varieties under different conditions.

**Two-Way ANOVA**

- Understanding:
Two-way ANOVA extends the one-way ANOVA by incorporating two independent variables (factors) and examining their individual and interactive effects on the dependent variable. This method is particularly powerful for understanding how two factors jointly influence an outcome.
Key Assumptions:
1.	Normality: The dependent variable should be normally distributed within each combination of factor levels.
2.	Homogeneity of Variance: The variance across groups should be consistent.
3.	Independence: Observations must be independent, with no overlap or influence between groups.

*Interactions and Main Effects:*
Two-way ANOVA not only assesses the individual (main) effects of each factor but also evaluates whether there is an interaction effect between the two factors. An interaction effect occurs when the impact of one factor on the dependent variable depends on the level of the other factor.

*Post Hoc Analysis:*
Similar to one-way ANOVA, post hoc tests like Tukey’s HSD can be used to explore specific differences between group means after a significant result is found.
Applications:
Two-way ANOVA is widely used in fields such as psychology, agriculture, and manufacturing. For example, it can be used to study how both temperature and humidity affect the growth rate of plants or how different teaching methods, and class sizes interact to influence student outcomes.
