# Technical Indicators based Buy/Sell Strategy Matrixy

This code introduces the idea of a 'Buy-Sell Strategy Matrix' to choose the best combination of technical indicators to enter and exit the trade for a given security/index. The wider idea stems from the fact that if you have 'n' buying strategies (for e.g. buy at a positive 20/5D SMA or 14/9/8D SMA crossover OR ) and 'm' selling stratgies (for e.g. sell when RSI > 70 or when RSI > 55), you can effectively create a 'n x m' matrix contianing all possible strategy combinations and study their respective risk/return profiles to eventually decide which strategy suits your criteria best.   

In doing so, this simple yet effective idea can help you solve the question that looms over the minds of many investors/traders, i.e. "Which buy/sell trading strategy should I opt for?" 

The indicator based strategies that are formulated and tested are as follows:

    a)	20-Days and 5-Days Simple and Exponential Moving Average crossover (Double crossover).
    b)	14-Days, 9-Days and 8-Days Simple/Exponential Moving Average crossover (Triple crossover).
    c)	Relative Strength Indicator based Buy and Sell Strategies
	
Thus, in total this code provides a 6x6 matrix with 36 possible combinations and thereafter plots three Buy-Sell Strategy Matrices: (a) Cumulative Return, (b)
Annualised Return and (c) Semi SD (to encapsulate downside risk).

**Acknowledgement**: I would like to thank Dr. Chui Yu Ko for making his document on 'Technicals Analysis with R' publicly available on https://bookdown.org/kochiuyu/Technical-Analysis-with-R/about-author.html that assisted me in structuring the foundational code for this analysis document.

**Result Examples are as follows:**

Annualised Return Matrix 


![image](https://user-images.githubusercontent.com/103153445/162549965-1cb6002f-4e7e-410a-865c-676ab36be34f.png)



Best-Strategy Signal Plot
![image](https://user-images.githubusercontent.com/103153445/162549374-b096d081-a692-4b68-bbc9-da835e7c9ca4.png)
