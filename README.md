# About project

Trading investment strategies in practical application are most often ineffective due to their low predictive power. Financial modeling on backtest, in most cases, generates strategies that work in-samle, but are largely rejected out-of-sample. This is most often explained by the theoretical focus of research on the construction of trading strategies - the focus on minimizing empirical risk and maximizing returns on history. In this case, investors bear higher risks of loss of funds associated with the poor quality and unreliability of such forecasting. Within the framework of this study, a method has been developed to optimize the parameters of the financial market forecasting model, which reduces the effect of overfitting. In this research overfitting means degradation of the forecast quality, and the fight against it is carried out through its "exploitation" on historical data to maximize future financial results. This was the main reason to launch this project.

# About methodology

First, we define overfitting as a degradation in forecast quality for financial models. Second, we use Bailey et al, 2015 approach to estimate overfit in the model. To begin with, we apply the method of combinatorial symmetric cross-validation to our sample. Then we form a cloud of values of the Sharpe ratio for each point in the sample and out of the sample. After that, we use the principal curve method to approximate the past and the future.Thus, this approach allows you to combat the effect of overfitting by reducing the negative effect.

## Bibliography

Bailey D, Borwein J.D., Prado M.L., Zhu Q.J. The probability of backtest overfitting // Journal of Computational Finance (Risk Journals), 2015
Hastie T, Stuetzl W. Principal curves // 1989 American Statistical Association. Journal of the American Statistical Association. June 1989, Vol. 84, No. 406, Theory and Method
