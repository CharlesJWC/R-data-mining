function [y_hat, err] = ANN(x, w, y)

y_hat = 1/(1+exp(-w*x'));
err = y_hat - y;



end