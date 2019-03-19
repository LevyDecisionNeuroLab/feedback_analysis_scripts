% compute the standard error of an array ignoring NaN values
function y = nansem(x)

y = nanstd(x)/sqrt(sum(~isnan(x)));