% plot correlation matrix
function plotcorrmat1(tb2plot)

screensize = get( groot, 'Screensize' );

figure('Position', [0.35*(screensize(3)-0.85*screensize(4)) 0.07*screensize(4) 0.85*screensize(4) 0.85*screensize(4)])
[S,AX,BigAx,H,HAx] = plotmatrix(table2array(tb2plot),'.k');
% [S,AX,BigAx,H,HAx] = plotmatrix(table2array(tb2plot));

BigAx.Visible = 'on';
BigAx.XAxisLocation = 'top';
BigAx.XLim = [0 1];
BigAx.XTick = [0.5/size(table2array(tb2plot),2):1/size(table2array(tb2plot),2):1-0.5/size(table2array(tb2plot),2)];
BigAx.XTickLabel = tb2plot.Properties.VariableNames;
BigAx.YAxisLocation = 'right';
BigAx.YTickLabelRotation = -90;
BigAx.YLim = [0 1];
BigAx.YTick = [0.5/size(table2array(tb2plot),2):1/size(table2array(tb2plot),2):1-0.5/size(table2array(tb2plot),2)];
BigAx.YTickLabel = fliplr(tb2plot.Properties.VariableNames);

% correlation coefficient and p value
% the matlab corrcoef function cannot deal with NaN values well
% [corrmat, pmat] = corrcoef(table2array(tb2plot),'rows','complete');

% correlation coefficient and pvalues of correlation
array2corr = table2array(tb2plot);
corrmat = zeros(size(array2corr,2));
pmat = zeros(size(array2corr,2));

for i = 1:size(array2corr,2)
    for j = 1:size(array2corr,2)
        [r,p] = corrcoef(array2corr(:,i),array2corr(:,j),'rows','complete');
        corrmat(i,j) = r(1,2);
        pmat(i,j) = p(1,2);
    end
end

cmap = colormap(parula);
cmap = cmap(length(cmap)/2+1:length(cmap),:);
% cmap = colormap(summer);

% set background color depending on correlation coefficient
for i = 1:size(table2array(tb2plot),2)
    for j = 1:size(table2array(tb2plot),2)
        set(AX(i,j), 'color', cmap(ceil(abs(corrmat(i,j))*length(cmap)),:));
        txt1 = ['r=', num2str(round(corrmat(i,j),2))];
        txt2 = ['p=', num2str(round(pmat(i,j),2,'significant'))];
        txt3 = ['n=', num2str(sum(   ~isnan(table2array(tb2plot(:,i))) & ~isnan(table2array(tb2plot(:,j)))   ))];
        txt = {txt1;txt2;txt3};
        xlab = AX(i,j).XLim;
        ylab = AX(i,j).YLim;
        text(AX(i,j), xlab(1)+(xlab(2)-xlab(1))/7, ylab(2)-(ylab(2)-ylab(1))/5, txt, 'FontSize',8)
    end
end

% colorbar(BigAx,'northoutside')
