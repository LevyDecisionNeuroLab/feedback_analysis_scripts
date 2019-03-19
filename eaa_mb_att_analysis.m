clearvars

% read or load data
root = 'D:\Ruonan\Projects in the lab\Ellen Ambig Avers\Data';

eaatb=readtable(fullfile(root,'feedback_analysis_all_08012018.xlsx')); % the one with corrected unconstrained fitting
% load eaatb.mat

%% exclude extreme model fit, table eaatb2
eaatb2 = eaatb;
eaatb2.alpha_t(eaatb2.alpha_t > 1.0987 | eaatb2.alpha_t < -1) = NaN;
eaatb2.beta_t(eaatb2.beta_t < -4.1475 | eaatb2.beta_t > 0) = NaN;
eaatb2.alpha_t_increase = repmat([eaatb2.alpha_t(eaatb2.is_post == 1) - eaatb2.alpha_t(eaatb2.is_post == 0)],2,1);
eaatb2.beta_t_increase = repmat([eaatb2.beta_t(eaatb2.is_post == 1) - eaatb2.beta_t(eaatb2.is_post == 0)],2,1);

% whether to exclude for model-based analysis. If either alpha or beta is
% out of range, exclude this participant
eaatb2.is_excluded_mb = zeros(height(eaatb2),1);
eaatb2.is_excluded_mb(isnan(eaatb2.beta_t_increase)|isnan(eaatb2.alpha_t_increase)) = 1;
sum(eaatb2.is_excluded_mb)

writetable(eaatb2, 'feedback_analysis_excludeLargeAtt_08012018.xlsx')

%% number of excluded subjects 
% separately for risk and ambiguity
sum(isnan(eaatb2.alpha_t_increase) & eaatb2.is_excluded == 0) / 2
sum(isnan(eaatb2.beta_t_increase) & eaatb2.is_excluded == 0) / 2

sum(isnan(eaatb2.alpha_t) & eaatb2.is_post == 0 & eaatb2.is_excluded == 0)% pre intervention alpha
sum(isnan(eaatb2.alpha_t) & eaatb2.is_post == 1 & eaatb2.is_excluded == 0)% post intervention alpha
eaatb2.id(isnan(eaatb2.alpha_t) & eaatb2.is_post == 0 & eaatb2.is_excluded == 0) % subject ID pre intervention alpha
eaatb2.id(isnan(eaatb2.alpha_t) & eaatb2.is_post == 1 & eaatb2.is_excluded == 0) % subject ID post intervention alpha
intersect(eaatb2.id(isnan(eaatb2.alpha_t) & eaatb2.is_post == 0 & eaatb2.is_excluded == 0),...
    eaatb2.id(isnan(eaatb2.alpha_t) & eaatb2.is_post == 1 & eaatb2.is_excluded == 0)) % subjects both pre and post intervention alpha


sum(isnan(eaatb2.beta_t) & eaatb2.is_post == 0 & eaatb2.is_excluded == 0)% pre intervention beta
sum(isnan(eaatb2.beta_t) & eaatb2.is_post == 1 & eaatb2.is_excluded == 0)% post intervention beta
eaatb2.id(isnan(eaatb2.beta_t) & eaatb2.is_post == 0 & eaatb2.is_excluded == 0) % subject ID pre intervention beta
eaatb2.id(isnan(eaatb2.beta_t) & eaatb2.is_post == 1 & eaatb2.is_excluded == 0) % subject ID post intervention beta
intersect(eaatb2.id(isnan(eaatb2.beta_t) & eaatb2.is_post == 0 & eaatb2.is_excluded == 0),...
    eaatb2.id(isnan(eaatb2.beta_t) & eaatb2.is_post == 1 & eaatb2.is_excluded == 0)) % subject ID both pre and post intervention

% number of total subjects excluded (take overlap into consideration)
sum((isnan(eaatb2.beta_t_increase)| isnan(eaatb2.alpha_t_increase))& eaatb2.is_excluded == 0) / 2

%% count group n and gener
nBR = sum(eaatb2.is_post == 0 & eaatb2.cond == 0 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0)
nBRF = sum(eaatb2.is_post == 0 & eaatb2.cond == 0 & eaatb2.is_excluded == 0 & eaatb2.gender == 2 & eaatb2.is_excluded_mb==0)

ageBR = mean(eaatb2.age(eaatb2.is_post == 0 & eaatb2.cond == 0 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))
ageBRstd = std(eaatb2.age(eaatb2.is_post == 0 & eaatb2.cond == 0 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))

nAC = sum(eaatb2.is_post == 0 & eaatb2.cond == 1 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0)
nACF = sum(eaatb2.is_post == 0 & eaatb2.cond == 1 & eaatb2.is_excluded == 0 & eaatb2.gender == 2 & eaatb2.is_excluded_mb==0)

ageAC = mean(eaatb2.age(eaatb2.is_post == 0 & eaatb2.cond == 1 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))
ageACstd = std(eaatb2.age(eaatb2.is_post == 0 & eaatb2.cond == 1 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))

nNC = sum(eaatb2.is_post == 0 & eaatb2.cond == 2 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0)
nNCF = sum(eaatb2.is_post == 0 & eaatb2.cond == 2 & eaatb2.is_excluded == 0 & eaatb2.gender == 2 & eaatb2.is_excluded_mb==0) 

ageNC = mean(eaatb2.age(eaatb2.is_post == 0 & eaatb2.cond == 2 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))
ageNCstd = std(eaatb2.age(eaatb2.is_post == 0 & eaatb2.cond == 2 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))

nAll = sum(eaatb2.is_post == 0 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0)
nAllF = sum(eaatb2.is_post == 0 & eaatb2.gender == 2 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0)
ageAll = mean(eaatb2.age(eaatb2.is_post == 0 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))
ageAllstd = std(eaatb2.age(eaatb2.is_post == 0 & eaatb2.is_excluded == 0 & eaatb2.is_excluded_mb==0))

%% two-way Anova
include = eaatb2.is_excluded == 0;

% model based risk att: before and after, conditions
[p,tbl,stats,terms] = anovan(eaatb2.alpha_t(include), {eaatb2.id(include), eaatb2.cond(include), eaatb2.is_post(include)},...
    'model', [1 0 0; 0 1 0; 0 0 1; 0 1 1],...
    'nested', [0 1 0; 0 0 0; 0 0 0],...                 % subject is nested in conditions
    'random', [1],...
    'varnames', {'Subjects', 'Conditions', 'Phase'})

% model based ambig att: before and after, conditions
[p,tbl,stats,terms] = anovan(eaatb2.beta_t(include), {eaatb2.id(include), eaatb2.cond(include), eaatb2.is_post(include)},...
    'model', [1 0 0; 0 1 0; 0 0 1; 0 1 1],...
    'nested', [0 1 0; 0 0 0; 0 0 0],...                 % subject is nested in conditions
    'random', [1],...
    'varnames', {'Subjects', 'Conditions', 'Phase'})

%% one-way anova on increase
include = eaatb2.is_excluded == 0 & eaatb2.is_post == 0;
% model based risk att: conditions
[p,tbl,stats,terms] = anovan(eaatb2.alpha_t_increase(include), {eaatb2.cond(include)},...
    'varnames', {'Conditions'})

c = multcompare(stats)

% model based ambig att: conditions
[p,tbl,stats,terms] = anovan(eaatb2.beta_t_increase(include), {eaatb2.cond(include)},...
    'varnames', {'Conditions'})

c = multcompare(stats)


%% plot model based risk attitudes
include_pre = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 & eaatb2.is_excluded_mb == 0;
include_post = eaatb2.is_excluded == 0 & eaatb2.is_post == 1 & eaatb2.is_excluded_mb == 0;

cond0 = eaatb2.is_excluded == 0 & eaatb2.cond == 0 & eaatb2.is_excluded_mb == 0;
cond1 = eaatb2.is_excluded == 0 & eaatb2.cond == 1 & eaatb2.is_excluded_mb == 0;
cond2 = eaatb2.is_excluded == 0 & eaatb2.cond == 2 & eaatb2.is_excluded_mb == 0;

plotmeanRisk = [nanmean(eaatb2.alpha_t(include_pre & cond0)),nanmean(eaatb2.alpha_t(include_pre & cond1)),nanmean(eaatb2.alpha_t(include_pre & cond2));...
    nanmean(eaatb2.alpha_t(include_post & cond0)),nanmean(eaatb2.alpha_t(include_post & cond1)),nanmean(eaatb2.alpha_t(include_post & cond2))]';

plotsemRisk = [nansem(eaatb2.alpha_t(include_pre & cond0)),...
    nansem(eaatb2.alpha_t(include_pre & cond1)),...
    nansem(eaatb2.alpha_t(include_pre & cond2));...
    ...
    nansem(eaatb2.alpha_t(include_post & cond0)),...
    nansem(eaatb2.alpha_t(include_post & cond1)),...
    nansem(eaatb2.alpha_t(include_post & cond2))]';

fig = figure
set(fig, 'Position', [90 200 700 700])
bplot = bar(plotmeanRisk);
hold on
errorbar([1,2,3]-0.14,plotmeanRisk(:,1),plotsemRisk(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2,3]+0.14,plotmeanRisk(:,2),plotsemRisk(:,2),'.','Color',[0,0,0],'LineWidth',2);


%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

bplot(1).EdgeColor = bplot(1).FaceColor;
bplot(2).EdgeColor = bplot(2).FaceColor;


bplot(1).BarWidth = 0.98;
bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
ax.YLabel.String = 'model based risk attitudes'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('model based risk attitudes')

leg = legend('Pre','Post');
leg.FontSize = 20;

%% plot model based risk att change
include = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 & eaatb2.is_excluded_mb == 0;

cond0 = eaatb2.is_excluded == 0 & eaatb2.cond == 0 & eaatb2.is_excluded_mb == 0;
cond1 = eaatb2.is_excluded == 0 & eaatb2.cond == 1 & eaatb2.is_excluded_mb == 0;
cond2 = eaatb2.is_excluded == 0 & eaatb2.cond == 2 & eaatb2.is_excluded_mb == 0;

plotmeanRisk = [nanmean(eaatb2.alpha_t_increase(include & cond0)),nanmean(eaatb2.alpha_t_increase(include & cond1)),nanmean(eaatb2.alpha_t_increase(include & cond2))];

plotsemRisk = [nansem(eaatb2.alpha_t_increase(include & cond0)),...
    nansem(eaatb2.alpha_t_increase(include & cond1)),...
    nansem(eaatb2.alpha_t_increase(include & cond2))];

fig = figure
set(fig, 'Position', [90 200 700 500])
bplot = bar(plotmeanRisk,0.7, 'LineWidth', 2);
hold on
errorbar([1,2,3],plotmeanRisk,plotsemRisk,'.','Color',[0,0,0],'LineWidth',2);

%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

% bplot(1).EdgeColor = bplot(1).FaceColor;
% bplot(2).EdgeColor = bplot(2).FaceColor;

% bplot(1).BarWidth = 0.98;
% bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'model based risk att'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('Att Increase Risk')

%% plot model based ambiguity attitudes
include_pre = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 & eaatb2.is_excluded_mb == 0;
include_post = eaatb2.is_excluded == 0 & eaatb2.is_post == 1 & eaatb2.is_excluded_mb == 0;

cond0 = eaatb2.is_excluded == 0 & eaatb2.cond == 0 & eaatb2.is_excluded_mb == 0;
cond1 = eaatb2.is_excluded == 0 & eaatb2.cond == 1 & eaatb2.is_excluded_mb == 0;
cond2 = eaatb2.is_excluded == 0 & eaatb2.cond == 2 & eaatb2.is_excluded_mb == 0;

plotmeanAmbig = [nanmean(eaatb2.beta_t(include_pre & cond0)),nanmean(eaatb2.beta_t(include_pre & cond1)),nanmean(eaatb2.beta_t(include_pre & cond2));...
    nanmean(eaatb2.beta_t(include_post & cond0)),nanmean(eaatb2.beta_t(include_post & cond1)),nanmean(eaatb2.beta_t(include_post & cond2))]';

plotsemAmbig = [nansem(eaatb2.beta_t(include_pre & cond0)),...
    nansem(eaatb2.beta_t(include_pre & cond1)),...
    nansem(eaatb2.beta_t(include_pre & cond2));...
    ...
    nansem(eaatb2.beta_t(include_post & cond0)),...
    nansem(eaatb2.beta_t(include_post & cond1)),...
    nansem(eaatb2.beta_t(include_post & cond2))]';

fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanAmbig);
hold on
errorbar([1,2,3]-0.14,plotmeanAmbig(:,1),plotsemAmbig(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2,3]+0.14,plotmeanAmbig(:,2),plotsemAmbig(:,2),'.','Color',[0,0,0],'LineWidth',2);


%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

bplot(1).EdgeColor = bplot(1).FaceColor;
bplot(2).EdgeColor = bplot(2).FaceColor;


bplot(1).BarWidth = 0.98;
bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
ax.YLabel.String = 'model based ambiguity attitudes'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('model based ambiguity attitudes')

leg = legend('Pre','Post');
leg.FontSize = 20;

%% plot model based ambig att change
include = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 & eaatb2.is_excluded_mb == 0;

cond0 = eaatb2.is_excluded == 0 & eaatb2.cond == 0 & eaatb2.is_excluded_mb == 0;
cond1 = eaatb2.is_excluded == 0 & eaatb2.cond == 1 & eaatb2.is_excluded_mb == 0;
cond2 = eaatb2.is_excluded == 0 & eaatb2.cond == 2 & eaatb2.is_excluded_mb == 0;

plotmeanAmbig = [nanmean(eaatb2.beta_t_increase(include & cond0)),nanmean(eaatb2.beta_t_increase(include & cond1)),nanmean(eaatb2.beta_t_increase(include & cond2))];

plotsemAmbig = [nansem(eaatb2.beta_t_increase(include & cond0)),...
    nansem(eaatb2.beta_t_increase(include & cond1)),...
    nansem(eaatb2.beta_t_increase(include & cond2))];

fig = figure
set(fig, 'Position', [90 200 700 500])
bplot = bar(plotmeanAmbig,0.7, 'LineWidth', 2);
hold on
errorbar([1,2,3],plotmeanAmbig,plotsemAmbig,'.','Color',[0,0,0],'LineWidth',2);

%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

% bplot(1).EdgeColor = bplot(1).FaceColor;
% bplot(2).EdgeColor = bplot(2).FaceColor;

% bplot(1).BarWidth = 0.98;
% bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'model based ambig att'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('Att Increase Ambig')

%% plot ambig choice - model based r50 estimate
include_pre = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 & eaatb2.is_excluded_mb == 0;
include_post = eaatb2.is_excluded == 0 & eaatb2.is_post == 1 & eaatb2.is_excluded_mb == 0;

cond0 = eaatb2.is_excluded == 0 & eaatb2.cond == 0 & eaatb2.is_excluded_mb == 0;
cond1 = eaatb2.is_excluded == 0 & eaatb2.cond == 1 & eaatb2.is_excluded_mb == 0;
cond2 = eaatb2.is_excluded == 0 & eaatb2.cond == 2 & eaatb2.is_excluded_mb == 0;

plotmeanAmbig = [nanmean(eaatb2.a_r50(include_pre & cond0)),nanmean(eaatb2.a_r50(include_pre & cond1)),nanmean(eaatb2.a_r50(include_pre & cond2));...
    nanmean(eaatb2.a_r50(include_post & cond0)),nanmean(eaatb2.a_r50(include_post & cond1)),nanmean(eaatb2.a_r50(include_post & cond2))]';

plotsemAmbig = [nansem(eaatb2.a_r50(include_pre & cond0)),...
    nansem(eaatb2.a_r50(include_pre & cond1)),...
    nansem(eaatb2.a_r50(include_pre & cond2));...
    ...
    nansem(eaatb2.a_r50(include_post & cond0)),...
    nansem(eaatb2.a_r50(include_post & cond1)),...
    nansem(eaatb2.a_r50(include_post & cond2))]';
%% plot ambig choice - model based r50 estimate change
include = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 & eaatb2.is_excluded_mb == 0;

cond0 = eaatb2.is_excluded == 0 & eaatb2.cond == 0 & eaatb2.is_excluded_mb == 0;
cond1 = eaatb2.is_excluded == 0 & eaatb2.cond == 1 & eaatb2.is_excluded_mb == 0;
cond2 = eaatb2.is_excluded == 0 & eaatb2.cond == 2 & eaatb2.is_excluded_mb == 0;

plotmeanAmbig = [nanmean(eaatb2.a_r50_increase(include & cond0)),nanmean(eaatb2.a_r50_increase(include & cond1)),nanmean(eaatb2.a_r50_increase(include & cond2))];

plotsemAmbig = [nansem(eaatb2.a_r50_increase(include & cond0)),...
    nansem(eaatb2.a_r50_increase(include & cond1)),...
    nansem(eaatb2.a_r50_increase(include & cond2))];

fig = figure
set(fig, 'Position', [90 200 700 500])
bplot = bar(plotmeanAmbig,0.7, 'LineWidth', 2);
hold on
errorbar([1,2,3],plotmeanAmbig,plotsemAmbig,'.','Color',[0,0,0],'LineWidth',2);

%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

% bplot(1).EdgeColor = bplot(1).FaceColor;
% bplot(2).EdgeColor = bplot(2).FaceColor;

% bplot(1).BarWidth = 0.98;
% bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'model based ambig att'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('Att Increase Ambig')

%% att-choice probability change correlation
include_pre = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 ;
include_post = eaatb2.is_excluded == 0 & eaatb2.is_post == 1;

cond0 = eaatb2.cond == 0;
cond1 = eaatb2.cond == 1;
cond2 = eaatb2.cond == 2;
cond12 = eaatb2.cond == 1 | eaatb2.cond == 2;

% correlation matrix (attitudes - choice probability change), BR
tb2plotpre = table(eaatb2.beta_t(include_pre & cond0), eaatb2.alpha_t(include_pre & cond0),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb2.a_increase(include_pre & cond0), eaatb2.r_increase(include_pre & cond0),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('BR att-choice probability correlation')

% correlation matrix (attitudes - choice probability change), AC
tb2plotpre = table(eaatb2.beta_t(include_pre & cond1), eaatb2.alpha_t(include_pre & cond1),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb2.a_increase(include_pre & cond1), eaatb2.r_increase(include_pre & cond1),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('AC att-choice probability correlation')

% correlation matrix (attitudes - choice probability change), NC
tb2plotpre = table(eaatb2.beta_t(include_pre & cond2), eaatb2.alpha_t(include_pre & cond2),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb2.a_increase(include_pre & cond2), eaatb2.r_increase(include_pre & cond2),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('NC att-choice probability correlation')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% linear model containing model based estimated, exclude extreme model fit
include_pre = eaatb2.is_excluded == 0 & eaatb2.is_post == 0 ;
include_post = eaatb2.is_excluded == 0 & eaatb2.is_post == 1;
cond0 = eaatb2.cond == 0;
cond1 = eaatb2.cond == 1;
cond2 = eaatb2.cond == 2;
cond12 = eaatb2.cond == 1 | eaatb2.cond == 2;

% BR
% correlation matrix (attitudes - choice probability change), 
tb2plotpre = table(eaatb2.beta_t(include_pre & cond0), eaatb2.alpha_t(include_pre & cond0),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb2.a_increase(include_pre & cond0), eaatb2.r_increase(include_pre & cond0),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('BR att-choice probability correlation')
% ambig risk att correlation
tb2plotatt = table(eaatb2.beta_t(include_pre & cond0), eaatb2.alpha_t(include_pre & cond0),...
    eaatb2.beta_t(include_post & cond0), eaatb2.alpha_t(include_post & cond0),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plotatt)
title('BR mb att')

%AC
% correlation matrix (attitudes - choice probability change)
tb2plotpre = table(eaatb2.beta_t(include_pre & cond1), eaatb2.alpha_t(include_pre & cond1),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb2.a_increase(include_pre & cond1), eaatb2.r_increase(include_pre & cond1),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('AC att-choice probability correlation')
% ambig risk att correlation
tb2plotatt = table(eaatb2.beta_t(include_pre & cond1), eaatb2.alpha_t(include_pre & cond1),...
    eaatb2.beta_t(include_post & cond1), eaatb2.alpha_t(include_post & cond1),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plotatt)
title('AC mb att')

% NC
% correlation matrix (attitudes - choice probability change)
tb2plotpre = table(eaatb2.beta_t(include_pre & cond2), eaatb2.alpha_t(include_pre & cond2),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb2.a_increase(include_pre & cond2), eaatb2.r_increase(include_pre & cond2),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('NC att-choice probability correlation')
% ambig risk att correlation
tb2plotatt = table(eaatb2.beta_t(include_pre & cond2), eaatb2.alpha_t(include_pre & cond2),...
    eaatb2.beta_t(include_post & cond2), eaatb2.alpha_t(include_post & cond2),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plotatt)
title('NC mb att')

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% without excluding

%% two-way Anova
% model based risk att: before and after, conditions
[p,tbl,stats,terms] = anovan(eaatb.alpha_t(include), {eaatb.id(include), eaatb.cond(include), eaatb.is_post(include)},...
    'model', [1 0 0; 0 1 0; 0 0 1; 0 1 1],...
    'nested', [0 1 0; 0 0 0; 0 0 0],...                 % subject is nested in conditions
    'random', [1],...
    'varnames', {'Subjects', 'Conditions', 'Phase'})

% model based ambig att: before and after, conditions
[p,tbl,stats,terms] = anovan(eaatb.beta_t(include), {eaatb.id(include), eaatb.cond(include), eaatb.is_post(include)},...
    'model', [1 0 0; 0 1 0; 0 0 1; 0 1 1],...
    'nested', [0 1 0; 0 0 0; 0 0 0],...                 % subject is nested in conditions
    'random', [1],...
    'varnames', {'Subjects', 'Conditions', 'Phase'})

%% plot model based risk attitudes
include_pre = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanRisk = [nanmean(eaatb.alpha_t(include_pre & cond0)),nanmean(eaatb.alpha_t(include_pre & cond1)),nanmean(eaatb.alpha_t(include_pre & cond2));...
    nanmean(eaatb.alpha_t(include_post & cond0)),nanmean(eaatb.alpha_t(include_post & cond1)),nanmean(eaatb.alpha_t(include_post & cond2))]';

plotsemRisk = [std(eaatb.alpha_t(include_pre & cond0))/sqrt(length(eaatb.alpha_t(include_pre & cond0))),...
    std(eaatb.alpha_t(include_pre & cond1))/sqrt(length(eaatb.alpha_t(include_pre & cond1))),...
    std(eaatb.alpha_t(include_pre & cond2))/sqrt(length(eaatb.alpha_t(include_pre & cond2)));...
    ...
    std(eaatb.alpha_t(include_post & cond0))/sqrt(length(eaatb.alpha_t(include_post & cond0))),...
    std(eaatb.alpha_t(include_post & cond1))/sqrt(length(eaatb.alpha_t(include_post & cond1))),...
    std(eaatb.alpha_t(include_post & cond2))/sqrt(length(eaatb.alpha_t(include_post & cond2)))]';

fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanRisk);
hold on
errorbar([1,2,3]-0.14,plotmeanRisk(:,1),plotsemRisk(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2,3]+0.14,plotmeanRisk(:,2),plotsemRisk(:,2),'.','Color',[0,0,0],'LineWidth',2);


%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

bplot(1).EdgeColor = bplot(1).FaceColor;
bplot(2).EdgeColor = bplot(2).FaceColor;


bplot(1).BarWidth = 0.98;
bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
ax.YLabel.String = 'model based risk attitudes'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('model based risk attitudes')

leg = legend('Pre','Post');
leg.FontSize = 20;

%% plot model based risk att change
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanRisk = [nanmean(eaatb.alpha_t_increase(include & cond0)),nanmean(eaatb.alpha_t_increase(include & cond1)),nanmean(eaatb.alpha_t_increase(include & cond2))];

plotsemRisk = [std(eaatb.alpha_t_increase(include & cond0))/sqrt(length(eaatb.alpha_t_increase(include & cond0))),...
    std(eaatb.alpha_t_increase(include & cond1))/sqrt(length(eaatb.alpha_t_increase(include & cond1))),...
    std(eaatb.alpha_t_increase(include & cond2))/sqrt(length(eaatb.alpha_t_increase(include & cond2)))];

fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanRisk);
hold on
errorbar([1,2,3],plotmeanRisk,plotsemRisk,'.','Color',[0,0,0],'LineWidth',2);

%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

% bplot(1).EdgeColor = bplot(1).FaceColor;
% bplot(2).EdgeColor = bplot(2).FaceColor;

% bplot(1).BarWidth = 0.98;
% bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
ax.YLabel.String = 'model based risk att'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Att Increase Risk')

%% plot model based ambiguity attitudes
include_pre = eaatb.is_excluded == 0 & eaatb.is_post == 0 ;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.beta_t(include_pre & cond0)),nanmean(eaatb.beta_t(include_pre & cond1)),nanmean(eaatb.beta_t(include_pre & cond2));...
    nanmean(eaatb.beta_t(include_post & cond0)),nanmean(eaatb.beta_t(include_post & cond1)),nanmean(eaatb.beta_t(include_post & cond2))]';

plotsemAmbig = [std(eaatb.beta_t(include_pre & cond0))/sqrt(length(eaatb.beta_t(include_pre & cond0))),...
    std(eaatb.beta_t(include_pre & cond1))/sqrt(length(eaatb.beta_t(include_pre & cond1))),...
    std(eaatb.beta_t(include_pre & cond2))/sqrt(length(eaatb.beta_t(include_pre & cond2)));...
    ...
    std(eaatb.beta_t(include_post & cond0))/sqrt(length(eaatb.beta_t(include_post & cond0))),...
    std(eaatb.beta_t(include_post & cond1))/sqrt(length(eaatb.beta_t(include_post & cond1))),...
    std(eaatb.beta_t(include_post & cond2))/sqrt(length(eaatb.beta_t(include_post & cond2)))]';

fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanAmbig);
hold on
errorbar([1,2,3]-0.14,plotmeanAmbig(:,1),plotsemAmbig(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2,3]+0.14,plotmeanAmbig(:,2),plotsemAmbig(:,2),'.','Color',[0,0,0],'LineWidth',2);


%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

bplot(1).EdgeColor = bplot(1).FaceColor;
bplot(2).EdgeColor = bplot(2).FaceColor;


bplot(1).BarWidth = 0.98;
bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
ax.YLabel.String = 'model based ambiguity attitudes'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('model based ambiguity attitudes')

leg = legend('Pre','Post');
leg.FontSize = 20;

%% plot model based ambig att change
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.beta_t_increase(include & cond0)),nanmean(eaatb.beta_t_increase(include & cond1)),nanmean(eaatb.beta_t_increase(include & cond2))];

plotsemAmbig = [std(eaatb.beta_t_increase(include & cond0))/sqrt(length(eaatb.beta_t_increase(include & cond0))),...
    std(eaatb.beta_t_increase(include & cond1))/sqrt(length(eaatb.beta_t_increase(include & cond1))),...
    std(eaatb.beta_t_increase(include & cond2))/sqrt(length(eaatb.beta_t_increase(include & cond2)))];

fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanAmbig);
hold on
errorbar([1,2,3],plotmeanAmbig,plotsemAmbig,'.','Color',[0,0,0],'LineWidth',2);

%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

% bplot(1).EdgeColor = bplot(1).FaceColor;
% bplot(2).EdgeColor = bplot(2).FaceColor;

% bplot(1).BarWidth = 0.98;
% bplot(2).BarWidth = 0.98;

%axis property
ax = gca;
ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
ax.YLabel.String = 'model based ambig att'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Att Increase Ambig')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

