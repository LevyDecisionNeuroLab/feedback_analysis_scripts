save('eaatb2.mat', 'eaatb2')

writetable(eaatb, 'feedback_analysis_all_09052018.csv')

load eaatb.mat

eaatb = readtable('feedback_analysis_all_09052018.csv');
% exclude model based bad fitting
eaatb = eaatb(eaatb.is_excluded_mb == 0 ,:);

%% Wilcoxon
[p,h,stats] = signrank(eaatb.a(eaatb.cond ==1 & eaatb.is_post ==1 & eaatb.is_excluded == 0),1)

[p,h,stats] = signrank(eaatb.a(eaatb.cond ==2 & eaatb.is_post ==1 & eaatb.is_excluded == 0),1)

[p,h,stats] = signrank(eaatb.a(eaatb.cond ==1 & eaatb.is_post ==1 & eaatb.is_excluded == 0),...
    eaatb.a(eaatb.cond ==2 & eaatb.is_post ==1 & eaatb.is_excluded == 0))


[p,h,stats] = signrank(eaatb.a(eaatb.is_post ==0 & eaatb.is_excluded == 0),1)

%% count group n and gener
nBR = sum(eaatb.is_post == 0 & eaatb.cond == 0 & eaatb.is_excluded == 0);
nBRF = sum(eaatb.is_post == 0 & eaatb.cond == 0 & eaatb.is_excluded == 0 & eaatb.gender == 2);

ageBR = mean(eaatb.age(eaatb.is_post == 0 & eaatb.cond == 0 & eaatb.is_excluded == 0));
ageBRstd = std(eaatb.age(eaatb.is_post == 0 & eaatb.cond == 0 & eaatb.is_excluded == 0));

nAC = sum(eaatb.is_post == 0 & eaatb.cond == 1 & eaatb.is_excluded == 0);
nACF = sum(eaatb.is_post == 0 & eaatb.cond == 1 & eaatb.is_excluded == 0 & eaatb.gender == 2); 

ageAC = mean(eaatb.age(eaatb.is_post == 0 & eaatb.cond == 1 & eaatb.is_excluded == 0));
ageACstd = std(eaatb.age(eaatb.is_post == 0 & eaatb.cond == 1 & eaatb.is_excluded == 0));

nNC = sum(eaatb.is_post == 0 & eaatb.cond == 2 & eaatb.is_excluded == 0);
nNCF = sum(eaatb.is_post == 0 & eaatb.cond == 2 & eaatb.is_excluded == 0 & eaatb.gender == 2); 

ageNC = mean(eaatb.age(eaatb.is_post == 0 & eaatb.cond == 2 & eaatb.is_excluded == 0));
ageNCstd = std(eaatb.age(eaatb.is_post == 0 & eaatb.cond == 2 & eaatb.is_excluded == 0));

nAll = sum(eaatb.is_post == 0);
nAllF = sum(eaatb.is_post == 0 & eaatb.gender == 2);
ageAll = mean(eaatb.age(eaatb.is_post == 0));
ageMinAll = min(eaatb.age(eaatb.is_post == 0))
ageMaxAll = max(eaatb.age(eaatb.is_post == 0))
ageAllstd = std(eaatb.age(eaatb.is_post == 0));

%% Subjects who had full debias
subj_fulldeb = eaatb.id(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.a == 1,:);

subj_closefulldeb = eaatb.id(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.a >0.94 & eaatb.a ~=1,:);
closefulldeb = eaatb(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.a >0.94 & eaatb.a ~=1,:);

preambig = eaatb.a(ismember(eaatb.id,closefulldeb.id) & eaatb.is_post == 0);

%% histogram of post intervention ambiguity choice
figure
hist(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==1), 25)
figure
hist(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==0), 25) 

screensize = get( groot, 'Screensize' );
fig = figure
set(fig, 'Position', [90 150 1700 600])



bins = [0:0.04:1];

% default figure size [744   630   560   420]
% AC pre and post
fig = figure
set(fig, 'Position', [700 500 700 500])
h1 = histogram(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 1), bins,...
    'EdgeColor', 'k', 'FaceColor', [0 0 0], 'FaceAlpha', 0.6, 'LineWidth', 2); % grey
hold on
h2 = histogram(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.cond == 1), bins,...
    'EdgeColor', 'k', 'FaceColor', [1 1 1], 'FaceAlpha', 0.7, 'LineWidth', 2); % white
%axis property
ax = gca;
ax.Box = 'off';
ax.FontSize = 18;
ax.LineWidth =3;
ax.YLim = [0,18];
ax.YTick = [0:2:18];
ax.XLim = [0,1.01];

% AC log (pre and post)
fig = figure
set(fig, 'Position', [700 500 700 500])
h1 = histogram(log(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 1)), 20,...
    'EdgeColor', 'k', 'FaceColor', [0 0 0], 'FaceAlpha', 0.6, 'LineWidth', 2); % grey
hold on
h2 = histogram(log(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.cond == 1)), 20,...
    'EdgeColor', 'k', 'FaceColor', [1 1 1], 'FaceAlpha', 0.7, 'LineWidth', 2); % white
%axis property
ax = gca;
ax.Box = 'off';
ax.FontSize = 18;
ax.LineWidth =3;
ax.YLim = [0,18];
ax.YTick = [0:2:18];
ax.XLim = [0,1.01];

% NC pre and post
fig = figure
set(fig, 'Position', [700 500 700 500])
h1 = histogram(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 2), bins,...
    'EdgeColor', 'k', 'FaceColor', [0 0 0], 'FaceAlpha', 0.6, 'LineWidth', 2); % grey
hold on
h2 = histogram(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.cond == 2), bins,...
    'EdgeColor', 'k', 'FaceColor', [1 1 1], 'FaceAlpha', 0.7, 'LineWidth', 2); % white
%axis property
ax = gca;
ax.Box = 'off';
ax.FontSize = 18;
ax.LineWidth =3;
ax.YLim = [0,18];
ax.YTick = [0:2:18];
ax.XLim = [0,1.01];

% BR pre and post
fig = figure
set(fig, 'Position', [700 500 700 500])
h1 = histogram(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 0), bins,...
    'EdgeColor', 'k', 'FaceColor', [0 0 0], 'FaceAlpha', 0.6, 'LineWidth', 2); % grey
hold on
h2 = histogram(eaatb.a(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.cond == 0), bins,...
    'EdgeColor', 'k', 'FaceColor', [1 1 1], 'FaceAlpha', 0.7, 'LineWidth', 2); % white
%axis property
ax = gca;
ax.Box = 'off';
ax.FontSize = 18;
ax.LineWidth =3;
ax.YLim = [0,18];
ax.YTick = [0:2:18];
ax.XLim = [0,1.01];


% Histogram of change
histogram(eaatb.a_increase(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 0), 20)
histogram(eaatb.a_increase(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 1), 20)
histogram(eaatb.a_increase(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 2), 20)

%% histogram of post intervention ambiguity choice - modeled risk 50 chocie
figure
hist(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==1), 25)
figure
hist(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==0), 25) 

screensize = get( groot, 'Screensize' );
fig = figure
set(fig, 'Position', [90 150 1700 600])


bins = [-0.8:0.06:0.4];
% default figure size [744   630   560   420]
% AC pre and post
fig = figure
set(fig, 'Position', [700 500 700 500])
h1 = histogram(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 1), bins,...
    'EdgeColor', 'k', 'FaceColor', [0 0 0], 'FaceAlpha', 0.6, 'LineWidth', 2); % grey
hold on
h2 = histogram(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.cond == 1), bins,...
    'EdgeColor', 'k', 'FaceColor', [1 1 1], 'FaceAlpha', 0.7, 'LineWidth', 2); % white
%axis property
ax = gca;
ax.Box = 'off';
ax.FontSize =18;
ax.LineWidth =3;
ax.XLim = [-0.8,0.4];
ax.XTick = [-0.8:0.2:0.4];


% NC pre and post
fig = figure
set(fig, 'Position', [700 500 700 500])
h1 = histogram(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 2), bins,...
    'EdgeColor', 'k', 'FaceColor', [0 0 0], 'FaceAlpha', 0.6, 'LineWidth', 2); % grey
hold on
h2 = histogram(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.cond == 2), bins,...
    'EdgeColor', 'k', 'FaceColor', [1 1 1], 'FaceAlpha', 0.7, 'LineWidth', 2); % white
%axis property
ax = gca;
ax.Box = 'off';
ax.FontSize = 18;
ax.LineWidth =3;
ax.YLim = [0,14];
ax.YTick = [0:2:14];
ax.XLim = [-0.8,0.4];
ax.XTick = [-0.8:0.2:0.4];


% BR pre and post
fig = figure
set(fig, 'Position', [700 500 700 500])
h1 = histogram(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 0), bins,...
    'EdgeColor', 'k', 'FaceColor', [0 0 0], 'FaceAlpha', 0.6, 'LineWidth', 2); % grey
hold on
h2 = histogram(eaatb.a_r50(eaatb.is_excluded == 0 & eaatb.is_post ==1 & eaatb.cond == 0), bins,...
    'EdgeColor', 'k', 'FaceColor', [1 1 1], 'FaceAlpha', 0.7, 'LineWidth', 2); % white
%axis property
ax = gca;
ax.Box = 'off';
ax.FontSize = 18;
ax.LineWidth =3;
ax.XLim = [-0.8,0.4];
ax.XTick = [-0.8:0.2:0.4];
ax.YLim = [0, 14];
ax.YTick = [0:2:14];


% Histogram of change
histogram(eaatb.a_increase(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 0), 20)
histogram(eaatb.a_increase(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 1), 20)
histogram(eaatb.a_increase(eaatb.is_excluded == 0 & eaatb.is_post ==0 & eaatb.cond == 2), 20)

%% Organize table to do repeated measure anova
tbpre = eaatb(eaatb.is_post == 0,:);
tbpre.Properties.VariableNames{'r'} = 'r_0';
tbpre.Properties.VariableNames{'a'} = 'a_0';
tbpre.Properties.VariableNames{'r5'} = 'r5_0';
tbpre.Properties.VariableNames{'a5'} = 'a5_0';
tbpre.Properties.VariableNames{'gamma'} = 'gamma_0';
tbpre.Properties.VariableNames{'alpha'} = 'alpha_0';
tbpre.Properties.VariableNames{'alpha_t'} = 'alpha_t_0';
tbpre.Properties.VariableNames{'beta'} = 'beta_0';
tbpre.Properties.VariableNames{'beta_t'} = 'beta_t_0';
tbpre(:,'is_post') = [];

tbpost = eaatb(eaatb.is_post == 1,:);
tbpost.Properties.VariableNames{'r'} = 'r_1';
tbpost.Properties.VariableNames{'a'} = 'a_1';
tbpost.Properties.VariableNames{'r5'} = 'r5_1';
tbpost.Properties.VariableNames{'a5'} = 'a5_1';
tbpost.Properties.VariableNames{'gamma'} = 'gamma_1';
tbpost.Properties.VariableNames{'alpha'} = 'alpha_1';
tbpost.Properties.VariableNames{'alpha_t'} = 'alpha_t_1';
tbpost.Properties.VariableNames{'beta'} = 'beta_1';
tbpost.Properties.VariableNames{'beta_t'} = 'beta_t_1';
tbpost(:,'is_post') = [];

eaatb2 = join(tbpre, tbpost,'Keys', 'id', 'KeepOneCopy', {'cond','is_excluded','gender','age','ep_score',...
    'r_increase','a_increase','r_increase5','a_increase5','gamma_increase','alpha_t_increase','beta_t_increase'});

% transform condition into strings
for i = 1:height(eaatb2)
    if eaatb2.cond(i) == 0
        eaatb2.cond2(i) = 'B';
    elseif eaatb2.cond(i) ==1
        eaatb2.cond2(i) = 'A';
    elseif eaatb2.cond(i) == 2
        eaatb2.cond2(i) = 'N';
    end
end
%% repeated measure Anova
include = eaatb2.is_excluded == 0;
phase = table([0 1]', 'VariableNames',{'phase'});

% ambig model free
rm1 = fitrm(eaatb2(include,:), 'a_0,a_1~cond2','WithinDesign',phase)
% within subject effect
anovatb1 = ranova(rm1)
% between subject effect
anovatb2 = anova(rm1)
% Mauchly's test of sphericity
mauchly(rm1)


%% two-way anova 
include = eaatb.is_excluded == 0;

% model free risk att: before and after, conditions
[p,tbl,stats,terms] = anovan(eaatb.r(include), {eaatb.id(include), eaatb.cond(include), eaatb.is_post(include)},...
    'model', [1 0 0; 0 1 0; 0 0 1; 0 1 1],...
    'nested', [0 1 0; 0 0 0; 0 0 0],...                 % subject is nested in conditions
    'random', [1],...
    'varnames', {'Subjects', 'Conditions', 'Phase'})

% model free risk choices for $5 trials: before and after, conditions
[p,tbl,stats,terms] = anovan(eaatb.r5(include), {eaatb.id(include), eaatb.cond(include), eaatb.is_post(include)},...
    'model', [1 0 0; 0 1 0; 0 0 1; 0 1 1],...
    'nested', [0 1 0; 0 0 0; 0 0 0],...                 % subject is nested in conditions
    'random', [1],...
    'varnames', {'Subjects', 'Conditions', 'Phase'})


% model free ambig att: before and after, conditions
[p,tbl,stats,terms] = anovan(eaatb.a(include), {eaatb.id(include), eaatb.cond(include), eaatb.is_post(include)},...
    'model', [1 0 0; 0 1 0; 0 0 1; 0 1 1],...
    'nested', [0 1 0; 0 0 0; 0 0 0],...                 % subject is nested in conditions
    'random', [1],...
    'varnames', {'Subjects', 'Conditions', 'Phase'})


%% one-way anova on increase
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

% model free risk att: conditions
[p,tbl,stats,terms] = anovan(eaatb.r_increase(include), {eaatb.cond(include)},...
    'varnames', {'Conditions'})

c = multcompare(stats)

% model free risk choice $5: conditions
[p,tbl,stats,terms] = anovan(eaatb.r_increase5(include), {eaatb.cond(include)},...
    'varnames', {'Conditions'})

c = multcompare(stats)

% model free ambig att: conditions
[p,tbl,stats,terms] = anovan(eaatb.a_increase(include), {eaatb.cond(include)},...
    'varnames', {'Conditions'})

c = multcompare(stats)

% model based risk att: conditions
[p,tbl,stats,terms] = anovan(eaatb.alpha_t_increase(include), {eaatb.cond(include)},...
    'varnames', {'Conditions'})

c = multcompare(stats)

% model based ambig att: conditions
[p,tbl,stats,terms] = anovan(eaatb.beta_t_increase(include), {eaatb.cond(include)},...
    'varnames', {'Conditions'})

c = multcompare(stats)

%% ep score anova
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

[p,tbl,stats,terms] = anovan(eaatb.ep_score(include), {eaatb.cond(include)},...
    'varnames', {'Conditions'})

% post-hoc
c = multcompare(stats)

%% plot EP score
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanEp = [nanmean(eaatb.ep_score(include & cond1)),nanmean(eaatb.ep_score(include & cond2)), nanmean(eaatb.ep_score(include & cond0))];

nEP = [sum(~isnan(eaatb.ep_score(include & cond1))),...
    sum(~isnan(eaatb.ep_score(include & cond2))),...
    sum(~isnan(eaatb.ep_score(include & cond0)))];

plotsemEp = [nansem(eaatb.ep_score(include & cond1)),...
    nansem(eaatb.ep_score(include & cond2)),...
    nansem(eaatb.ep_score(include & cond0))];

fig = figure
set(fig, 'Position', [90 200 700 700])
bplot = bar(plotmeanEp, 0.7, 'LineWidth', 2); % bar width 0.5
hold on
errorbar([1,2,3],plotmeanEp,plotsemEp,'.','Color',[0,0,0],'LineWidth',2);

% bplot.EdgeColor = [0.4,0.4,0.4];
% bplot.FaceColor = [0.4,0.4,0.4];

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'EP score'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('EP score')

%% plot inconsistency change
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanInconsist = [nanmean(eaatb.inconsist_increase(include & cond0)),nanmean(eaatb.inconsist_increase(include & cond1)),nanmean(eaatb.inconsist_increase(include & cond2))];

plotsemInconsist = [std(eaatb.inconsist_increase(include & cond0))/sqrt(length(eaatb.inconsist_increase(include & cond0))),...
    std(eaatb.inconsist_increase(include & cond1))/sqrt(length(eaatb.inconsist_increase(include & cond1))),...
    std(eaatb.inconsist_increase(include & cond2))/sqrt(length(eaatb.inconsist_increase(include & cond2)))];

fig = figure
set(fig, 'Position', [90 200 700 700])
bplot = bar(plotmeanInconsist, 0.7, 'LineWidth', 2); % bar width 0.5
hold on
errorbar([1,2,3],plotmeanInconsist,plotsemInconsist,'.','Color',[0,0,0],'LineWidth',2);

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'risky choice change'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('Increase Inconsistency')

%% plot risk choices
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanRisk = [nanmean(eaatb.r(include_0 & cond0)),nanmean(eaatb.r(include_0 & cond1)),nanmean(eaatb.r(include_0 & cond2));...
    nanmean(eaatb.r(include_post & cond0)),nanmean(eaatb.r(include_post & cond1)),nanmean(eaatb.r(include_post & cond2))]';

plotsemRisk = [std(eaatb.r(include_0 & cond0))/sqrt(length(eaatb.r(include_0 & cond0))),...
    std(eaatb.r(include_0 & cond1))/sqrt(length(eaatb.r(include_0 & cond1))),...
    std(eaatb.r(include_0 & cond2))/sqrt(length(eaatb.r(include_0 & cond2)));...
    ...
    std(eaatb.r(include_post & cond0))/sqrt(length(eaatb.r(include_post & cond0))),...
    std(eaatb.r(include_post & cond1))/sqrt(length(eaatb.r(include_post & cond1))),...
    std(eaatb.r(include_post & cond2))/sqrt(length(eaatb.r(include_post & cond2)))]';

fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanRisk, 0.9,'LineWidth', 2);
hold on
errorbar([1,2,3]-0.14,plotmeanRisk(:,1),plotsemRisk(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2,3]+0.14,plotmeanRisk(:,2),plotsemRisk(:,2),'.','Color',[0,0,0],'LineWidth',2);


%bar color
% bplot(1).FaceColor = [104,160,66]/255;
% bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;

% bplot(1).EdgeColor = bplot(1).FaceColor;
% bplot(2).EdgeColor = bplot(2).FaceColor;


bplot(1).BarWidth = 0.85;
bplot(2).BarWidth = 0.85;

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'risky choices'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

leg = legend('Pre-intervention','Post-intervention');
leg.FontSize = 20;

print('riskchoice.bmp', '-dbmp');
im = imread( 'riskchoice.bmp', 'bmp' );


% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'w.',0,0,[],200);
[im_hatch,colorlist] = applyhatch_pluscolor(im,'w.');

title('Choice Percentage Risk')


%% plot risk choices for $5 trials
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanRisk = [nanmean(eaatb.r5(include_0 & cond0)),nanmean(eaatb.r5(include_0 & cond1)),nanmean(eaatb.r5(include_0 & cond2));...
    nanmean(eaatb.r5(include_post & cond0)),nanmean(eaatb.r5(include_post & cond1)),nanmean(eaatb.r5(include_post & cond2))]';

plotsemRisk = [std(eaatb.r5(include_0 & cond0))/sqrt(length(eaatb.r5(include_0 & cond0))),...
    std(eaatb.r5(include_0 & cond1))/sqrt(length(eaatb.r5(include_0 & cond1))),...
    std(eaatb.r5(include_0 & cond2))/sqrt(length(eaatb.r5(include_0 & cond2)));...
    ...
    std(eaatb.r5(include_post & cond0))/sqrt(length(eaatb.r5(include_post & cond0))),...
    std(eaatb.r5(include_post & cond1))/sqrt(length(eaatb.r5(include_post & cond1))),...
    std(eaatb.r5(include_post & cond2))/sqrt(length(eaatb.r5(include_post & cond2)))]';

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
ax.YLabel.String = 'risky choices'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Choice Percentage Risk for $5')

leg = legend('Pre','Post');
leg.FontSize = 20;

%% plot risk choices change
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanRisk = [nanmean(eaatb.r_increase(include & cond1)),nanmean(eaatb.r_increase(include & cond2)),nanmean(eaatb.r_increase(include & cond0))];

plotsemRisk = [...
    std(eaatb.r_increase(include & cond1))/sqrt(length(eaatb.r_increase(include & cond1))),...
    std(eaatb.r_increase(include & cond2))/sqrt(length(eaatb.r_increase(include & cond2))),...
    std(eaatb.r_increase(include & cond0))/sqrt(length(eaatb.r_increase(include & cond0)))];

fig = figure
set(fig, 'Position', [90 200 700 700])
bplot = bar(plotmeanRisk, 0.7, 'LineWidth', 2); % bar width 0.5
hold on
errorbar([1,2,3],plotmeanRisk,plotsemRisk,'.','Color',[0,0,0],'LineWidth',2);

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'risky choice change'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');

title('Choice Percentage Increase Risk')

%% plot risk choices change for $5 trials
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanRisk = [nanmean(eaatb.r_increase5(include & cond0)),nanmean(eaatb.r_increase5(include & cond1)),nanmean(eaatb.r_increase5(include & cond2))];

plotsemRisk = [std(eaatb.r_increase5(include & cond0))/sqrt(length(eaatb.r_increase5(include & cond0))),...
    std(eaatb.r_increase5(include & cond1))/sqrt(length(eaatb.r_increase5(include & cond1))),...
    std(eaatb.r_increase5(include & cond2))/sqrt(length(eaatb.r_increase5(include & cond2)))];

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
ax.YLabel.String = 'risky choices'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Choice Percentage Increase Risk $5')

%% plot ambig choices
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a(include_0 & cond0)),nanmean(eaatb.a(include_0 & cond1)),nanmean(eaatb.a(include_0 & cond2));...
    nanmean(eaatb.a(include_post & cond0)),nanmean(eaatb.a(include_post & cond1)),nanmean(eaatb.a(include_post & cond2))]';

plotsemAmbig = [std(eaatb.a(include_0 & cond0))/sqrt(length(eaatb.a(include_0 & cond0))),...
    std(eaatb.a(include_0 & cond1))/sqrt(length(eaatb.a(include_0 & cond1))),...
    std(eaatb.a(include_0 & cond2))/sqrt(length(eaatb.a(include_0 & cond2)));...
    ...
    std(eaatb.a(include_post & cond0))/sqrt(length(eaatb.a(include_post & cond0))),...
    std(eaatb.a(include_post & cond1))/sqrt(length(eaatb.a(include_post & cond1))),...
    std(eaatb.a(include_post & cond2))/sqrt(length(eaatb.a(include_post & cond2)))]';

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
ax.XTickLabel = {'BR','AC', 'NC'};
ax.XTick = [];
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'ambiguous choices'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Choice Percentage Ambig')

leg = legend('Pre-intervention','Post-intervention');
leg = legend('Pre-intervention','Post-intervention', 'Location', 'northeastoutside');
leg.FontSize = 20;

%% plot ambig50-ambig24 
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a50_a24(include_0 & cond0)),nanmean(eaatb.a50_a24(include_0 & cond1)),nanmean(eaatb.a50_a24(include_0 & cond2));...
    nanmean(eaatb.a50_a24(include_post & cond0)),nanmean(eaatb.a50_a24(include_post & cond1)),nanmean(eaatb.a50_a24(include_post & cond2))]';

plotsemAmbig = [std(eaatb.a50_a24(include_0 & cond0))/sqrt(length(eaatb.a50_a24(include_0 & cond0))),...
    std(eaatb.a50_a24(include_0 & cond1))/sqrt(length(eaatb.a50_a24(include_0 & cond1))),...
    std(eaatb.a50_a24(include_0 & cond2))/sqrt(length(eaatb.a50_a24(include_0 & cond2)));...
    ...
    std(eaatb.a50_a24(include_post & cond0))/sqrt(length(eaatb.a50_a24(include_post & cond0))),...
    std(eaatb.a50_a24(include_post & cond1))/sqrt(length(eaatb.a50_a24(include_post & cond1))),...
    std(eaatb.a50_a24(include_post & cond2))/sqrt(length(eaatb.a50_a24(include_post & cond2)))]';

%% plot ambig74-ambig24 
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a74_a24(include_0 & cond0)),nanmean(eaatb.a74_a24(include_0 & cond1)),nanmean(eaatb.a74_a24(include_0 & cond2));...
    nanmean(eaatb.a74_a24(include_post & cond0)),nanmean(eaatb.a74_a24(include_post & cond1)),nanmean(eaatb.a74_a24(include_post & cond2))]';

plotsemAmbig = [std(eaatb.a74_a24(include_0 & cond0))/sqrt(length(eaatb.a74_a24(include_0 & cond0))),...
    std(eaatb.a74_a24(include_0 & cond1))/sqrt(length(eaatb.a74_a24(include_0 & cond1))),...
    std(eaatb.a74_a24(include_0 & cond2))/sqrt(length(eaatb.a74_a24(include_0 & cond2)));...
    ...
    std(eaatb.a74_a24(include_post & cond0))/sqrt(length(eaatb.a74_a24(include_post & cond0))),...
    std(eaatb.a74_a24(include_post & cond1))/sqrt(length(eaatb.a74_a24(include_post & cond1))),...
    std(eaatb.a74_a24(include_post & cond2))/sqrt(length(eaatb.a74_a24(include_post & cond2)))]';


%% plot ambig choices - modeled risk 50 choices
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a_r50(include_0 & cond0)),nanmean(eaatb.a_r50(include_0 & cond1)),nanmean(eaatb.a_r50(include_0 & cond2));...
    nanmean(eaatb.a_r50(include_post & cond0)),nanmean(eaatb.a_r50(include_post & cond1)),nanmean(eaatb.a_r50(include_post & cond2))]';

plotsemAmbig = [std(eaatb.a_r50(include_0 & cond0))/sqrt(length(eaatb.a_r50(include_0 & cond0))),...
    std(eaatb.a_r50(include_0 & cond1))/sqrt(length(eaatb.a_r50(include_0 & cond1))),...
    std(eaatb.a_r50(include_0 & cond2))/sqrt(length(eaatb.a_r50(include_0 & cond2)));...
    ...
    std(eaatb.a_r50(include_post & cond0))/sqrt(length(eaatb.a_r50(include_post & cond0))),...
    std(eaatb.a_r50(include_post & cond1))/sqrt(length(eaatb.a_r50(include_post & cond1))),...
    std(eaatb.a_r50(include_post & cond2))/sqrt(length(eaatb.a_r50(include_post & cond2)))]';

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
ax.XTickLabel = {'BR','AC', 'NC'};
ax.XTick = [];
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'ambiguous choices'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Choice Percentage Ambig-Risk50')

leg = legend('Pre-intervention','Post-intervention');
leg = legend('Pre-intervention','Post-intervention', 'Location', 'northeastoutside');
leg.FontSize = 20;

%% plot ambig choices proportion difference (adiff)
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.adiff(include_0 & cond0)),nanmean(eaatb.adiff(include_0 & cond1)),nanmean(eaatb.adiff(include_0 & cond2));...
    nanmean(eaatb.adiff(include_post & cond0)),nanmean(eaatb.adiff(include_post & cond1)),nanmean(eaatb.adiff(include_post & cond2))]';

plotsemAmbig = [std(eaatb.adiff(include_0 & cond0))/sqrt(length(eaatb.adiff(include_0 & cond0))),...
    std(eaatb.adiff(include_0 & cond1))/sqrt(length(eaatb.adiff(include_0 & cond1))),...
    std(eaatb.adiff(include_0 & cond2))/sqrt(length(eaatb.adiff(include_0 & cond2)));...
    ...
    std(eaatb.adiff(include_post & cond0))/sqrt(length(eaatb.adiff(include_post & cond0))),...
    std(eaatb.adiff(include_post & cond1))/sqrt(length(eaatb.adiff(include_post & cond1))),...
    std(eaatb.adiff(include_post & cond2))/sqrt(length(eaatb.adiff(include_post & cond2)))]';

%% plot ambig choices with $5 trials
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a5(include_0 & cond0)),nanmean(eaatb.a5(include_0 & cond1)),nanmean(eaatb.a5(include_0 & cond2));...
    nanmean(eaatb.a5(include_post & cond0)),nanmean(eaatb.a5(include_post & cond1)),nanmean(eaatb.a5(include_post & cond2))]';

plotsemAmbig = [std(eaatb.a5(include_0 & cond0))/sqrt(length(eaatb.a5(include_0 & cond0))),...
    std(eaatb.a5(include_0 & cond1))/sqrt(length(eaatb.a5(include_0 & cond1))),...
    std(eaatb.a5(include_0 & cond2))/sqrt(length(eaatb.a5(include_0 & cond2)));...
    ...
    std(eaatb.a5(include_post & cond0))/sqrt(length(eaatb.a5(include_post & cond0))),...
    std(eaatb.a5(include_post & cond1))/sqrt(length(eaatb.a5(include_post & cond1))),...
    std(eaatb.a5(include_post & cond2))/sqrt(length(eaatb.a5(include_post & cond2)))]';

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
ax.YLabel.String = 'ambiguous choices'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Choice Percentage Ambig $5')

leg = legend('Pre','Post');
leg.FontSize = 20;

%% plot ambig choices change
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a_increase(include & cond0)),nanmean(eaatb.a_increase(include & cond1)),nanmean(eaatb.a_increase(include & cond2))];

plotsemAmbig = [std(eaatb.a_increase(include & cond0))/sqrt(length(eaatb.a_increase(include & cond0))),...
    std(eaatb.a_increase(include & cond1))/sqrt(length(eaatb.a_increase(include & cond1))),...
    std(eaatb.a_increase(include & cond2))/sqrt(length(eaatb.a_increase(include & cond2)))];

fig = figure
set(fig, 'Position', [90 200 700 700])
bplot = bar(plotmeanAmbig, 0.7, 'LineWidth', 2); % bar width 0.5
hold on
errorbar([1,2,3],plotmeanAmbig,plotsemAmbig,'.','Color',[0,0,0],'LineWidth',2);

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'risky choice change'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('Choice Percentage Increase Ambig')

%% plot ambig-modeled risk50 choices change
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a_r50_increase(include & cond1)),nanmean(eaatb.a_r50_increase(include & cond2)),nanmean(eaatb.a_r50_increase(include & cond0))];

plotsemAmbig = [...
    std(eaatb.a_r50_increase(include & cond1))/sqrt(length(eaatb.a_r50_increase(include & cond1))),...
    std(eaatb.a_r50_increase(include & cond2))/sqrt(length(eaatb.a_r50_increase(include & cond2))),...
    std(eaatb.a_r50_increase(include & cond0))/sqrt(length(eaatb.a_r50_increase(include & cond0)))];

fig = figure
set(fig, 'Position', [90 200 700 700])
bplot = bar(plotmeanAmbig, 0.7, 'LineWidth', 2); % bar width 0.5
hold on
errorbar([1,2,3],plotmeanAmbig,plotsemAmbig,'.','Color',[0,0,0],'LineWidth',2);

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'risky choice change'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('Choice Percentage Increase Ambig')

%% plot ambig choices change
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.adiff_increase(include & cond0)),nanmean(eaatb.adiff_increase(include & cond1)),nanmean(eaatb.adiff_increase(include & cond2))];

plotsemAmbig = [std(eaatb.adiff_increase(include & cond0))/sqrt(length(eaatb.adiff_increase(include & cond0))),...
    std(eaatb.adiff_increase(include & cond1))/sqrt(length(eaatb.adiff_increase(include & cond1))),...
    std(eaatb.adiff_increase(include & cond2))/sqrt(length(eaatb.adiff_increase(include & cond2)))];

fig = figure
set(fig, 'Position', [90 200 700 700])
bplot = bar(plotmeanAmbig, 0.7, 'LineWidth', 2); % bar width 0.5
hold on
errorbar([1,2,3],plotmeanAmbig,plotsemAmbig,'.','Color',[0,0,0],'LineWidth',2);

%axis property
ax = gca;
ax.XTick = [];
% ax.XTickLabel = {'Base Rate','Active Counting', 'Nonactive Counting'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;
% ax.YLabel.String = 'risky choice change'; 
% ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

% Aply pattern
[im_hatch,colorlist] = applyhatch_pluscolor(gcf,'/');


title('Choice Percentage Increase Ambig')

%% plot ambig choices change $5
include = eaatb.is_excluded == 0 & eaatb.is_post == 0;

cond0 = eaatb.is_excluded == 0 & eaatb.cond == 0;
cond1 = eaatb.is_excluded == 0 & eaatb.cond == 1;
cond2 = eaatb.is_excluded == 0 & eaatb.cond == 2;

plotmeanAmbig = [nanmean(eaatb.a_increase5(include & cond0)),nanmean(eaatb.a_increase5(include & cond1)),nanmean(eaatb.a_increase5(include & cond2))];

plotsemAmbig = [std(eaatb.a_increase5(include & cond0))/sqrt(length(eaatb.a_increase5(include & cond0))),...
    std(eaatb.a_increase5(include & cond1))/sqrt(length(eaatb.a_increase5(include & cond1))),...
    std(eaatb.a_increase5(include & cond2))/sqrt(length(eaatb.a_increase5(include & cond2)))];

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
ax.YLabel.String = 'ambig choices'; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title('Choice Percentage Increase Ambig $5')


%% plot correlation
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0 ;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.cond == 0;
cond1 = eaatb.cond == 1;
cond2 = eaatb.cond == 2;
cond12 = eaatb.cond == 1 | eaatb.cond == 2;


plotcorr(eaatb.r(include_0 & cond12), eaatb.r_increase(include_0 & cond12), 'Pre risk choices', 'risk choices increase')
title('AC and NC')
plotcorr(eaatb.alpha_t(include_0 & cond12), eaatb.alpha_t_increase(include_0 & cond12), 'Pre risk att', 'risk att increase')
title('AC and NC')
plotcorr(eaatb.alpha_t(include_0 & cond12), eaatb.r_increase(include_0 & cond12), 'Pre risk att', 'risk choices increase')
title('AC and NC')

plotcorr(eaatb.a(include_0 & cond12), eaatb.a_increase(include_0 & cond12), 'Pre ambig choices', 'ambig choices change')
title('AC and NC')
plotcorr(eaatb.beta_t(include_0 & cond12), eaatb.beta_t_increase(include_0 & cond12), 'Pre ambig att', 'ambig att change')
title('AC and NC')
plotcorr(eaatb.beta_t(include_0 & cond12), eaatb.a_increase(include_0 & cond12), 'Pre ambig att', 'ambig choices change')
title('AC and NC')

% pre-post correlation matrix, BR
tb2plot = table(eaatb.a(include_0 & cond0), eaatb.r(include_0 & cond0),...
    eaatb.a(include_post & cond0), eaatb.r(include_post & cond0),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plot)
title('BR choice probability Pre-Post correlation')

% pre-post correlation matrix, AC
tb2plot = table(eaatb.a(include_0 & cond1), eaatb.r(include_0 & cond1),...
    eaatb.a(include_post & cond1), eaatb.r(include_post & cond1),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plot)
title('AC choice probability Pre-Post correlation')

% pre-post correlation matrix, NC
tb2plot = table(eaatb.a(include_0 & cond2), eaatb.r(include_0 & cond2),...
    eaatb.a(include_post & cond2), eaatb.r(include_post & cond2),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plot)
title('NC choice probability Pre-Post correlation')



% pre-post correlation matrix, BR
tb2plot = table(eaatb.beta_t(include_0 & cond0), eaatb.alpha_t(include_0 & cond0),...
    eaatb.beta_t(include_post & cond0), eaatb.alpha_t(include_post & cond0),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plot)
title('BR att Pre-Post correlation')

% pre-post correlation matrix, AC
tb2plot = table(eaatb.beta_t(include_0 & cond1), eaatb.alpha_t(include_0 & cond1),...
    eaatb.beta_t(include_post & cond1), eaatb.alpha_t(include_post & cond1),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plot)
title('AC att Pre-Post correlation')

% pre-post correlation matrix, NC
tb2plot = table(eaatb.beta_t(include_0 & cond2), eaatb.alpha_t(include_0 & cond2),...
    eaatb.beta_t(include_post & cond2), eaatb.alpha_t(include_post & cond2),...
    'VariableNames', {'AmbigPre', 'RiskPre', 'AmbigPost', 'RiskPost'});
plotcorrmat1(tb2plot)
title('NC att Pre-Post correlation')



% pre-change, change-change correlation matrix, BR
tb2plotpre = table(eaatb.a(include_0 & cond0), eaatb.r(include_0 & cond0),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond0), eaatb.r_increase(include_0 & cond0),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat1(tb2plotchange)
title('BR choice probability change correlation')
plotcorrmat2(tb2plotpre, tb2plotchange)
title('BR choice probability correlation')

% pre-change, change-change correlation matrix, AC
tb2plotpre = table(eaatb.a(include_0 & cond1), eaatb.r(include_0 & cond1),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond1), eaatb.r_increase(include_0 & cond1),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat1(tb2plotchange)
title('AC choice probability change correlation')
plotcorrmat2(tb2plotpre, tb2plotchange)
title('AC choice probability correlation')

% pre-change, change-change correlation matrix, NC
tb2plotpre = table(eaatb.a(include_0 & cond2), eaatb.r(include_0 & cond2),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond2), eaatb.r_increase(include_0 & cond2),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat1(tb2plotchange)
title('NC choice probability change correlation')
plotcorrmat2(tb2plotpre, tb2plotchange)
title('NC choice probability correlation')


% correlation matrix (attitudes - choice probability change), BR
tb2plotpre = table(eaatb.beta_t(include_0 & cond0), eaatb.alpha_t(include_0 & cond0),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond0), eaatb.r_increase(include_0 & cond0),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('BR att-choice probability correlation')

% correlation matrix (attitudes - choice probability change), AC
tb2plotpre = table(eaatb.beta_t(include_0 & cond1), eaatb.alpha_t(include_0 & cond1),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond1), eaatb.r_increase(include_0 & cond1),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('AC att-choice probability correlation')

% correlation matrix (attitudes - choice probability change), NC
tb2plotpre = table(eaatb.beta_t(include_0 & cond2), eaatb.alpha_t(include_0 & cond2),...
    'VariableNames', {'AmbigPre', 'RiskPre'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond2), eaatb.r_increase(include_0 & cond2),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotpre, tb2plotchange)
title('NC att-choice probability correlation')


% EP - choice prob change correlation
%  BR
tb2plotEp = table(eaatb.ep_score(include_0 & cond0),...
    'VariableNames', {'EpScore'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond0), eaatb.r_increase(include_0 & cond0),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotEp,tb2plotchange)
title('BR EP choice probability change correlation')
% AC
tb2plotEp = table(eaatb.ep_score(include_0 & cond1),...
    'VariableNames', {'EpScore'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond1), eaatb.r_increase(include_0 & cond1),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotEp,tb2plotchange)
title('AC EP choice probability change correlation')

plotcorr(eaatb.ep_score(include_0 & cond1),eaatb.a_increase(include_0 & cond1),'','');

plotcorr(eaatb.ep_score(include_0 & cond1),eaatb.a_r50_increase(include_0 & cond1),'','');

% NC
tb2plotEp = table(eaatb.ep_score(include_0 & cond2),...
    'VariableNames', {'EpScore'});
tb2plotchange = table(eaatb.a_increase(include_0 & cond2), eaatb.r_increase(include_0 & cond2),...
    'VariableNames', {'AmbigChange', 'RiskChange'});
plotcorrmat2(tb2plotEp,tb2plotchange)
title('NC EP choice probability change correlation')

plotcorr(eaatb.ep_score(include_0 & cond2),eaatb.a_increase(include_0 & cond2),'','');

plotcorr(eaatb.ep_score(include_0 & cond2),eaatb.a_r50_increase(include_0 & cond2),'','');

% correlation between risk and ambiguity
include_pre = eaatb.is_excluded == 0 & eaatb.is_post == 0;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;
cond0 = eaatb.cond == 0;
cond1 = eaatb.cond == 1;
cond2 = eaatb.cond == 2;
cond12 = eaatb.cond == 1 | eaatb.cond == 2;

plotcorr(eaatb.a(include_post), eaatb.r(include_pre), 'Ambiguity', 'Risk')
ax = gca;
ax.YLim = [-0.1,1.1];
ax.XLim = [-0.1,1.1];
ax.XTick = [0,0.2,0.4,0.6,0.8,1];

plotcorr(eaatb.a_r50(include_post), eaatb.r(include_post), 'Ambiguity', 'Risk')
ax = gca;
ax.XLim = [-1,0.6];
ax.XTick = [-1:0.4:0.6];

plotcorr(eaatb.a_r50(include_post & cond12), eaatb.r(include_post & cond12), 'Ambiguity', 'Risk')
ax = gca;
ax.XLim = [-1,0.6];
ax.XTick = [-1:0.4:0.6];

plotcorr(eaatb.a_r50(include_post & cond0), eaatb.r(include_post & cond0), 'Ambiguity', 'Risk')
ax = gca;
ax.XLim = [-1,0.6];
ax.XTick = [-1:0.4:0.6];

plotcorr(eaatb.a_r50(include_pre), eaatb.r(include_pre), 'Ambiguity', 'Risk')
ax = gca;
ax.XLim = [-1,0.6];
ax.XTick = [-1:0.4:0.6];

plotcorr(eaatb.a_r50(include_pre & cond12), eaatb.r(include_pre & cond12), 'Ambiguity', 'Risk')
ax = gca;
ax.XLim = [-1,0.6];
ax.XTick = [-1:0.4:0.6];

plotcorr(eaatb.a_r50(include_pre & cond0), eaatb.r(include_pre & cond0), 'Ambiguity', 'Risk')
ax = gca;
ax.XLim = [-1,0.6];
ax.XTick = [-1:0.4:0.6];

[rho,pval] = corr(eaatb.a_r50(include_pre & cond12), eaatb.r(include_pre & cond12))


plotcorr(eaatb.beta_t(include_post), eaatb.alpha_t(include_post), 'Ambiguity', 'Risk')

plotcorr(eaatb.a_increase(include_pre & cond1),...
    eaatb.r_increase(include_pre & cond1), 'Ambiguity', 'Risk')
plotcorr(eaatb.a_increase(include_pre & cond2),...
    eaatb.r_increase(include_pre & cond2), 'Ambiguity', 'Risk')

plotcorr(eaatb.a_r50_increase(include_pre & cond12),...
    eaatb.r_increase(include_pre & cond12), 'Ambiguity', 'Risk')
ax = gca;
ax.XLim = [-0.4,0.6];
ax.XTick = [-0.4:0.2:0.6];

%% Linear model
include_0 = eaatb.is_excluded == 0 & eaatb.is_post == 0 ;
include_post = eaatb.is_excluded == 0 & eaatb.is_post == 1;

cond0 = eaatb.cond == 0;
cond1 = eaatb.cond == 1;
cond2 = eaatb.cond == 2;
cond12 = eaatb.cond == 1 | eaatb.cond == 2;

%linear model choice as a variable, for all subj

lme0 = fitlme(eaatb(include_0 & cond0,:),'a_increase ~ a + ep_score')
lme1 = fitlme(eaatb(include_0 & cond1,:),'a_increase ~ a + ep_score')
lme2 = fitlme(eaatb(include_0 & cond2,:),'a_increase ~ a + ep_score')
lme3 = fitlme(eaatb(include_0 & cond12,:),'a_increase ~ a + ep_score + cond + ep_score*cond')
lme4 = fitlme(eaatb(include_0 & cond12,:),'a_increase ~ a + ep_score + cond')
lme5 = fitlme(eaatb(include_0,:),'a_increase ~ a + ep_score')
lme5 = fitlme(eaatb(include_0,:),'a_increase ~ beta_t + ep_score')

lme0 = fitlme(eaatb(include_0 & cond0,:),'r_increase ~ r + ep_score')
lme1 = fitlme(eaatb(include_0 & cond1,:),'r_increase ~ r + ep_score')
lme2 = fitlme(eaatb(include_0 & cond2,:),'r_increase ~ r + ep_score')
lme3 = fitlme(eaatb(include_0 & cond12,:),'r_increase ~ r + ep_score + cond + ep_score*cond')
lme4 = fitlme(eaatb(include_0 & cond12,:),'r_increase ~ r + ep_score + cond')
lme5 = fitlme(eaatb(include_0,:),'r_increase ~ r + ep_score')
lme5 = fitlme(eaatb(include_0,:),'r_increase ~ alpha_t + ep_score')

%% linear model of AC and NC combined
eaatab.group = (eaatab.cond-1);
for i = 1:length(eaatab.subj)
    eaatab.groupbyscore(i) = eaatab.group(i)*eaatab.score(i);
end

%lme3 = fitlme(eaatab(find(eaatab.cond ==2 | eaatab.cond ==1),:),'ambigdiff ~  group + ambig_b + score')
% Group: AC as 
lme4 = fitlme(eaatab(find(eaatab.cond ==2 | eaatab.cond ==1),:),'ambigdiff ~ groupbyscore + group + ambig_b + score')


% pnchigh = eaatab(find(eaatab.ambigatt > 0.2),:);
% lmehigh = fitlme(pnchigh,'pupil ~ choice + (1|subj)')
% 
% pnclow = eaatab(find(eaatab.ambigatt < 0.2),:);
% lmelow = fitlme(pnclow,'pupil ~ al + (1|subj)')
% 
%%
% linear model for each subj
lmechoice_subj = cell(length(eaatab.subj),1);
for i = 1:length(eaatab.subj)
    tabtemp = table;
    tabtemp = eaatab(find(eaatab.subj == eaatab.subj(i)),:);
    if isnan(tabtemp.score) == 0
        lmechoice_subj{i} = fitlme(tabtemp,'ambigdiff ~ score');
        coeff_choice(i,1) = double(lmechoice_subj{i}.Coefficients (2,2)); % regression coefficient for choice
        coeff_choice(i,2) = double(lmechoice_subj{i}.Coefficients (2,6)); % p value for choice
    end
end
