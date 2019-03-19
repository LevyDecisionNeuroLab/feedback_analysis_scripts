clearvars

%% read files

uncon_file = 'feedback_AllSubj_Par_03012019.xlsx';
con_file = 'feedback_AllSubj_ParConstrained_03082019.xlsx';

uncon = readtable(uncon_file);
con = readtable(con_file);

%% confirm subject id is aligned
id_confirm(:,1) = uncon.ID;
id_confirm(:,2) = con.ID;

scatter(id_confirm(:,1), id_confirm(:,2))


%% distribution of before alpha unconstrained and constrained
figure
histogram(uncon.alpha, 60, 'FaceAlpha', 0.55, 'FaceColor', 'b' )
hold on
histogram(con.alpha,  20, 'FaceAlpha', 0.55, 'FaceColor', 'r' )
title('Before intervention alpha')
hold off

%% distribution of after alpha unconstrained and constrained
figure
histogram(uncon.alpha_1, 60, 'FaceAlpha', 0.55, 'FaceColor', 'b' )
hold on
histogram(con.alpha_1,  20, 'FaceAlpha', 0.55, 'FaceColor', 'r' )
title('After intervention alpha')
hold off

%% distribution of before beta unconstrained and constrained
figure
histogram(uncon.beta, 60, 'FaceAlpha', 0.55, 'FaceColor', 'b' )
hold on
histogram(con.beta,  20, 'FaceAlpha', 0.55, 'FaceColor', 'r' )
title('Before intervention beta')
hold off

%% distribution of after beta unconstrained and constrained
figure
histogram(uncon.beta_1, 60, 'FaceAlpha', 0.55, 'FaceColor', 'b' )
hold on
histogram(con.beta_1,  10, 'FaceAlpha', 0.55, 'FaceColor', 'r' )
title('After intervention beta')
hold off
%% before intervention alpha

figure
scatter(con.alpha, uncon.alpha);
ax = gca
ax.XLabel.String = 'Constrained';
ax.YLabel.String = 'Unconstrained';
x = linspace(0,2.5);
line(x,x)

%% after intervention alpha

figure
scatter(con.alpha_1, uncon.alpha_1);
ax = gca
ax.XLabel.String = 'Constrained';
ax.YLabel.String = 'Unconstrained';
x = linspace(0,2.5);
line(x,x)

%% before intervention beta

figure
scatter(con.beta, uncon.beta);
ax = gca
ax.XLabel.String = 'Constrained';
ax.YLabel.String = 'Unconstrained';
x = linspace(0,4.5);
line(x,x)

%% after intervention beta

figure
scatter(con.beta_1, uncon.beta_1);
ax = gca
ax.XLabel.String = 'Constrained';
ax.YLabel.String = 'Unconstrained';
x = linspace(0,4.5);
line(x,x)