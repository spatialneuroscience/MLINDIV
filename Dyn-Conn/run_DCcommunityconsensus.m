function run_DCcommunity_consensus(subject_id, gam, ome, n_partitions)
% Add necessary folders to path
addpath(genpath(pwd))

% Read in Explore sessions N x N x Time Slice Adj Arrays
filenames = dir("/mnt/chrastil/lab/users/rob/projects/DynConn/3DArrays/sub-033_Ex*CWExpl_2mm_3Darray.mat");

sub_label = split(filenames(1).name, "_");
sub_label = split(sub_label(1), "-");
sub_labels(1) = sub_label(2);
disp(filenames(1).name);

arr_dict = load(fullfile(filenames(1).folder, filenames(1).name));


array3D_ex1 = permute(arr_dict.scan_1, [3 2 1]);
array3D_ex2 = permute(arr_dict.scan_2, [3 2 1]);

% Read in Test sessions N x N x Time Slice Adj Arrays
filenames = dir("/mnt/chrastil/lab/users/rob/projects/DynConn/3DArrays/sub-033_Run*CWTest_2mm_3Darray.mat");

sub_label = split(filenames(1).name, "_");
sub_label = split(sub_label(1), "-");
sub_labels(1) = sub_label(2);
disp(filenames(1).name);

arr_dict = load(fullfile(filenames(1).folder, filenames(1).name));
array3D_te1 = permute(arr_dict.scan_1, [3 2 1]);
array3D_te2 = permute(arr_dict.scan_2, [3 2 1]);
array3D_te3 = permute(arr_dict.scan_3, [3 2 1]);
array3D_te4 = permute(arr_dict.scan_4, [3 2 1]);
array3D_te5 = permute(arr_dict.scan_5, [3 2 1]);
array3D_te6 = permute(arr_dict.scan_6, [3 2 1]);

% Concatenate all 3D arrays across sessions into one N x N x Time Slice Adj Array
A = cat(3, array3D_ex1, array3D_ex2, array3D_te1, array3D_te2, array3D_te3, array3D_te4, array3D_te5, array3D_te6);

% Set Parameters
time_points = size(A, 3);   % Number of Time Points in 3D Array
n_rois = size(A, 1);        % Number of ROIs in 3D Array

% Initialize Value Arrays
partition_matrix = zeros([n_partitions n_rois time_points]);
consensus_partition = zeros([n_rois time_points]);
sigs = [];

% Extract Consensus Partition
for p=1:n_partitions
    [partition_matrix(p, :, :)] = mod_max(gam, ome, A);
end

for t=1:time_points
    consensus_partition(:, t) = consensus_similarity(squeeze(partition_matrix(:, :, t)));
end

consensus_partition = postprocess_ordinal_multilayer(consensus_partition, time_points);

% Compute Proportion of Communities Significant
for time_point=1:time_points
    community_partition = consensus_partition(:, time_point);
    adjArray = A(:, :, time_point);

    [sigMatrix, Qmatrix, Qmatrix_r] = sig_permtest(community_partition, adjArray, 1000);

    sigs = [sigs diag(sigMatrix)'];
end

proportion_significant = mean(sigs < 0.05);
num_communities = numel(unique(consensus_partition));

% Save to file
formatspec = "partitions/sub%03d_consensus%04d_g%0.3f_o%0.1f.mat";
filename = sprintf(formatspec, subject_id, n_partitions, gam, ome);
save(filename, "consensus_partition", "proportion_significant", "num_communities");

