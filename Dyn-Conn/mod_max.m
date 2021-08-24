function [S, Q] = mod_max(gamma, omega, A)

A_new = num2cell(A, [1 2]);
[X, N, T] = size(A);

% Turn Adj Array into Modularity Array
[B, mm] = multiord(A_new, gamma, omega);

PP = @(S) postprocess_ordinal_multilayer(S,T);

% Perform Optimization with Generalized Louvain Method
[S,Q, n_it] = iterated_genlouvain(B, 10000, 0, 1, 'moverandw', [], PP);

Q = Q/mm;
S = reshape(S,N,T);