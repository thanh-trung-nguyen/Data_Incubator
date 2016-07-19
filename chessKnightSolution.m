function [E,std,condProb] = chessKnightSolution(nrMoves,div1,div2)
% function chessKnightSolution calculates the expected values, standard
% deviation of the sum modulo nrMoves, and the conditional probability that 
% the sum is divisible by "div2" given that it is divisible by "div1"
% Note: the knight starts from zero on a numeric keyboard as follows: 
%     1   2   3
%     4   5   6
%     7   8   9
%         0
% Input parameters:
%       numMoves: number of moves
%       "div1" and "div2": see above explanation. 
% Output parameters: 
%       E: expected value of sum S mod nrMoves,
%       std: standard deviation of S mod nrMoves
%       condProb: conditional probability that S is divisible by div2 given
%       that S is divisible by div1. 
% Example: [E,std,condProb] = chessKnightSolution(10,7,5)
% Example: [E,std,condProb] = chessKnightSolution(1024,29,23)

% =========================================================================
% Method: use recursive formulas and conditional probability to calculate
% the probability for number of moves equal to 1, 2, 3..., nrMoves. 
% Note: this algorithm is fast, but must be modified if the knight starts from another
% location.
% Note: the index of number zero is replaced by 10 in indexing. 
% Thanh Nguyen, 2016.

if nrMoves < 1
    error('At least one move'); 
end

N = 1; % first move
P = zeros(9*N,10); % P(k,j) is the probability that the sum is equal to k 
                   % and the knight is at number j (note: j=10 for number zero) after N move. 

P(4,4) = 1/2; % in the first move the knight may move from zero to 4 or 6 with equal probability. 
P(6,6) = 1/2; 


for n = 1:nrMoves-1
    Pnew = zeros(9*(n+1),10); % possible values of the sum increases. 
    i = 1:9*n;
    if mod(n,2)==0 % even number
        Pnew(i+2,2) = 1/2*P(i,7) + 1/2*P(i,9); % if the knight moves to 2
        Pnew(i+4,4) = 1/2*P(i,3) + 1/2*P(i,9) + 1/2*P(i,10); % if the knight moves to 4
        Pnew(i+6,6) = 1/2*P(i,1) + 1/2*P(i,7) + 1/2*P(i,10); % if the knight moves to 6
        Pnew(i+8,8) = 1/2*P(i,1) + 1/2*P(i,3); % if the knight moves to 8

    else
        Pnew(i,10) = 1/3*P(i,4) + 1/3*P(i,6); % if the knight moves to 0, the sum remains the same.
        Pnew(i+1,1) = 1/3*P(i,6) + 1/2*P(i,8); % if the knight moves to 1
        Pnew(i+3,3) = 1/3*P(i,4) + 1/2*P(i,8); % if the knight moves to 3
        Pnew(i+7,7) = 1/2*P(i,2) + 1/3*P(i,6);
        Pnew(i+9,9) = 1/2*P(i,2) + 1/3*P(i,4);
    end
    P = Pnew;     
end

% expected value: 
SUM = 1:9*nrMoves; % range of sum

P = sum(P,2); % disgard the location of the knight, just take the probability for each value of the sum.

% remove sums with zero probability:
idx = find(P>0);
P = P(idx); SUM = SUM(idx); 

% take modulo: 
SUMmod = mod(SUM,nrMoves); 
E = SUMmod*P;   % P is a column vector, SUMmod is a row vector.

% standard derivation:
std = sqrt(SUMmod.^2*P - E^2);

% conditional probability: 
idx1 = find(mod(SUM,div1)==0); % consider only sums with positive probabilities and divisible by div1.

if isempty(idx1)
    condProb = 1;
else   
    SUM = SUM(idx1); P = P(idx1);
    condProb = sum(P(mod(SUM,div2)==0))/sum(P);
end
fprintf('%s%d%s%15.10f\n','Expected value of S mod ',nrMoves,' is ',E);
fprintf('%s%d%s%15.10f\n','Standard deviation of S mod ',nrMoves,' is ',std);
fprintf('%s%15.10f\n','Conditional probability is ',condProb);


