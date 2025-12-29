"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.LUDecomposeBareiss = LUDecomposeBareiss;
exports.LUSolveBareissMulti = LUSolveBareissMulti;
exports.LUSolveBareissTransposeMulti = LUSolveBareissTransposeMulti;
exports.LUDecomposeBareissT = LUDecomposeBareissT;
exports.LUSolveBareissMultiT = LUSolveBareissMultiT;
exports.LUSolveBareissTransposeMultiT = LUSolveBareissTransposeMultiT;
exports.solveRectangularBareiss = solveRectangularBareiss;
exports.solveRectangularBareissT = solveRectangularBareissT;
//-----------------------------------------------------------------------------
// LU Decomposition and Solvers
//-----------------------------------------------------------------------------
function LUDecomposeBareiss(A, pivot = true) {
    const N = A.length;
    const perm = Array.from({ length: N }, (_, i) => i);
    let prev = 1;
    let swaps = 0;
    for (let k = 0; k < N - 1; k++) {
        if (pivot && A[k][k] === 0) {
            let swap = k + 1;
            while (swap < N && A[swap][k] === 0)
                swap++;
            if (swap === N)
                return { perm, swaps };
            [A[k], A[swap]] = [A[swap], A[k]];
            [perm[k], perm[swap]] = [perm[swap], perm[k]];
            swaps++;
        }
        const akk = A[k][k];
        for (let i = k + 1; i < N; i++) {
            for (let j = k + 1; j < N; j++)
                A[i][j] = (A[i][j] * akk - A[i][k] * A[k][j]) / prev;
        }
        prev = akk;
    }
    return { perm, swaps };
}
function LUSolveBareissMulti(A, X, perm) {
    const N = A.length;
    const R = X.length;
    const inv = Array.from({ length: N }, (_, k) => 1 / A[k][k]);
    // Forward: solve L * Y = B  (lower-triangular)
    for (let r = 0; r < R; ++r)
        X[0][r] *= inv[0];
    for (let i = 0; i < N; ++i) {
        const Ai = A[i];
        const rAii = (i > 0 ? A[i - 1][i - 1] : 1) / Ai[i];
        for (let r = 0; r < R; ++r) {
            let s = Ai[0] * X[0][r];
            for (let k = 1; k < i; ++k)
                s += Ai[k] * X[k][r] * inv[k - 1];
            X[i][r] = (X[i][r] - s) * rAii;
        }
    }
    // Backward: solve U * X = Y  (upper-triangular)
    for (let i = N - 2; i >= 0; --i) {
        const Ai = A[i];
        const rAii = 1 / Ai[i];
        for (let r = 0; r < R; ++r) {
            let s = 0;
            for (let k = i + 1; k < N; ++k)
                s += Ai[k] * X[k][r];
            X[i][r] = X[i][r] - s * rAii;
        }
    }
    if (perm) {
        const invperm = [];
        for (let i = 0; i < N; i++)
            invperm[perm[i]] = i;
        return X.map(x => invperm.map(i => x[i]));
    }
    return X;
}
// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length N).
function LUSolveBareissTransposeMulti(A, X, perm) {
    const N = A.length;
    const R = X.length;
    const invPrev = Array.from({ length: N }, (_, k) => (k > 0 ? 1 / A[k - 1][k - 1] : 1));
    // Forward: solve U^T * Y = B  (lower-triangular)
    for (let i = 0; i < N; ++i) {
        const rAii = (i > 0 ? A[i - 1][i - 1] : 1) / A[i][i];
        for (let r = 0; r < R; ++r) {
            const x = X[r];
            let s = 0;
            for (let k = 0; k < i; ++k)
                s += A[k][i] * x[k] * invPrev[k];
            x[i] = (x[i] - s) * rAii;
        }
    }
    // Backward: solve L^T * X = Y  (upper-triangular, unit diagonal)
    for (let i = N - 1; i >= 0; --i) {
        const Aii = A[i][i];
        for (let r = 0; r < R; ++r) {
            const x = X[r];
            let s = 0;
            for (let k = i + 1; k < N; ++k)
                s += A[k][i] * x[k];
            x[i] = x[i] - s / Aii;
        }
    }
    if (perm) {
        const invperm = [];
        for (let i = 0; i < N; i++)
            invperm[perm[i]] = i;
        return X.map(x => invperm.map(i => x[i]));
    }
    return X;
}
function LUDecomposeBareissT(A, pivot = true) {
    const N = A.length;
    const perm = Array.from({ length: N }, (_, i) => i);
    let prev; //	= A[0][0].from(1);
    let swaps = 0;
    for (let k = 0; k < N - 1; k++) {
        if (pivot && A[k][k].sign() === 0) {
            let swap = k + 1;
            while (swap < N && A[swap][k].sign() === 0)
                swap++;
            if (swap === N)
                return { perm, swaps };
            [A[k], A[swap]] = [A[swap], A[k]];
            [perm[k], perm[swap]] = [perm[swap], perm[k]];
            swaps++;
        }
        const akk = A[k][k];
        for (let i = k + 1; i < N; i++) {
            for (let j = k + 1; j < N; j++)
                A[i][j] = (prev ? A[i][j].div(prev) : A[i][j]).mul(akk).sub(A[i][k].mul(A[k][j]));
        }
        prev = akk;
    }
    return { perm, swaps };
}
// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length N).
function LUSolveBareissMultiT(A, X, perm) {
    const N = A.length;
    const R = X.length;
    // Precompute invPrev[k] = recip(prev_k) where prev_k = k>0 ? A[k-1][k-1] : one
    const inv = Array.from({ length: N }, (_, k) => A[k][k].recip());
    // Forward: solve L * Y = B  (lower-triangular)
    for (let r = 0; r < R; ++r)
        X[0][r] = X[0][r].mul(inv[0]);
    for (let i = 1; i < N; ++i) {
        const Ai = A[i];
        const rAii = A[i - 1][i - 1].div(Ai[i]);
        for (let r = 0; r < R; ++r) {
            let s = Ai[0].mul(X[0][r]);
            for (let k = 1; k < i; ++k)
                s = s.add(Ai[k].mul(X[k][r]).mul(inv[k - 1]));
            X[i][r] = X[i][r].sub(s).mul(rAii);
        }
    }
    // Backward: solve U * X = Y  (upper-triangular) skip the i = N-1 iteration (it has no terms to sum)
    for (let i = N - 2; i >= 0; --i) {
        const Ai = A[i];
        const rAii = inv[i];
        for (let r = 0; r < R; ++r) {
            let s = Ai[i + 1].mul(X[i + 1][r]);
            for (let k = i + 2; k < N; ++k)
                s = s.add(Ai[k].mul(X[k][r]));
            X[i][r] = X[i][r].sub(s.mul(rAii));
        }
    }
    if (perm) {
        const invperm = [];
        for (let i = 0; i < N; i++)
            invperm[perm[i]] = i;
        return X.map(x => invperm.map(i => x[i]));
    }
    return X;
}
// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length N).
function LUSolveBareissTransposeMultiT(A, X, perm) {
    const N = A.length;
    const R = X.length;
    // Precompute invPrev[k] = recip(prev_k) where prev_k = k>0 ? A[k-1][k-1] : one
    const inv = Array.from({ length: N }, (_, k) => A[k][k].recip());
    // Forward: solve U^T * Y = B  (lower-triangular)
    for (let r = 0; r < R; ++r)
        X[r][0] = X[r][0].mul(inv[0]);
    for (let i = 1; i < N; ++i) {
        const rAii = A[i - 1][i - 1].div(A[i][i]);
        for (let r = 0; r < R; ++r) {
            let s = A[0][i].mul(X[r][0]);
            for (let k = 1; k < i; ++k)
                s = s.add(A[k][i].mul(X[r][k]).mul(inv[k - 1]));
            X[r][i] = X[r][i].sub(s).mul(rAii);
        }
    }
    // Backward: solve L^T * X = Y  (upper-triangular) skip the i = N-1 iteration (it has no terms to sum)
    for (let i = N - 2; i >= 0; --i) {
        const rAii = inv[i];
        for (let r = 0; r < R; ++r) {
            let s = A[i + 1][i].mul(X[r][i + 1]);
            for (let k = i + 2; k < N; ++k)
                s = s.add(A[k][i].mul(X[r][k]));
            X[r][i] = X[r][i].sub(s.mul(rAii));
        }
    }
    if (perm) {
        const invperm = [];
        for (let i = 0; i < N; i++)
            invperm[perm[i]] = i;
        return X.map(x => invperm.map(i => x[i]));
    }
    return X;
}
// Solve rectangular system A * x = B for exact scalar types T.
// A is m x n, B is array of column vectors length m (column-major): B.length === R, B[r].length === m
// Returns array of column vectors length n (one column per RHS) when a unique solution exists (full column rank and consistent), otherwise undefined.
function solveRectangularBareiss(A, B) {
    const m = A.length;
    if (m === 0)
        return;
    const n = A[0].length;
    if (A.some(row => row.length !== n) || B.some(row => row.length !== m))
        return undefined;
    const R = B.length;
    // augmented matrix m x (n+R)
    A = Array.from({ length: m }, (_, i) => Array.from({ length: n + R }, (_, j) => j < n ? A[i][j] : B[j - n][i]));
    let row = 0;
    const pivotCol = [];
    for (let col = 0; col < n && row < m; col++) {
        let pivot = -1;
        for (let r = row; r < m; r++) {
            if (A[r][col] !== 0) {
                pivot = r;
                break;
            }
        }
        if (pivot !== -1) {
            if (pivot !== row)
                [A[row], A[pivot]] = [A[pivot], A[row]];
            const piv = A[row][col];
            for (let j = col; j < n + R; j++)
                A[row][j] = A[row][j] / piv;
            for (let r = 0; r < m; r++) {
                if (r !== row) {
                    const factor = A[r][col];
                    if (factor !== 0) {
                        for (let j = col; j < n + R; j++)
                            A[r][j] = A[r][j] - A[row][j] * factor;
                    }
                }
            }
            pivotCol.push(col);
            row++;
        }
    }
    const rank = pivotCol.length;
    for (let r = rank; r < m; r++) {
        if (A[r].some(i => i === 0)) {
            for (let rr = 0; rr < R; rr++) {
                if (A[r][n + rr] !== 0)
                    return undefined;
            }
        }
    }
    if (rank !== n)
        return undefined;
    const sol = Array.from({ length: R }, () => new Array());
    for (let idx = 0; idx < pivotCol.length; idx++) {
        const c = pivotCol[idx];
        for (let rr = 0; rr < R; rr++)
            sol[rr][c] = A[idx][n + rr];
    }
    return sol;
}
function solveRectangularBareissT(A, B) {
    const m = A.length;
    if (m === 0)
        return;
    const n = A[0].length;
    if (A.some(row => row.length !== n) || B.some(row => row.length !== m))
        return undefined;
    const R = B.length;
    // augmented matrix m x (n+R)
    A = Array.from({ length: m }, (_, i) => Array.from({ length: n + R }, (_, j) => j < n ? A[i][j] : B[j - n][i]));
    let row = 0;
    const pivotCol = [];
    for (let col = 0; col < n && row < m; col++) {
        let pivot = -1;
        for (let r = row; r < m; r++) {
            if (A[r][col].sign() !== 0) {
                pivot = r;
                break;
            }
        }
        if (pivot !== -1) {
            if (pivot !== row)
                [A[row], A[pivot]] = [A[pivot], A[row]];
            const piv = A[row][col];
            for (let j = col; j < n + R; j++)
                A[row][j] = A[row][j].div(piv);
            for (let r = 0; r < m; r++) {
                if (r !== row) {
                    const factor = A[r][col];
                    if (factor.sign() !== 0) {
                        for (let j = col; j < n + R; j++)
                            A[r][j] = A[r][j].sub(A[row][j].mul(factor));
                    }
                }
            }
            pivotCol.push(col);
            row++;
        }
    }
    const rank = pivotCol.length;
    for (let r = rank; r < m; r++) {
        if (A[r].some(i => i.sign() === 0)) {
            for (let rr = 0; rr < R; rr++) {
                if (A[r][n + rr].sign() !== 0)
                    return undefined;
            }
        }
    }
    if (rank !== n)
        return undefined;
    const sol = Array.from({ length: R }, () => new Array());
    for (let idx = 0; idx < pivotCol.length; idx++) {
        const c = pivotCol[idx];
        for (let rr = 0; rr < R; rr++)
            sol[rr][c] = A[idx][n + rr];
    }
    return sol;
}
//# sourceMappingURL=bareiss.js.map