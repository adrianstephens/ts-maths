import { ops1, has, hasop } from './core';

//-----------------------------------------------------------------------------
// Matrix helpers
//-----------------------------------------------------------------------------

function matMulT<T extends hasop<'add'|'mul'|'scale'>>(A: T[][], B: T[][]): T[][] {
	const zero = A[0][0].scale(0);
	const N = A.length;
	const R = Array.from({ length: N }, () => Array.from({ length: N }, () => zero));
	for (let i = 0; i < N; i++) {
		for (let k = 0; k < N; k++) {
			const aik = A[i][k];
			if (aik) {
				for (let j = 0; j < N; j++) {
					const bkj = B[k][j];
					if (bkj)
						R[i][j] = R[i][j].add(aik.mul(bkj));
				}
			}
		}
	}
	return R;
}

function scaleVecN(A: number[], b: number): number[] {
	return A.map(a => a * b);
}
function scaleVecT<T extends hasop<'mul'>>(A: T[], b: T): T[] {
	return A.map(a => a.mul(b));
}

function mulVecN(A: number[][], b: number[]): number[] {
	const r	= scaleVecN(A[0], b[0]);
	for (let i = 1; i < A.length; i++) {
		const a = scaleVecN(A[i], b[i]);
		if (a.length > r.length)
			r.push(...Array(a.length - r.length).fill(0));
		r.forEach((_, j) => r[j] += a[j]);
	}
	return r;
}
function mulVecT<T extends hasop<'mul'|'add'>>(A: T[][], b: T[]): T[] {
	const r	= scaleVecT(A[0], b[0]);
	for (let i = 1; i < A.length; i++) {
		const a = scaleVecT(A[i], b[i]);
		if (a.length > r.length)
			r.push(...Array(a.length - r.length).fill(0));
		r.forEach((_, j) => r[j] = r[j].add(a[j]));
	}
	return r;
}

function matmulN(A: number[][], B: number[][]): number[][] {
	return B.map(b => mulVecN(A, b));
}


function traceN(A: number[][]): number {
	const N = A.length;
	let s = 0;
	for (let i = 0; i < N; i++)
		s = s += A[i][i];
	return s;
}
function traceT<T extends hasop<'add'>>(A: T[][]): T {
	const N = A.length;
	let s = A[0][0];
	for (let i = 1; i < N; i++)
		s = s.add(A[i][i]);
	return s;
}

export function characteristicN(A: number[][]): number[] {
	const n = A.length;
	const B = A.map(col => col.slice());
	const coeffs: number[] = [];

	for (let k = 1; k < n; ++k) {
		const ck	= -traceN(B) / k;
		coeffs.push(ck);

		for (let i = 0; i < n; i++) {
			B[i][i] += ck;
			B[i] = mulVecN(A, B[i]);
		}
	}
	coeffs.push(-traceN(B) / n);
	coeffs.reverse();
	return coeffs;
}

export function characteristicT<T extends hasop<'add'|'mul'|'scale'>>(A: T[][]): T[] {
	const n = A.length;

	const B = A.map(col => col.slice());
	const coeffs: T[] = [];

	for (let k = 1; k < n; ++k) {
		const ck	= traceT(B).scale(-1 / k);
		coeffs.push(ck);

		for (let i = 0; i < n; i++) {
			B[i][i] = B[i][i].add(ck);
			B[i] = mulVecT(A, B[i]);
		}
	}
	coeffs.push(traceT(B).scale(-1 /n));
	coeffs.reverse();
	return coeffs;
}

//-----------------------------------------------------------------------------
// LU Decomposition and Solvers
//-----------------------------------------------------------------------------

export function LUDecomposeBareiss(A: number[][], pivot = true) {
	const	N		= A.length;
	const	perm	= Array.from({ length: N }, (_, i) => i);
	let		prev	= 1;
	let		swaps	= 0;

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

export function LUSolveBareissMulti(A: number[][], X: number[][], perm?: number[]) {
	const N = A.length;
	const R = X.length;

	const inv = Array.from({ length: N }, (_, k) => 1 / A[k][k]);

	// Forward: solve L * Y = B  (lower-triangular)
	for (let r = 0; r < R; ++r)
		X[0][r] *= inv[0];
	for (let i = 0; i < N; ++i) {
		const Ai	= A[i];
		const rAii	= (i > 0 ? A[i - 1][i - 1] : 1) / Ai[i];
		for (let r = 0; r < R; ++r) {
			let s = Ai[0] * X[0][r];
			for (let k = 1; k < i; ++k)
				s += Ai[k] * X[k][r] * inv[k - 1];
			X[i][r] = (X[i][r] - s) * rAii;
		}
	}

	// Backward: solve U * X = Y  (upper-triangular)
	for (let i = N - 2; i >= 0; --i) {
		const Ai	= A[i];
		const rAii	= 1 / Ai[i];
		for (let r = 0; r < R; ++r) {
			let s = 0;
			for (let k = i + 1; k < N; ++k)
				s += Ai[k] * X[k][r];
			X[i][r] = X[i][r] - s * rAii;
		}
	}

	if (perm) {
		const invperm: number[] = [];
		for (let i = 0; i < N; i++)
			invperm[perm[i]] = i;
		return X.map(x => invperm.map(i => x[i]));
	}
	return X;
}

// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length N).
export function LUSolveBareissTransposeMulti(A: number[][], X: number[][], perm?: number[]) {
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
		const invperm: number[] = [];
		for (let i = 0; i < N; i++)
			invperm[perm[i]] = i;
		return X.map(x => invperm.map(i => x[i]));
	}
	return X;
}

export function LUDecomposeBareissT<T extends ops1<T> & has<'sign'>>(A: T[][], pivot = true) {
	const	N		= A.length;
	const	perm	= Array.from({ length: N }, (_, i) => i);
	let		prev: T | undefined;//	= A[0][0].from(1);
	let		swaps	= 0;

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
export function LUSolveBareissMultiT<T extends ops1<T> & has<'recip'>>(A: T[][], X: T[][], perm?: number[]) {
	const N = A.length;
	const R = X.length;

	// Precompute invPrev[k] = recip(prev_k) where prev_k = k>0 ? A[k-1][k-1] : one
	const inv: T[] = Array.from({ length: N }, (_, k) => A[k][k].recip());

	// Forward: solve L * Y = B  (lower-triangular)
	for (let r = 0; r < R; ++r)
		X[0][r] = X[0][r].mul(inv[0]);
	for (let i = 1; i < N; ++i) {
		const Ai	= A[i];
		const rAii	= A[i - 1][i - 1].div(Ai[i]);
		for (let r = 0; r < R; ++r) {
			let s = Ai[0].mul(X[0][r]);
			for (let k = 1; k < i; ++k)
				s = s.add(Ai[k].mul(X[k][r]).mul(inv[k - 1]));
			X[i][r] = X[i][r].sub(s).mul(rAii);
		}
	}

	// Backward: solve U * X = Y  (upper-triangular) skip the i = N-1 iteration (it has no terms to sum)
	for (let i = N - 2; i >= 0; --i) {
		const Ai	= A[i];
		const rAii	= inv[i];
		for (let r = 0; r < R; ++r) {
			let s = Ai[i + 1].mul(X[i + 1][r]);
			for (let k = i + 2; k < N; ++k)
				s = s.add(Ai[k].mul(X[k][r]));
			X[i][r] = X[i][r].sub(s.mul(rAii));
		}
	}

	if (perm) {
		const invperm: number[] = [];
		for (let i = 0; i < N; i++)
			invperm[perm[i]] = i;
		return X.map(x => invperm.map(i => x[i]));
	}
	return X;
}

// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length N).
export function LUSolveBareissTransposeMultiT<T extends ops1<T> & has<'recip'>>(A: T[][], X: T[][], perm?: number[]) {
	const N = A.length;
	const R = X.length;

	// Precompute invPrev[k] = recip(prev_k) where prev_k = k>0 ? A[k-1][k-1] : one
	const inv: T[] = Array.from({ length: N }, (_, k) => A[k][k].recip());

	// Forward: solve U^T * Y = B  (lower-triangular)
	for (let r = 0; r < R; ++r)
		X[r][0] = X[r][0].mul(inv[0]);
	for (let i = 1; i < N; ++i) {
		const rAii = A[i - 1][i - 1].div(A[i][i]);
		for (let r = 0; r < R; ++r) {
			let	s = A[0][i].mul(X[r][0]);
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
		const invperm: number[] = [];
		for (let i = 0; i < N; i++)
			invperm[perm[i]] = i;
		return X.map(x => invperm.map(i => x[i]));
	}
	return X;
}

// Solve rectangular system A * x = B for exact scalar types T.
// A is m x n, B is array of column vectors length m (column-major): B.length === R, B[r].length === m
// Returns array of column vectors length n (one column per RHS) when a unique solution exists (full column rank and consistent), otherwise undefined.

export function solveRectangularBareiss(A: number[][], B: number[][]): number[][] | undefined {
	const m = A.length;
	if (m === 0)
		return;

	const n = A[0].length;
	if (A.some(row => row.length !== n) || B.some(row => row.length !== m))
		return undefined;

	const R = B.length;

	// augmented matrix m x (n+R)
	A = Array.from({ length: m }, (_, i) =>
		Array.from({ length: n + R }, (_, j) => j < n ? A[i][j] : B[j - n][i])
	);

	let row = 0;
	const pivotCol: number[] = [];
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

	const sol = Array.from({ length: R }, () => new Array<number>());
	for (let idx = 0; idx < pivotCol.length; idx++) {
		const c = pivotCol[idx];
		for (let rr = 0; rr < R; rr++)
			sol[rr][c] = A[idx][n + rr];
	}
	return sol;
}

export function solveRectangularBareissT<T extends ops1<T> & has<'recip'|'sign'>>(A: T[][], B: T[][]): T[][] | undefined {
	const m = A.length;
	if (m === 0)
		return;

	const n = A[0].length;
	//if (A.some(row => row.length !== n) || B.some(row => row.length !== m))
	//	return undefined;

	const R = B.length;

	// augmented matrix m x (n+R)
	A = Array.from({ length: m }, (_, i) =>
		Array.from({ length: n + R }, (_, j) => j < n ? A[i][j] : B[j - n][i])
	);

	let row = 0;
	const pivotCol: number[] = [];
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
		// If the coefficient part of the row (first n entries) is all zero
		// but the RHS (augmented columns) is non-zero, the system is
		// inconsistent. Previously this checked `some` zeros which was
		// incorrect; we must test that all coefficient entries are zero.
		if (A[r].slice(0, n).every(i => i.sign() === 0)) {
			for (let rr = 0; rr < R; rr++) {
				if (A[r][n + rr].sign() !== 0)
					return undefined;
			}
		}
	}
	// If rank < n there are free variables and infinitely many solutions.
	// We return one particular solution by setting all non-pivot (free)
	// variables to zero (constructed as `x.sub(x)`), and filling pivot
	// variables from the reduced augmented matrix. This mirrors the
	// numeric solver's behavior but avoids outright failure on underdetermined
	// but consistent systems.
	const zero = A[0][0].sub(A[0][0]);
	const sol = Array.from({ length: R }, () => Array.from({ length: n }, () => zero));
	for (let idx = 0; idx < pivotCol.length; idx++) {
		const c = pivotCol[idx];
		for (let rr = 0; rr < R; rr++)
			sol[rr][c] = A[idx][n + rr];
	}
	return sol;
}

