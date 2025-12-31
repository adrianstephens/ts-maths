import { ops1, hasop, builtinNumber } from './core';

//-----------------------------------------------------------------------------
// Vector/Matrix helpers
//-----------------------------------------------------------------------------

function scaleVecN<T extends builtinNumber>(A: T[], b: T): T[] {
	return A.map(a => a * b) as T[];
}
function scaleVecT<T extends hasop<'mul'>>(A: T[], b: T): T[] {
	return A.map(a => a.mul(b));
}

function mulVecN<T extends builtinNumber>(A: T[][], b: T[]): T[] {
	const r: T[] = [];
	for (const i in b) {
		const a = scaleVecN(A[i], b[i]);
		for (const j in a)
			r[j] = r[j] ? r[j] + (a[j] as any) : a[j];
	}
	return r;
}
function mulVecT<T extends hasop<'mul'|'add'>>(A: T[][], b: T[]): T[] {
	const r: T[] = [];
	for (const i in b) {
		const a = scaleVecT(A[i], b[i]);
		for (const j in a)
			r[j] = r[j] ? r[j].add(a[j]) : a[j];
	}
	return r;
}

export function matMulN<T extends builtinNumber>(A: T[][], B: T[][]): T[][] {
	return B.map(b => mulVecN(A, b));
}

export function matMulT<T extends hasop<'add'|'mul'>>(A: T[][], B: T[][]): T[][] {
	return B.map(b => mulVecT(A, b));
}

export function traceN<T extends builtinNumber>(A: T[][]): T {
	const N = A.length;
	let s = A[0][0];
	for (let i = 1; i < N; i++)
		s += A[i][i] as any;
	return s;
}
export function traceT<T extends hasop<'add'>>(A: T[][]): T {
	const N = A.length;
	let s = A[0][0];
	for (let i = 1; i < N; i++)
		s = s.add(A[i][i]);
	return s;
}

export function characteristicN<T extends builtinNumber>(A: T[][]): T[] {
	const n = A.length;
	const B = A.map(col => col.slice());
	const coeffs: T[] = [];

	for (let k = 1; k < n; ++k) {
		const ck	= -traceN(B) / k;
		coeffs.push(ck as any);

		for (let i = 0; i < n; i++) {
			B[i][i] += ck as any;
			B[i] = mulVecN(A, B[i]);
		}
	}
	coeffs.push(-traceN(B) / n as any);
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

export function matPowN<T extends builtinNumber>(A: T[][], e: number): T[][] {
	if (e === 0) {
		return (typeof A[0][0] === 'number'
			? A.map((row, i) => row.map((_, j) => i === j ? 1 : 0))
			: A.map((row, i) => row.map((_, j) => i === j ? 1n : 0n))
		 ) as T[][];
	}
    let R = e & 1 ? A : undefined;
	for (e >>= 1; e; e >>= 1) {
		A = matMulN(A, A);
		if (e & 1)
			R = R ? matMulN(R, A) : A;
	}
    return R!;
}

export function matPow<T extends hasop<'add'|'mul'>>(A: T[][], e: number): T[][] {
	if (e === 0) {
		const c = A[0][0];
		if (hasop('from')(c)) {
			const c0 = c.from(0), c1 = c.from(1);
			return A.map((row, i) => row.map((_, j) => i === j ? c1 : c0));
		}
		throw new Error('matPow: cannot create identity matrix for this type');
	}
    let R = e & 1 ? A : undefined;
	for (e >>= 1; e; e >>= 1) {
		A = matMulT(A, A);
		if (e & 1)
			R = R ? matMulT(R, A) : A;
	}
    return R!;
}

// Evaluate an integer-coefficient polynomial at a matrix given in column-major form.
export function evaluateIntegerPolyAtMatrix<T extends hasop<'add'|'mul'>>(poly: number[], M: T[][], from: (n: builtinNumber) => T): T[][] {
	const size	= M.length;
	const zero	= from(0), c0 = from(poly[0] ?? 0);
	const R		= Array.from({ length: size }, (_, i) => Array.from({ length: size }, (_, j) => i === j ? c0 : zero));

	let Mpow = M, lastPow = 1;

	for (const k in poly) {
		const pow = +k;
		if (pow === 0)
			continue;

		while (lastPow < pow) {
			Mpow = matMulT(Mpow, M);
			++lastPow;
		}

		const ck = from(poly[k]);
		for (const i in Mpow) {
			const r = R[i];
			const Mcol = Mpow[i];
			for (const j in Mcol)
				r[j] = r[j].add(Mcol[j].mul(ck));
		}
	}

	return R;
}

// Project matrix M (column-major) into the invariant subspace spanned by `nsCols` (array of column vectors).
// Returns the small m x m matrix M_sub with columns expressed in the basis `nsCols`.
export function projectSubspace<T extends ops1<T> & hasop<'recip'|'sign'>>(Mcol: T[][], nsCols: T[][], zero: T): T[][] {
	const N			= Mcol.length;
	const m			= nsCols.length;
	// build T_rows as N x m matrix where column j is nsCols[j]
	const T_rows	= Array.from({ length: N }, (_, row) => Array.from({ length: m }, (_, col) => nsCols[col][row] ?? zero));
	const M_sub		= Array.from({ length: m }, () => Array.from({ length: m }, () => zero));

	for (let j = 0; j < m; j++) {
		// w = M * nsCols[j]
		const w: T[] = Array.from({ length: N }, () => zero);
		for (let row = 0; row < N; row++) {
			let sum = zero;
			for (let col = 0; col < N; col++) {
				const mv = Mcol[col][row];
				if (mv === undefined)
					continue;
				const bv = nsCols[j][col] ?? zero;
				if ((bv as any).sign && (bv as any).sign() === 0)
					continue;
				sum = sum.add(mv.mul(bv));
			}
			w[row] = sum;
		}
		const sol = solveRectangularBareissT(T_rows, [w]);
		if (!sol)
			continue;
		const ccol = sol[0];
		for (let i = 0; i < m; i++)
			M_sub[i][j] = ccol[i];
	}
	return M_sub;
}


export function transpose<T>(A: T[][]): T[][] {
	const R: T[][] = [];
	for (const i in A) {
		const a = A[i];
		for (const j in a)
			(R[j] ??= [])[i] = a[j];
	}
	return R;
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

export function LUDecomposeBareissT<T extends ops1<T> & hasop<'sign'>>(A: T[][], pivot = true) {
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

export function LUSolveBareissMultiT<T extends ops1<T> & hasop<'recip'>>(A: T[][], X: T[][], perm?: number[]) {
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

export function LUSolveBareissTransposeMultiT<T extends ops1<T> & hasop<'recip'>>(A: T[][], X: T[][], perm?: number[]) {
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

export function solveRectangularBareissT<T extends ops1<T> & hasop<'recip'|'sign'>>(A: T[][], B: T[][]): T[][] | undefined {
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
		// If the coefficient part of the row (first n entries) is all zero but the RHS (augmented columns) is non-zero, the system is inconsistent.
		// Previously this checked `some` zeros which was incorrect; we must test that all coefficient entries are zero.
		if (A[r].slice(0, n).every(i => i.sign() === 0)) {
			for (let rr = 0; rr < R; rr++) {
				if (A[r][n + rr].sign() !== 0)
					return undefined;
			}
		}
	}
	// If rank < n there are free variables and infinitely many solutions.
	// We return one particular solution by setting all non-pivot (free) variables to zero (constructed as `x.sub(x)`), and filling pivot variables from the reduced augmented matrix
	// This mirrors the numeric solver's behavior but avoids outright failure on underdetermined but consistent systems.
	const zero = A[0][0].sub(A[0][0]);
	const sol = Array.from({ length: R }, () => Array.from({ length: n }, () => zero));
	for (let idx = 0; idx < pivotCol.length; idx++) {
		const c = pivotCol[idx];
		for (let rr = 0; rr < R; rr++)
			sol[rr][c] = A[idx][n + rr];
	}
	return sol;
}

// Compute a nullspace basis for A (column vectors) for exact scalar types T.
// Returns an array of column vectors (each length n) forming a basis for the nullspace.
// mutates A
export function nullspaceRowT<T extends ops1<T> & hasop<'recip'|'sign'|'from'>>(A: T[][]): T[][] {
	const m = A.length;
	if (m === 0)
		return [];
	const n = A[0].length;

	const pivots: number[] = [];
	let row = 0;
	for (let col = 0; col < n && row < m; col++) {
		let r = row;
		while (r < m && A[r][col].sign() === 0)
			r++;
		if (r === m)
			continue;

		if (r !== row)
			[A[row], A[r]] = [A[r], A[row]];

		pivots.push(col);
		const piv = A[row][col];
		for (let j = col; j < n; j++)
			A[row][j] = A[row][j].div(piv);

		for (let i = 0; i < m; i++) {
			if (i !== row && A[i][col].sign()) {
				const f = A[i][col];
				for (let j = col; j < n; j++)
					A[i][j] = A[i][j].sub(A[row][j].mul(f));
			}
		}
		row++;
	}

	if (pivots.length === 0)
		return [];

	const pivotSet = new Set(pivots);
	const freeCols = Array.from({ length: n }, (_, i) => i).filter(i => !pivotSet.has(i));
	if (freeCols.length === 0)
		return [];

	const U = pivots.map((_, r) => pivots.map(c => A[r][c]));
	const { perm } = LUDecomposeBareissT(U, true);
	
	const one	= A[0][0].from(1);
	return LUSolveBareissTransposeMultiT(U, freeCols.map(fc => pivots.map((_, i) => A[i][fc].neg())), perm).map((xpiv, idx) => {
		const x: T[] = [];
		x[freeCols[idx]] = one;
		for (let i = 0; i < pivots.length; i++)
			x[pivots[i]] = xpiv[i];
		return x;
	});
}

// Column-major nullspace: accepts A as array of columns (each column length m)
// Returns an array of column vectors (each length n) forming a basis for the nullspace.
// mutates A
export function nullspaceColT<T extends ops1<T> & hasop<'recip'|'sign'|'from'>>(A: T[][]): T[][] {
	const n = A.length; // number of columns
	if (n === 0)
		return [];
	const m = A[0].length; // number of rows

	const pivotCols: number[] = [];
	const pivotRows: number[] = [];

	// Select pivots without moving heavy data: pick an unused column with a non-zero at the current row.
	const usedCols = Array.from({ length: n }, () => false);
	for (let row = 0; row < m && pivotCols.length < n; row++) {
		let col = -1;
		for (let k = 0; k < n; k++) {
			if (!usedCols[k] && A[k][row].sign() !== 0) {
				col = k;
				break;
			}
		}
		if (col === -1)
			continue;

		usedCols[col] = true;
		pivotCols.push(col);
		pivotRows.push(row);

		const piv = A[col][row];
		// normalize this pivot row across all columns
		for (let j = 0; j < n; j++)
			A[j][row] = A[j][row].div(piv);

		// eliminate other rows using this pivot
		for (let i = 0; i < m; i++) {
			if (i !== row && A[col][i].sign()) {
				const f = A[col][i];
				for (let j = 0; j < n; j++)
					A[j][i] = A[j][i].sub(A[j][row].mul(f));
			}
		}
	}

	if (pivotCols.length === 0)
		return [];

	const freeCols = Array.from({ length: n }, (_, i) => i).filter(i => !usedCols[i]);
	if (freeCols.length === 0)
		return [];

	// Build U from pivot columns restricted to the recorded pivotRows
	const U = pivotRows.map(r => pivotCols.map(c => A[c][r]));
	const { perm } = LUDecomposeBareissT(U, true);

	const one = A[0][0].from(1);
	return LUSolveBareissTransposeMultiT(U, freeCols.map(fc => pivotRows.map(pr => A[fc][pr].neg())), perm).map((xpiv, idx) => {
		const out: T[] = [];
		out[freeCols[idx]] = one;
		for (let i = 0; i < pivotCols.length; i++)
			out[pivotCols[i]] = xpiv[i];
		return out;
	});
}
