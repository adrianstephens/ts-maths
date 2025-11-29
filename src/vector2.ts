/* eslint-disable no-restricted-syntax */
import { scalar } from './core';
import complex from './complex';
import { PolynomialN } from './polynomial';
import { vscalar, vec, vops } from './vector';

//-----------------------------------------------------------------------------
// floatN -- Numeric vector of arbitrary length with basic vector operations
//-----------------------------------------------------------------------------

export class floatN extends Array<number> implements vops<floatN, number> {
	constructor(...args: number[]) {
		if (args.length === 1) {
			super();
			this.push(args[0]);
		} else {
			super(...args);
		}
	}
	create(...args: number[]): floatN {
		return new floatN(...args);
	}
	get _values(): number[] { return this; }

	static from<T extends Iterable<number> | ArrayLike<number>>(input: T, mapFn?: (v: number, i: number) => number, thisArg?: any): floatN {
		const a = Array.from(input as any, mapFn as any, thisArg);
		Object.setPrototypeOf(a, floatN.prototype);
		return a as floatN;
	}
	static zeros(n: number) {
		return new floatN(...Array(n).fill(0));
	}
	static fromArray(v: number[]) {
		return new floatN(...v);
	}
	static fromVec(v: vec<number, string>) {
		return this.fromArray(Object.values(v));
	}

	dup() 					{ return new floatN(...this); }
	neg() 					{ return new floatN(...this.map(x => -x)); }
	abs() 					{ return new floatN(...this.map(x => Math.abs(x))); }
	scale(b: number) 		{ return new floatN(...this.map(x => x * b)); }
	mul(b: floatN) 			{ return new floatN(...this.map((x, i) => x * b[i])); }
	div(b: floatN) 			{ return new floatN(...this.map((x, i) => x / b[i])); }
	add(b: floatN) 			{ return new floatN(...this.map((x, i) => x + b[i])); }
	sub(b: floatN) 			{ return new floatN(...this.map((x, i) => x - b[i])); }
	min(b: floatN) 			{ return new floatN(...this.map((x, i) => Math.min(x, b[i]))); }
	max(b: floatN) 			{ return new floatN(...this.map((x, i) => Math.max(x, b[i]))); }
	eq(b: floatN) 			{ return this.length === b.length && this.every((v, i) => v === b[i]); }
	dot(b: floatN) 			{ return this.reduce((acc, v, i) => acc + v * b[i], 0); }
	perp() 					{
		const i = this.reduce((minI, c, k) => Math.abs(c) < Math.abs(this[minI]) ? k : minI, 0);
		const x = this[i] / this.lensq();
		return this.create(...this.map((c, j) => (j === i ? 1 : 0) - x * c));
	}

	lensq() 				{ return this.dot(this); }
	len() 					{ return Math.sqrt(this.lensq()); }
	mag()					{ return this.len(); }
	selfScale(b: number) 	{ for (const i in this) this[i] *= b; }
	selfMul(b: floatN) 		{ for (const i in this) this[i] *= b[i]; }
	selfAdd(b: floatN) 		{ for (const i in this) this[i] += b[i]; }
	selfSub(b: floatN) 		{ for (const i in this) this[i] -= b[i]; }
	clamp(min: floatN, max: floatN) 	{ return this.max(min).min(max); }

	toString() 				{ return '('+this.join(', ')+')'; }
	[Symbol.for("debug.description")]() { return this.toString(); }
}


function mulN(A: floatN[], b: floatN): floatN {
	const r	= A[0].scale(b[0]);
	for (let i = 1; i < A.length; i++) {
		const a = A[i].scale(b[i]);
		if (a.length > r.length)
			r.push(...Array(a.length - r.length).fill(0));
		r.selfAdd(a);
	}
	return r;
}

function matmulN(A: floatN[], B: floatN[]): floatN[] {
	return B.map(b => mulN(A, b));
}

//-----------------------------------------------------------------------------
// Wedge
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Modified Gram-Schmidt. Returns Q (array of floatNs) and R (numeric m x m upper-triangular)
//-----------------------------------------------------------------------------

function QR(cols: floatN[]) {
	const n = cols.length;
	const Q: floatN[] = [];
	const R = cols.map((v, j) => {
		const Rj: number[] = [];
		for (let i = 0; i < j; ++i) {
			const rij = Q[i].dot(v);
			Rj[i] = rij;
			v.selfSub(Q[i].scale(rij));
		}
		const norm = v.len();
		Rj[j] = norm;
		Q[j] = norm ? v.scale(1 / norm) : floatN.zeros(n);
		return floatN.fromArray(Rj);
	});
	return { Q, R };
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

	const invPrev = Array.from({ length: N }, (_, k) => (k > 0 ? 1 / A[k - 1][k - 1] : 1));

	// Forward: solve L * Y = B  (lower-triangular)
	for (let i = 0; i < N; ++i) {
		const Ai	= A[i];
		const rAii	= (i > 0 ? A[i - 1][i - 1] : 1) / Ai[i];
		for (let r = 0; r < R; ++r) {
			let s = 0;
			for (let k = 0; k < i; ++k)
				s += Ai[k] * X[k][r] * invPrev[k];
			X[i][r] = (X[i][r] - s) * rAii;
		}
	}

	// Backward: solve U * X = Y  (upper-triangular)
	for (let i = N - 1; i >= 0; --i) {
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

/*
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
*/
export function LUDecomposeBareissT<T extends scalar<T>>(A: T[][], pivot = true) {
	const	N		= A.length;
	const	perm	= Array.from({ length: N }, (_, i) => i);
	let		prev	= A[0][0].from(1);
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
				A[i][j] = A[i][j].mul(akk).sub(A[i][k].mul(A[k][j])).div(prev);
		}
		prev = akk;
	}
	return { perm, swaps };
}

// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length N).
export function LUSolveBareissMultiT<T extends vscalar<T>>(A: T[][], X: T[][], perm?: number[]) {
	const N = A.length;
	const R = X.length;
	const zero = A[0][0].from(0), one = A[0][0].from(1);

	// Precompute invPrev[k] = recip(prev_k) where prev_k = k>0 ? A[k-1][k-1] : one
	const invPrev: T[] = Array.from({ length: N }, (_, k) => (k > 0 ? A[k - 1][k - 1].recip() : one));

	// Forward: solve L * Y = B  (lower-triangular)
	for (let i = 0; i < N; ++i) {
		const Ai	= A[i];
		const rAii	= (i > 0 ? A[i - 1][i - 1] : one).div(Ai[i]);
		for (let r = 0; r < R; ++r) {
			let s = zero;
			for (let k = 0; k < i; ++k)
				s = s.add(Ai[k].mul(X[k][r]).mul(invPrev[k]));
			X[i][r] = X[i][r].sub(s).mul(rAii);
		}
	}

	// Backward: solve U * X = Y  (upper-triangular)
	for (let i = N - 1; i >= 0; --i) {
		const Ai	= A[i];
		const rAii	= Ai[i].recip();
		for (let r = 0; r < R; ++r) {
			let s = zero;
			for (let k = i + 1; k < N; ++k)
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
/*
// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length N).
export function LUSolveBareissTransposeMultiT<T extends vscalar<T>>(A: T[][], X: T[][], perm?: number[]) {
	const N = A.length;
	const R = X.length;
	const zero = A[0][0].from(0), one = A[0][0].from(1);

	// Precompute invPrev[k] = recip(prev_k) where prev_k = k>0 ? A[k-1][k-1] : one
	const invPrev: T[] = Array.from({ length: N }, (_, k) => (k > 0 ? A[k - 1][k - 1].recip() : one));

	// Forward: solve U^T * Y = B  (lower-triangular)
	for (let i = 0; i < N; ++i) {
		const rAii = (i > 0 ? A[i - 1][i - 1] : one).div(A[i][i]);
		for (let r = 0; r < R; ++r) {
			let s = zero;
			for (let k = 0; k < i; ++k)
				s = s.add(A[k][i].mul(X[r][k]).mul(invPrev[k]));
			X[r][i] = X[r][i].sub(s).mul(rAii);
		}
	}

	// Backward: solve L^T * X = Y  (upper-triangular)
	for (let i = N - 1; i >= 0; --i) {
		const Aii = A[i][i];
		for (let r = 0; r < R; ++r) {
			let s = zero;
			for (let k = i + 1; k < N; ++k)
				s = s.add(A[k][i].mul(X[r][k]));
			X[r][i] = X[r][i].sub(s.div(Aii));
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
*/
//-----------------------------------------------------------------------------
// matrix helpers
//-----------------------------------------------------------------------------

export function characteristic(A: floatN[]): PolynomialN<number> {
	const n = A.length;

	function trace() {
		let trace = 0;
		for (let i = 0; i < n; ++i)
			trace += B_cols[i][i];
		return trace;
	}

	const B_cols = A.map(col => col.dup());
	const I_cols = Array.from({length: n}, (_, i) => floatN.from({length: n}, (_, j):number => i === j ? 1 : 0));

	const coeffs: number[] = [];

	for (let k = 1; k < n; ++k) {
		const ck	= -trace() / k;
		coeffs.push(ck);

		for (let i = 0; i < n; i++)
			B_cols[i] = mulN(A, B_cols[i].add(I_cols[i].scale(ck)));
	}
	coeffs.push(-trace() / n);
	coeffs.reverse();
	return PolynomialN(coeffs);
}

// Basic QR-based eigensolver (returns array of complex eigenvalues).
export function eigenvalues(A: floatN[]): complex[] {
	const n = A.length;

	// For small matrices (<= 5) prefer the polynomial solver (uses closed-form/Aberth)
	if (n <= 5) {
		const roots = characteristic(A).allRoots();
		return roots.map(r => typeof r === 'number' ? complex(r, 0) : complex(r.r, r.i));
	}

	const	tol = 1e-12;
	const	maxIter = Math.max(1000, 100 * n);
	const	eigs: complex[] = [];
	const	cols = A.map(c => c.dup());

	let		m = n;
	for (let iter = 0; m > 1 && iter < maxIter; iter++) {
		const a = cols[m - 2][m - 2];
		const b = cols[m - 1][m - 2];
		const c = cols[m - 2][m - 1];
		const d = cols[m - 1][m - 1];

		// Check deflation by looking at entry (m-1, m-2)
		if (Math.abs(c || 0) <= tol * (Math.abs(a) + Math.abs(d))) {
			eigs.push(complex(d, 0));
			m -= 1;
			continue;
		}

		// Wilkinson shift from bottom 2x2 (extracting using the truncated top-m entries)
		const tr	= a + d;
		const det	= a * d - b * c;
		const disc	= tr * tr - 4 * det;
		let mu: number;
		if (disc >= 0) {
			const s = Math.sqrt(disc);
			const mu1 = 0.5 * (tr + s);
			const mu2 = 0.5 * (tr - s);
			mu = Math.abs(mu1 - d) < Math.abs(mu2 - d) ? mu1 : mu2;
		} else {
			mu = tr * 0.5;
		}

		// Build truncated columns for top m x m block and apply shift
		const ScolsTrunc = cols.slice(0, m).map(c => new floatN(...Object.values(c).slice(0, m)));
		for (let j = 0; j < m; ++j)
			ScolsTrunc[j][j] -= mu;

		// QR decompose truncated S
		const { Q, R } = QR(ScolsTrunc);
		const RQ = matmulN(R, Q);

		// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
		for (let j = 0; j < m; ++j) {
			for (let i = 0; i < m; ++i)
				cols[j][i] = RQ[j][i] + (i === j ? mu : 0);
		}
	}

	// If not fully converged, extract remaining eigenvalues from trailing blocks
	while (m > 1) {
		const a = cols[m - 2][m - 2];
		const b = cols[m - 1][m - 2];
		const c = cols[m - 2][m - 1];
		const d = cols[m - 1][m - 1];

		if (Math.abs(c || 0) <= tol * (Math.abs(a) + Math.abs(d))) {
			eigs.push(complex(cols[m - 1][m - 1], 0));
			m -= 1;
			continue;
		}
		// take 2x2 block - delegate to polynomial solver for correctness
		const roots = PolynomialN([a * d - b * c, -(a + d)]).allRoots();
		eigs.push(...roots);
		m -= 2;
	}

	if (m === 1)
		eigs.push(complex((cols[0])[0], 0));

	// eigenvalues collected bottom-up; reverse to have original order
	return eigs.reverse();
}

