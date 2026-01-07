import { hasop, builtinNumber } from './core';
import complex, { complexT, complexOps, complexFor, canMakeComplex } from './complex';

export interface eigenPair<T> {
	value:	T;
	vector:	T[];
}

//-----------------------------------------------------------------------------
// Vector/Matrix helpers
//-----------------------------------------------------------------------------

export interface Matrix<T extends number|hasop<'mul'|'add'>> {
	c: T[][];
	rows: number;
	cols: number;

	from(b: T)			: Matrix<T>;
	from<U extends hasop<'mul'|'add'|'from'>>(this: Matrix<U>, b: number) : Matrix<T>;
	scale(b: T|number)	: Matrix<T>;
	add(b: Matrix<T>)	: Matrix<T>;
	mul(b: Matrix<T>)	: Matrix<T>;
	div(b: Matrix<T>)	: Matrix<T> | undefined;
	mulVec(b: T[])		: T[];
	nullspace()			: T[][] | undefined;
	trace()				: T;
	det()				: T;
	characteristic()	: T[];
	inverse()			: Matrix<T> | undefined;
	fillHoles(b: T)		: Matrix<T>;
}

class _matN {
	constructor(public c: number[][]) {}
	get rows() { return this.c[0].length; }
	get cols() { return this.c.length; }

	from(b: number) {
		const rows = this.rows;
		const cols = this.cols;
		const m = Array.from({length: cols}, () => Array<number>(rows));
		const n = Math.min(cols, rows);
		for (let i = 0; i < n; i++)
			m[i][i] = b;
		return new _matN(m);
	}

	scale(b: number)		{ return new _matN(this.c.map(c => scaleVecN(c, b))); }
	add(b: _matN)			{ return new _matN(this.c.map((a, i) => addVecN(a, b.c[i]))); }
	mul(b: _matN)			{ return new _matN(b.c.map(b => mulVecN(this.c, b))); }
	mulVec(b: number[])		{ return mulVecN(this.c, b); }
	nullspace()				{ return nullspaceN(this.c); }
	trace()					{ return traceN(this.c); }
	characteristic()		{ return characteristicN(this.c); }
	fillHoles(b: number)	{ fillHoles(this.c, this.rows, b); return this; }

	div(b: _matN) {
		const B		= copyFillHoles(b.c, b.rows, 0);
		const { perm, swaps } = LUDecomposeBareiss(B);
		const sol	= LUSolveBareissMulti(B, copyFillHoles(this.c, this.rows, 0), swaps ? perm : undefined);
		if (sol)
			return new _matN(sol);
	}
	det() {
		const A = copyFillHoles(this.c, this.rows, 0);
		const { swaps } = LUDecomposeBareiss(A);
		const n = A.length;
		return (swaps & 1 ? -A[n - 1][n - 1] : A[n - 1][n - 1]);
	}
	inverse() {
		const A		= copyFillHoles(this.c, this.rows, 0);
		const { perm, swaps } = LUDecomposeBareiss(A);
		const inv	= LUSolveBareissMulti(A, Matrix.identity(this.cols, 1).fillHoles(0).c, swaps ? perm : undefined);
		if (inv)
			return new _matN(inv);
	}
}

class _matT<T extends hasop<'mul'|'add'>> {
	constructor(public c: T[][]) {}
	get rows() { return this.c[0].length; }
	get cols() { return this.c.length; }

	from<U extends hasop<'mul'|'add'|'from'>>(this: _matT<U>, b: number) : _matT<T>;
	from(b: T) : _matT<T>;
	from(b: T|number) {
		if (typeof b === 'number') {
			if (!hasop('from')(this.c[0][0]))
				throw new Error('');
			b = this.c[0][0].from(b);
		}
		const rows = this.rows;
		const cols = this.cols;
		const m = Array.from({length: cols}, () => Array<T>(rows));
		const n = Math.min(cols, rows);
		for (let i = 0; i < n; i++)
			m[i][i] = b;
		//return new _matT(m).fillHoles((b as any).from(0));
		return new _matT(m);
	}

	scale(b: T): _matT<T>;
	scale<U extends hasop<'scale'>>(this: U, b: number): _matT<T>;
	scale(b: T|number) {
		if (typeof b === 'number') {
			if (!hasop('scale')(this.c[0][0]))
				throw new Error('');
			return new _matT(this.c.map(c => scaleVecTN(c as (T & hasop<'scale'>)[], b)));
		}
		return new _matT(this.c.map(c => scaleVecT(c, b)));
	}

	add(b: _matT<T>) 	{ return new _matT(this.c.map((a, i) => addVecT(a, b.c[i]))); }
	mul(b: _matT<T>) 	{ return new _matT(b.c.map(b => mulVecT(this.c, b))); }
	mulVec(b: T[]) 		{ return mulVecT(this.c, b); }
	nullspace() 		{ return nullspaceT(this.c as any); }
	trace() 			{ return traceT(this.c); }
	characteristic() 	{ return characteristicT(this.c as any); }
	fillHoles(b: T) 	{ fillHoles(this.c, this.rows, b); return this; }

	div<U extends bareissOps<U> & hasop<'from', U>>(this: _matT<U>, b: _matT<U>) {
		const zero	= this.c[0][0].from(0);
		const B		= copyFillHoles(b.c, b.rows, zero);
		const { perm, swaps } = LUDecomposeBareissT(B);
		const sol	= LUSolveBareissMultiT(B, copyFillHoles(this.c, this.rows, zero), swaps ? perm : undefined);
		if (sol)
			return new _matT(sol);
	}
	det<U extends bareissOps<U> & hasop<'from', U>>(this: _matT<U>) {
		const A = copyFillHoles(this.c, this.rows, this.c[0][0].from(0));
		const { swaps } = LUDecomposeBareissT(A);
		const n = A.length;
		return (swaps & 1 ? A[n - 1][n - 1].neg() : A[n - 1][n - 1]);
	}
	inverse<U extends bareissOps<U> & hasop<'from', U>>(this: _matT<U>) {
		const t		= this.c[0][0];
		const zero	= t.from(0);
		const A		= copyFillHoles(this.c, this.rows, zero);
		const { perm, swaps } = LUDecomposeBareissT(A);
		const inv	= LUSolveBareissMultiT(A, Matrix.identity(this.cols, t.from(1)).fillHoles(zero).c, swaps ? perm : undefined);
		if (inv)
			return new _matT(inv);
	}
}
export const Matrix = Object.assign(
	function (c: any[][]) {
		return typeof c[0][0] === 'number' ? new _matN(c) : new _matT(c);
	},
	{
		identity(n: number, one: any) {
			return Matrix(Array.from({ length: n }, (_, i) => { const col = Array(n); col[i] = one; return col; }));
		}
	}
) as {
	(c: number[][]): Matrix<number>;
	<T extends hasop<'mul' | 'add'>>(c: T[][]): Matrix<T>;
	identity(n: number, one: number): Matrix<number>;
	identity<T extends hasop<'mul' | 'add'>>(n: number, one: T): Matrix<T>;
};

export function transpose<T>(A: T[][]): T[][] {
	const R: T[][] = [];
	for (const i in A) {
		const a = A[i];
		for (const j in a)
			(R[j] ??= [])[i] = a[j];
	}
	return R;
}

export function makeVec<T>(width: number, value: T) {
	return Array.from({length: width}, () => value);
}

export function makeMat<T>(width: number, height: number, value: T) {
	return Array.from({length: height}, () => Array.from({length: width}, () => value));
}

function fillHoles<T>(A: T[][], n: number, t: T) {
	for (const c of A)
		for (let i = 0; i < n; i++)
			c[i] ??= t;
	return A;
}
function copyFillHoles<T>(A: T[][], n: number, t: T) {
	return A.map(c => Array.from({length: n}, (_, i) => c[i] ?? t));
}


function scaleVecN<T extends builtinNumber>(A: T[], b: T): T[] {
	return A.map(a => a * b) as T[];
}

function scaleVecTN<T extends hasop<'scale',T>>(A: T[], b: number): T[] {
	return A.map(a => a.scale(b));
}
function scaleVecT<T extends hasop<'mul',T>>(A: T[], b: T): T[] {
	return A.map(a => a.mul(b));
}

function addVecN<T extends builtinNumber>(A: T[], B: T[], offset = 0): T[] {
	const r = A.map((a, i) => B[i + offset] ? a + (B[i + offset] as any) : a);
	for (const i in B) {
		if (+i >= offset)
			r[+i - offset] ??= B[i];
	}
	return r;
}
function addVecT<T extends hasop<'add'>>(A: T[], B: T[], offset = 0): T[] {
	const r = A.map((a, i) => B[i + offset] ? a.add(B[i + offset]) : a);
	for (const i in B) {
		if (+i >= offset)
			r[+i - offset] ??= B[i];
	}
	return r;
}
function addScaleVecN<T extends builtinNumber>(R: T[], A: T[], b: T) {
	for (const j in A)
		R[j] = R[j] ? R[j] + (A[j] * b as any) : A[j] * b;
	return R;
}
function addScaleVecT<T extends hasop<'add'|'mul'>>(R: T[], A: T[], b: T) {
	for (const j in A)
		R[j] = R[j] ? R[j].add(A[j].mul(b)) : A[j].mul(b);
	return R;
}

function dotN(A: number[], B: number[], offset = 0): number {
	return A.reduce((acc, a, i) => acc + a * (B[i + offset] ?? 0), 0);
}
function dotT<T extends hasop<'add'|'mul', T>>(A: T[], B: T[], offset = 0): T {
	return A.reduce((acc, a, i) => B[i + offset] ? (acc ? acc.add(a.mul(B[i + offset])) : a.mul(B[i + offset])) : acc, undefined as unknown as T);
}

function norm2N(A: number[]): number {
	return A.reduce((acc, a) => acc + a * a, 0);
}
function norm2T<T extends hasop<'add'|'mul', T>>(A: T[]): T {
	return A.reduce((acc, a) => acc ? acc.add(a.mul(a)) : a.mul(a), undefined as unknown as T);
}

function mulVecN<T extends builtinNumber>(A: T[][], b: T[]): T[] {
	const r: T[] = [];
	for (const i in b)
		addScaleVecN(r, A[i], b[i]);
	return r;
}
function mulVecT<T extends hasop<'mul'|'add'>>(A: T[][], b: T[]): T[] {
	const r: T[] = [];
	for (const i in b)
		addScaleVecT(r, A[i], b[i]);
	return r;
}

function traceN<T extends builtinNumber>(A: T[][]): T {
	const N = A.length;
	let s = A[0][0];
	for (let i = 1; i < N; i++)
		s += A[i][i] as any;
	return s;
}
function traceT<T extends hasop<'add'>>(A: T[][]): T {
	const N = A.length;
	let s = A[0][0];
	for (let i = 1; i < N; i++)
		s = s.add(A[i][i]);
	return s;
}

//-----------------------------------------------------------------------------
// characteristic
//-----------------------------------------------------------------------------

export function characteristicN<T extends builtinNumber>(A: T[][]): T[] {
	const n = A.length;
	const B = A.map(col => col.slice());
	const coeffs: T[] = [];

	for (let k = 1; k < n; ++k) {
		const ck = traceN(B) / -k;
		coeffs.push(ck as any);

		for (let i = 0; i < n; i++) {
			B[i][i] += ck as any;
			B[i] = mulVecN(A, B[i]);
		}
	}
	coeffs.push(traceN(B) / -n as any);
	return coeffs.reverse();
}

export function characteristicT<T extends hasop<'add'|'mul'|'scale'>>(A: T[][]): T[] {
	const n = A.length;
	const B = A.map(col => col.slice());
	const coeffs: T[] = [];

	for (let k = 1; k < n; ++k) {
		const ck = traceT(B).scale(-1 / k);
		coeffs.push(ck);

		for (let i = 0; i < n; i++) {
			B[i][i] = B[i][i].add(ck);
			B[i] = mulVecT(A, B[i]);
		}
	}
	coeffs.push(traceT(B).scale(-1 /n));
	return coeffs.reverse();
}

export function characteristic<T extends builtinNumber|hasop<'add'|'mul'|'scale'>>(A: T[][]): T[] {
	return typeof A[0][0] === 'object'
		? characteristicT(A as any) as any
		: characteristicN(A as any) as any;
}

//-----------------------------------------------------------------------------
// Basic QR-based eigensolver (returns array of complex eigenvalues).
//-----------------------------------------------------------------------------
export function eigenvaluesN(A: number[][]): complex[] {
	const n = A.length;

	const	tol		= 1e-12;
	const	maxIter	= Math.max(1000, 100 * n);
	const	eigs: complex[] = [];
	const	cols	= A.map(c => c.slice());

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
		const disc	= tr * tr - 4 * (a * d - b * c);
		let mu: number;
		if (disc >= 0) {
			const s = Math.sqrt(disc);
			const mu1 = 0.5 * (tr + s);
			const mu2 = 0.5 * (tr - s);
			mu = Math.abs(mu1 - d) < Math.abs(mu2 - d) ? mu1 : mu2;
		} else {
			mu = tr * 0.5;
		}

		for (let j = 0; j < m; j++)
			cols[j][j] -= mu;
		const { Q, R } = QRN(cols, m, m);
		const RQ = Matrix(R).mul(Matrix(Q));

		// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
		for (let j = 0; j < m; ++j) {
			for (let i = 0; i < m; ++i)
				cols[j][i] = RQ.c[j][i] + (i === j ? mu : 0);
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

		} else {
			const tr = a + d;
			const s = complex.sqrt(tr * tr - 4 * (a * d - b * c));
			eigs.push(complex(tr).add(s), complex(tr).sub(s));
			m -= 2;
		}
	}

	if (m === 1)
		eigs.push(complex((cols[0])[0], 0));

	// eigenvalues collected bottom-up; reverse to have original order
	return eigs.reverse();
}

export function eigenvaluesT<T extends complexOps<T> & hasop<'recip'|'lt'>>(A: T[][]): complexT<T>[] {
	const n = A.length;
	if (n === 0)
		return [];

	const	maxIter = Math.max(1000, 100 * n);
	const	cols = A.map(c => c.slice());
	const 	tol = 1e-12;
	const	eigs: complexT<T>[] = [];

	let		m = n;
	for (let iter = 0; m > 1 && iter < maxIter; iter++) {
		const a = cols[m - 2][m - 2];
		const b = cols[m - 1][m - 2];
		const c = cols[m - 2][m - 1];
		const d = cols[m - 1][m - 1];

		// Check deflation by looking at subdiagonal entry (row m-1, col m-2) => cols[m-2][m-1]
		if (!c || c.abs().lt(a.abs().add(d.abs()).scale(tol))) {
			eigs.push(complexT(d));
			m -= 1;
			continue;
		}

		// Wilkinson shift from bottom 2x2 (extracting using the truncated top-m entries)
		const tr	= a.add(d);
		const disc	= tr.mul(tr).sub(a.mul(d).sub(b.mul(c)).scale(4));
		let mu;
		if (disc.sign() < 0) {
			mu = tr.scale(0.5);
		} else {
			const s = disc.sqrt();
			const mu1 = tr.add(s).scale(0.5);
			const mu2 = tr.sub(s).scale(0.5);
			mu = mu1.sub(d).abs() < mu2.sub(d).abs() ? mu1 : mu2;
		}

		// QR decompose truncated S
		for (let j = 0; j < m; j++)
			cols[j][j] = cols[j][j].sub(mu);
		const { Q, R } = QRT(cols, m, m);
		const RQ = Matrix(R).mul(Matrix(Q));

		// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
		for (let j = 0; j < m; ++j) {
			for (let i = 0; i < m; ++i)
				cols[j][i] = i == j ? RQ.c[j][i].add(mu) : RQ.c[j][i];
		}
	}

	// If not fully converged, extract remaining eigenvalues from trailing blocks
	while (m > 1) {
		const a = cols[m - 2][m - 2];
		const b = cols[m - 1][m - 2];
		const c = cols[m - 2][m - 1];
		const d = cols[m - 1][m - 1];

		if (!c || c.abs().lt(a.abs().add(d.abs()).scale(tol))) {
			eigs.push(complexT<T>(d));
			m -= 1;

		} else {
			const tr = a.add(d);
			const s = complexT.sqrt(tr.mul(tr).sub(a.mul(d).sub(b.mul(c)).scale(4)));
			eigs.push(complexT(tr).add(s), complexT(tr).sub(s));
			m -= 2;
		}
	}

	if (m === 1)
		eigs.push(complexT((cols[0])[0]));

	// eigenvalues collected bottom-up; reverse to have original order
	return eigs.reverse();
}

export function eigenvalues<T extends canMakeComplex>(A: T[][]): complexFor<T>[] {
	return typeof A[0][0] === 'number'
		? eigenvaluesN(A as any) as any
		: eigenvaluesT(A as any) as any;
}

export function eigenVectorN(A: number[][], eigenvalue: number) {
	const M = copyFillHoles(A, A[0].length, 0);
	for (let i = 0; i < A.length; i++)
		M[i][i] -= eigenvalue;
	return nullspaceN(M);
}
export function eigenVectorT<T extends bareissOps<T> & hasop<'from'>>(A: T[][], eigenvalue: T) {
	const M = copyFillHoles(A, A[0].length, eigenvalue.sub(eigenvalue));
	for (let i = 0; i < A.length; i++)
		M[i][i] = M[i][i].sub(eigenvalue);
	return nullspaceT(M);
}

/*
export function eigenPairsN(A: number[][]): eigenPair<complex>[] {
	const n = A.length;
	if (n === 0)
		return [];

	const	tol = 1e-12;
	const	maxIter = Math.max(1000, 100 * n);
	const	cols = A.map(c => c.slice());

	// Initialize V as identity (column-major, sparse is OK)
	const	V = Matrix.identity(n, 1);

	let		m = n;
	for (let iter = 0; m > 1 && iter < maxIter; iter++) {
		const a = cols[m - 2][m - 2];
		const b = cols[m - 1][m - 2];
		const c = cols[m - 2][m - 1];
		const d = cols[m - 1][m - 1];

		// Check deflation by looking at subdiagonal entry (row m-1, col m-2) => cols[m-2][m-1]
		if (Math.abs(c || 0) <= tol * (Math.abs(a) + Math.abs(d))) {
			m -= 1;
			continue;
		}

		// Wilkinson shift from bottom 2x2 (extracting using the truncated top-m entries)
		const tr	= a + d;
		const disc	= tr * tr - 4 * (a * d - b * c);
		let mu: number;
		if (disc < 0) {
			mu = tr * 0.5;
		} else {
			const s = Math.sqrt(disc);
			const mu1 = 0.5 * (tr + s);
			const mu2 = 0.5 * (tr - s);
			mu = Math.abs(mu1 - d) < Math.abs(mu2 - d) ? mu1 : mu2;
		}

		// QR decompose truncated S
		for (let j = 0; j < m; j++)
			cols[j][j] -= mu;
		const { Q, R } = QRN(cols, m, m);
		const RQ = Matrix(R).mul(Matrix(Q));

		// Apply Q transformation to the relevant part of V
		V.c = V.mul(Matrix(Q)).c;

		// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
		for (let j = 0; j < m; ++j) {
			for (let i = 0; i < m; ++i)
				cols[j][i] = RQ.c[j][i] + (i === j ? mu : 0);
		}
	}

	// After convergence: cols ≈ real Schur form T, V = Schur vectors
	const eigs: complex[] = [];
	const eigenvectors: complexFor<number>[][] = [];
	for (let i = 0; i < n; i++) {
		// Check if we have a 2×2 block: subdiagonal (row i+1, col i) or superdiagonal (row i, col i+1)
		const a = cols[i + 0][i + 0];
		const b = cols[i + 1][i + 0];
		const c = cols[i + 0][i + 1];
		if (i < n - 1 && (Math.abs(c) > tol || Math.abs(b) > tol)) {
			const d = cols[i + 1][i + 1];
			const tr = a + d;
			const s = complex.sqrt(tr * tr - 4 * (a * d - b * c));
			const l1 = complex(tr).add(s);
			const l2 = complex(tr).sub(s);
			eigs.push(l1, l2);

			// Choose stable equation for ratio (complex-aware)
			const ratio = Math.abs(c) >= Math.abs(b)
				? complex(a, 0).sub(l1).scale(1 / c)
				: complex(-b, 0).div(complex(d, 0).sub(l1));

			// Eigenvector in Schur basis: [1, ratio]
			const	col0	= V.c[i + 0], col1 = V.c[i + 1];
			let		ev		= addScaleVecT(col0 ? col0.map(complex) : [], col1 ? col1.map(complex) : [], ratio);
			if (ev.length) {
				const pivot	= ev.reduce((besti, v, i) => v.magSq() > ev[besti].magSq() ? i : besti, 0);
				const phase	= ev[pivot].recip();
				ev	= ev.map(v => v.mul(phase));
			}

			eigenvectors.push(ev, ev.map(i => i.conj()));
			++i;
		} else {
			// 1×1 block
			eigs.push(complex(a, 0));
			eigenvectors.push(V.c[i] && V.c[i].some(Boolean) ? V.c[i].map(complex) : Array.from({length: n}, (_, j) => complex(i === j ? 1 : 0, 0)));
		}
	}

	return eigs.map((value, i) => ({ value, vector: Math.abs(value.i) <= tol ? eigenvectors[i].map(v => complex(v.r, 0)) : eigenvectors[i] }));
}

export function eigenPairsT<T extends complexOps<T>&hasop<'from'|'recip'>>(A: T[][]): eigenPair<complexT<T>>[] {
	const n = A.length;
	if (n === 0)
		return [];

	const	maxIter = Math.max(1000, 100 * n);
	const	cols = A.map(c => c.slice());
	const 	tol = 1e-12;
	const	one = A[0][0].from(1);

	// Initialize V as identity (column-major, sparse is OK)
	const	V = Matrix.identity(n, A[0][0].from(1));

	let		m = n;
	for (let iter = 0; m > 1 && iter < maxIter; iter++) {
		const a = cols[m - 2][m - 2];
		const b = cols[m - 1][m - 2];
		const c = cols[m - 2][m - 1];
		const d = cols[m - 1][m - 1];

		// Check deflation by looking at subdiagonal entry (row m-1, col m-2) => cols[m-2][m-1]
		if (!c || c.abs().lt(a.abs().add(d.abs()).scale(tol))) {
			m -= 1;
			continue;
		}

		// Wilkinson shift from bottom 2x2 (extracting using the truncated top-m entries)
		const tr	= a.add(d);
		const disc	= tr.mul(tr).sub(a.mul(d).sub(b.mul(c)).scale(4));
		let mu;
		if (disc.sign() < 0) {
			mu = tr.scale(0.5);
		} else {
			const s = disc.sqrt();
			const mu1 = tr.add(s).scale(0.5);
			const mu2 = tr.sub(s).scale(0.5);
			mu = mu1.sub(d).abs() < mu2.sub(d).abs() ? mu1 : mu2;
		}

		// QR decompose truncated S
		for (let j = 0; j < m; j++)
			cols[j][j] = cols[j][j].sub(mu);
		const { Q, R } = QRT(cols, m, m);
		const RQ = Matrix(R).mul(Matrix(Q));

		// Apply Q transformation to the relevant part of V
		V.c = V.mul(Matrix(Q)).c;

		// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
		for (let j = 0; j < m; ++j) {
			for (let i = 0; i < m; ++i)
				cols[j][i] = i == j ? RQ.c[j][i].add(mu) : RQ.c[j][i];
		}
	}

	// After convergence: cols ≈ real Schur form T, V = Schur vectors
	const eigs: complexT<T>[] = [];
	const eigenvectors: complexT<T>[][] = [];
	for (let i = 0; i < n; i++) {
		// Check if we have a 2×2 block: subdiagonal (row i+1, col i) or superdiagonal (row i, col i+1)
		const a = cols[i + 0][i + 0];
		const b = cols[i + 1][i + 0];
		const c = cols[i + 0][i + 1];
		if (i < n - 1 && (c.sign() || b.sign())) {
			const d = cols[i + 1][i + 1];
			const tr = a.add(d);
			const s = complexT.sqrt(tr.mul(tr).sub(a.mul(d).sub(b.mul(c)).scale(4)));
			const l1 = complexT(tr).add(s);
			const l2 = complexT(tr).sub(s);
			eigs.push(l1, l2);

			// Choose stable equation for ratio (complex-aware)
			const ratio = b.abs().lt(c.abs())
				? complexT(a).sub(l1).div(complexT(c))
				: complexT(b.neg()).div(complexT(d).sub(l1));

			// Eigenvector in Schur basis: [1, ratio]
			const	col0	= V.c[i + 0], col1 = V.c[i + 1];
			let		ev		= addScaleVecT(col0 ? col0.map(complexT) : [], col1 ? col1.map(complexT) : [], ratio) as complexT<T>[];
			if (ev.length) {
				const pivot	= ev.reduce((besti, v, i) => v.magSq() > ev[besti].magSq() ? i : besti, 0);
				const phase	= ev[pivot].recip();
				ev	= ev.map(v => v.mul(phase));
			}

			eigenvectors.push(ev, ev.map(i => i.conj()));
			++i;
		} else {
			// 1×1 block
			eigs.push(complexT(a));
			eigenvectors.push(V.c[i] && V.c[i].some(Boolean) ? V.c[i].map(complexT) : Array.from({length: n}, (_, j) => complexT(one.scale(j ? 1 : 0))));
		}
	}

	return eigs.map((value, i) => ({ value, vector: value.i.sign() ? eigenvectors[i].map(v => complexT(v.r)) : eigenvectors[i] }));
}

export function eigenPairs<T extends canMakeComplex>(A: T[][]): eigenPair<complexFor<T>>[] {
	return typeof A[0][0] === 'number'
		? eigenPairsN(A as any) as any
		: eigenPairsT(A as any) as any;
}
*/
//-----------------------------------------------------------------------------
// projectSubspace
// Project matrix M (column-major) into the invariant subspace spanned by `nsCols` (array of column vectors).
// Returns the small m x m matrix M_sub with columns expressed in the basis `nsCols`.
//-----------------------------------------------------------------------------

export function projectSubspaceN(M: number[][], ns: number[][]): number[][] {
	const N			= M.length;
	const m			= ns.length;
	const M_sub		= makeMat(m, m, 0);
	const A			= Array.from({ length: N }, (_, row) => Array.from({ length: m }, (_, col) => ns[col][row] ?? 0));
	const { perm }	= LUDecomposeBareiss(A);

	for (let j = 0; j < m; j++) {
		// w = M * nsCols[j]
		const w = makeVec(N, 0);
		for (let row = 0; row < N; row++) {
			let sum = 0;
			for (let col = 0; col < N; col++) {
				if (M[col][row] && ns[j][col])
					sum += M[col][row] * ns[j][col];
			}
			w[row] = sum;
		}
		const sol = LUSolveBareiss(A, w, perm);
		if (sol) {
			for (let i = 0; i < m; i++)
				M_sub[i][j] = sol[i];
		}
	}
	return M_sub;
}

export function projectSubspaceT<T extends bareissOps<T>>(M: T[][], ns: T[][], zero: T): T[][] {
	const N			= M.length;
	const m			= ns.length;
	const M_sub		= makeMat(m, m, zero);
	const A			= Array.from({ length: N }, (_, row) => Array.from({ length: m }, (_, col) => ns[col][row] ?? zero));
	const { perm, swaps } = LUDecomposeBareissT(A);

	for (let j = 0; j < m; j++) {
		// w = M * nsCols[j]
		const w = makeVec(N, zero);
		for (let row = 0; row < N; row++) {
			let sum = zero;
			for (let col = 0; col < N; col++) {
				if (M[col][row] && ns[j][col])
					sum = sum.add(M[col][row].mul(ns[j][col]));
			}
			w[row] = sum;
		}
		
		// Handle full-rank case: when A is m x m and full rank, solution is direct
			// A is identity matrix; solution is just w
		const sol = m === N && swaps === 0 ? w : LUSolveBareissT(A, w, perm);
		if (sol) {
			for (let i = 0; i < m; i++)
				M_sub[i][j] = sol[i];
		}
	}
	return M_sub;
}

export function projectSubspace<T extends number|bareissOps<T>>(M: T[][], ns: T[][], zero: T): T[][] {
	return typeof zero === 'number'
		? projectSubspaceN(M as any, ns as any) as any
		: projectSubspaceT(M as any, ns as any, zero as any) as any;
}

//-----------------------------------------------------------------------------
// QR factorisation: modified Gram-Schmidt
// Returns Q (array of number[]) and R (numeric m x m upper-triangular)
//-----------------------------------------------------------------------------

export function QRN(cols: number[][], n = cols.length, m = cols[0]?.length ?? 0) {
	const Q: number[][] = [];
	const R = cols.map(() => Array(m).fill(0));

	for (let j = 0; j < n; ++j) {
		const v = cols[j].slice();
		for (let i = 0; i < j; ++i) {
			const rij = dotN(Q[i], v);
			R[j][i] = rij;
			for (let k = 0; k < m; ++k)
				v[k] -= Q[i][k] * rij;
		}
		const norm	= Math.sqrt(norm2N(v));
		const scale	= norm ? 1 / norm : 0;
		R[j][j]	= norm;
		Q[j]	= v.map(v => v * scale);
	}
	return { Q, R };
}

export function QRT<T extends hasop<'add'|'sub'|'mul'|'sqrt'|'sign'|'recip', T>>(cols: T[][], n = cols.length, m = cols[0]?.length ?? 0) {
	const Q: T[][] = [];
	const R = cols.map(() => Array(m).fill(0));

	for (let j = 0; j < n; ++j) {
		const v = cols[j].slice();
		for (let i = 0; i < j; ++i) {
			const rij = dotT(Q[i], v);
			R[j][i] = rij;
			for (let k = 0; k < m; ++k)
				v[k] = v[k].sub(Q[i][k].mul(rij));
		}
		const norm	= norm2T(v).sqrt();
		const scale	= norm.sign() ? norm.recip() : norm;
		R[j][j]	= norm;
		Q[j]	= v.map(v => v.mul(scale));
	}
	return { Q, R };
}

export function QR<T extends number|hasop<'add'|'sub'|'mul'|'sqrt'|'sign'|'recip', T>>(cols: T[][], n = cols.length, m = cols[0]?.length ?? 0) {
	return typeof cols[0][0] === 'number'
		? QRN(cols as any, n, m) as any
		: QRT(cols as any, n, m) as any;
}

//-----------------------------------------------------------------------------
// QR factorisation: householder
//-----------------------------------------------------------------------------

export function QRHouseholder(cols: number[][]) {
	const n = cols.length;
	const m = cols[0].length;

	const Q = Array.from({ length: m }, (_, k) => {
		const col = makeVec(m, 0);
		col[k] = 1;
		return col;
	});

	// Process each column
	for (let j = 0; j < Math.min(n, m); j++) {
		const x = cols[j].slice(j);
		let xlen2 = norm2N(x);
		if (xlen2 === 0)
			continue;

		const xlen	= Math.sqrt(xlen2);
		xlen2 = 2 * (xlen2 + xlen * Math.abs(x[0]));
		x[0] += Math.sign(x[0]) * xlen;

		// Apply H = I - 2*v*v^T to columns of A
		for (let k = j; k < n; k++) {
			const dot = dotN(x, cols[k], j) * 2 / xlen2;
			for (let i = 0; i < x.length; i++)
				cols[k][j + i] -= dot * x[i];
		}

		// Apply H to Q: for each column of Q, apply the reflection
		for (let k = 0; k < m; k++) {
			const dot = dotN(x, Q[k], j) * 2 / xlen2;
			for (let i = 0; i < x.length; i++)
				Q[k][j + i] -= dot * x[i];
		}
	}

	return Q;
}

//-----------------------------------------------------------------------------
// LU Decomposition and Solvers
//-----------------------------------------------------------------------------

export type bareissOps<T> = hasop<'neg'|'add'|'sub'|'mul'|'div'|'recip'|'sign', T>;

export function LUDecomposeBareiss(A: number[][], pivot = 1) {
	const	m		= A.length;
	const	n		= A[0].length;
	const	cperm	= Array.from({length: Math.min(m, n)}, (_,i) => i);
	const	rperm	= Array.from({length: Math.min(m, n)}, (_,i) => i);
	let		prev	= 1;
	let		swaps	= 0;

	function pivotCol(k: number) {
		let swap = k + 1;
		while (swap < m && !A[swap][k])
			swap++;
		if (swap === m)
			return false;
		[A[k], A[swap]] = [A[swap], A[k]];
		[cperm[k], cperm[swap]] = [cperm[swap], cperm[k]];
		swaps++;
		return true;
	}
	function pivotRow(k: number) {
		let swap = k + 1;
		while (swap < n && !A[k][swap])
			swap++;
		if (swap === n)
			return false;
		for (let i = 0; i < m; i++)
			[A[i][k], A[i][swap]] = [A[i][swap], A[i][k]];
		[rperm[k], rperm[swap]] = [rperm[swap], rperm[k]];
		swaps++;
		return true;
	}

	for (let k = 0; k < Math.min(m, n); k++) {
		if (pivot && !A[k][k]) {
			if (!(((pivot & 1) && pivotCol(k)) || ((pivot & 2) && pivotRow(k))))
				return { swaps, perm: cperm.slice(0, k), rperm: rperm.slice(0, k) };
		}

		const akk = A[k][k];
		for (let i = k + 1; i < m; i++) {
			for (let j = k + 1; j < n; j++)
				A[i][j] = (A[i][j] * akk - A[i][k] * A[k][j]) / prev;
		}
		prev = akk;
	}
	return { swaps, perm: cperm, rperm };
}

export function LUDecomposeBareissT<T extends bareissOps<T>>(A: T[][], pivot = 1) {
	const	m		= A.length;
	const	n		= A[0].length;
	const	cperm	= Array.from({length: Math.min(m, n)}, (_,i) => i);
	const	rperm	= Array.from({length: Math.min(m, n)}, (_,i) => i);
	let		prev: T | undefined;
	let		swaps	= 0;

	function pivotCol(k: number) {
		let swap = k + 1;
		while (swap < m && A[swap][k].sign() === 0)
			swap++;
		if (swap === m)
			return false;
		[A[k], A[swap]] = [A[swap], A[k]];
		[cperm[k], cperm[swap]] = [cperm[swap], cperm[k]];
		swaps++;
		return true;
	}
	function pivotRow(k: number) {
		let swap = k + 1;
		while (swap < n && A[k][swap].sign() === 0)
			swap++;
		if (swap === n)
			return false;
		for (let i = 0; i < m; i++)
			[A[i][k], A[i][swap]] = [A[i][swap], A[i][k]];
		[rperm[k], rperm[swap]] = [rperm[swap], rperm[k]];
		swaps++;
		return true;
	}

	for (let k = 0; k < Math.min(m, n); k++) {
		if (pivot && A[k][k].sign() === 0) {
			if (!(((pivot & 1) && pivotCol(k)) || ((pivot & 2) && pivotRow(k))))
				return { swaps, perm: cperm.slice(0, k), rperm: rperm.slice(0, k) };
		}

		const akk = A[k][k];
		for (let i = k + 1; i < m; i++) {
			for (let j = k + 1; j < n; j++) {
				const Aik = A[i][k], Akj = A[k][j];
				if (Aik && Akj) {
					const t = Aik.mul(Akj);
					A[i][j] = A[i][j] ? ((prev ? A[i][j].div(prev) : A[i][j]).mul(akk).sub(t)) : t.neg();
				} else if (prev && A[i][j]) {
					A[i][j] = A[i][j].div(prev);
				}
			}
		}
		prev = akk;
	}
	return { swaps, perm: cperm, rperm };
}

export function LUSolveBareiss(A: number[][], X: number[], perm?: number[]) {
	const m = A.length;
	const n = A[0].length;
	const rank = perm ? perm.length : Math.min(m, n);

	if (rank === 0)
		return [];

	const inv = Array.from({ length: rank }, (_, k) => 1 / A[k][k]);
	const Y: number[] = [];

	// Forward: solve L * Y = B  (lower-triangular)
	Y[0] = X[0] * inv[0];
	for (let i = 1; i < rank; ++i) {
		const Ai	= A[i];
		let s = Ai[0] * Y[0];
		for (let k = 1; k < i; ++k)
			s += Ai[k] * Y[k] * inv[k - 1];
		Y[i] = (X[i] - s) * A[i - 1][i - 1] / Ai[i];
	}

	// For overdetermined systems, check consistency
	if (m > n) {
		for (let i = rank; i < m; ++i) {
			const Ai = A[i];
			let s = 0;
			for (let k = 0; k < rank; ++k)
				s += Ai[k] * Y[k];
			if (Math.abs(X[i] - s) > 1e-10)
				return undefined;
		}
	}

	// Backward: solve U * X = Y  (upper-triangular)
	for (let i = rank - 2; i >= 0; --i) {
		const Ai	= A[i];
		let s = 0;
		for (let k = i + 1; k < rank; ++k)
			s += Ai[k] * Y[k];
		Y[i] -= s * inv[i];
	}

	if (perm && perm.length > 0) {
		const invperm: number[] = [];
		for (let i = 0; i < perm.length; i++)
			invperm[perm[i]] = i;
		return Array.from({ length: n }, (_, i) => i < rank ? Y[invperm[i]] : 0);
	}

	for (let i = rank; i < n; ++i)
		Y[i] = 0;
	return Y;
}

export function LUSolveBareissT<T extends bareissOps<T>>(A: T[][], X: T[], perm?: number[]) {
	const m		= A.length;
	const n		= A[0].length;
	const rank	= perm ? perm.length : Math.min(m, n);

	if (rank === 0)
		return [];

	const inv	= Array.from({ length: rank }, (_, k) => A[k][k].recip());
	const Y: T[] = [];

	// Forward: solve L * Y = B  (lower-triangular)
	Y[0] = X[0].mul(inv[0]);
	for (let i = 1; i < rank; ++i) {
		const Ai	= A[i];
		let s = Ai[0].mul(Y[0]);
		for (let k = 1; k < i; ++k)
			s = s.add(Ai[k].mul(Y[k]).mul(inv[k - 1]));
		Y[i] = X[i].sub(s).mul(A[i - 1][i - 1]).div(Ai[i]);
	}

	// For overdetermined systems, check consistency
	if (m > n) {
		for (let i = rank; i < m; ++i) {
			const Ai = A[i];
			let s = Ai[0].mul(Y[0]);
			for (let k = 1; k < rank; ++k)
				s = s.add(Ai[k].mul(Y[k]));
			if (X[i].sub(s).sign() !== 0)
				return undefined;
		}
	}

	// Backward: solve U * X = Y  (upper-triangular)
	for (let i = rank - 2; i >= 0; --i) {
		const Ai	= A[i];
		let s = Ai[i + 1].mul(Y[i + 1]);
		for (let k = i + 2; k < rank; ++k)
			s = s.add(Ai[k].mul(Y[k]));
		Y[i] = Y[i].sub(s.mul(inv[i]));
	}

	// For underdetermined systems, pad with zeros
	const zero = X[0].sub(X[0]);

	if (perm) {
		const invperm: number[] = [];
		for (let i = 0; i < m; i++)
			invperm[perm[i]] = i;
		return Array.from({ length: n }, (_, i) => i < rank ? Y[invperm[i]] : zero);
	}

	for (let i = rank; i < n; ++i)
		Y[i] = zero;
	return Y;
}

export function LUSolveBareissMulti(A: number[][], X: number[][], perm?: number[]) {
	const m = A.length;
	const n = A[0].length;
	const R = X.length;
	const rank = Math.min(m, n);

	const inv = Array.from({ length: rank }, (_, k) => 1 / A[k][k]);

	// Forward: solve L * Y = B  (lower-triangular)
	for (let r = 0; r < R; ++r)
		X[0][r] *= inv[0];
	for (let i = 1; i < rank; ++i) {
		const Ai	= A[i];
		const rAii	= A[i - 1][i - 1] / Ai[i];
		for (let r = 0; r < R; ++r) {
			let s = Ai[0] * X[0][r];
			for (let k = 1; k < i; ++k)
				s += Ai[k] * X[k][r] * inv[k - 1];
			X[i][r] = (X[i][r] - s) * rAii;
		}
	}

	// For overdetermined systems, check consistency
	if (m > n) {
		for (let i = rank; i < m; ++i) {
			const Ai = A[i];
			for (let r = 0; r < R; ++r) {
				let s = 0;
				for (let k = 0; k < rank; ++k)
					s += Ai[k] * X[k][r];
				if (Math.abs(X[i][r] - s) > 1e-10)
					return undefined;
			}
		}
	}

	// Backward: solve U * X = Y  (upper-triangular)
	for (let i = rank - 2; i >= 0; --i) {
		const Ai	= A[i];
		const rAii	= inv[i];
		for (let r = 0; r < R; ++r) {
			let s = 0;
			for (let k = i + 1; k < rank; ++k)
				s += Ai[k] * X[k][r];
			X[i][r] = X[i][r] - s * rAii;
		}
	}

	// Resize solution to n rows
	const Y = Array.from({ length: n }, (_, i) => Array.from({ length: R }, (_, r) => X[i]?.[r] ?? 0));

	if (perm) {
		const invperm: number[] = [];
		for (let i = 0; i < m; i++)
			invperm[perm[i]] = i;
		return Y.map((_, i) => {
			const pi = invperm[i];
			return pi < n ? Array.from({ length: R }, (_, r) => Y[pi][r]) : Array.from({ length: R }, () => 0);
		});
	}
	return Y;
}

export function LUSolveBareissMultiT<T extends bareissOps<T>>(A: T[][], X: T[][], perm?: number[]) {
	const m = A.length;
	const n = A[0].length;
	const R = X.length;
	const rank = Math.min(m, n);

	const inv: T[] = Array.from({ length: rank }, (_, k) => A[k][k].recip());

	// Forward: solve L * Y = B  (lower-triangular)
	for (let r = 0; r < R; ++r)
		X[0][r] = X[0][r].mul(inv[0]);
	for (let i = 1; i < rank; ++i) {
		const Ai	= A[i];
		const rAii	= A[i - 1][i - 1].div(Ai[i]);
		for (let r = 0; r < R; ++r) {
			let s = Ai[0].mul(X[0][r]);
			for (let k = 1; k < i; ++k)
				s = s.add(Ai[k].mul(X[k][r]).mul(inv[k - 1]));
			X[i][r] = X[i][r].sub(s).mul(rAii);
		}
	}

	// For overdetermined systems, check consistency
	if (m > n) {
		for (let i = rank; i < m; ++i) {
			const Ai = A[i];
			for (let r = 0; r < R; ++r) {
				let s = Ai[0].mul(X[0][r]);
				for (let k = 1; k < rank; ++k)
					s = s.add(Ai[k].mul(X[k][r]));
				if (X[i][r].sub(s).sign() !== 0)
					return undefined;
			}
		}
	}

	// Backward: solve U * X = Y  (upper-triangular)
	for (let i = rank - 2; i >= 0; --i) {
		const Ai	= A[i];
		const rAii	= inv[i];
		for (let r = 0; r < R; ++r) {
			let s = Ai[i + 1].mul(X[i + 1][r]);
			for (let k = i + 2; k < rank; ++k)
				s = s.add(Ai[k].mul(X[k][r]));
			X[i][r] = X[i][r].sub(s.mul(rAii));
		}
	}

	// Resize solution to n rows
	const zero = X[0][0].sub(X[0][0]);
	const Y = Array.from({ length: n }, (_, i) => Array.from({ length: R }, (_, r) => X[i]?.[r] ?? zero));

	if (perm) {
		const invperm: number[] = [];
		for (let i = 0; i < m; i++)
			invperm[perm[i]] = i;
		return Y.map((_, i) => {
			const pi = invperm[i];
			return pi < n ? Array.from({ length: R }, (_, r) => Y[pi][r]) : Array.from({ length: R }, () => zero);
		});
	}
	return Y;
}

// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length n).
export function LUSolveBareissTransposeMulti(A: number[][], X: number[][], perm?: number[]) {
	const m = A.length;
	const n = A[0].length;
	const R = X.length;
	const rank = Math.min(m, n);

	const inv = Array.from({ length: rank }, (_, k) => 1 / A[k][k]);

	// Forward: solve U^T * Y = B  (upper-triangular)
	for (let r = 0; r < R; ++r)
		X[r][0] = X[r][0] * inv[0];
	for (let i = 1; i < rank; ++i) {
		const rAii = A[i - 1][i - 1] / A[i][i];
		for (let r = 0; r < R; ++r) {
			const x = X[r];
			let s = 0;
			for (let k = 1; k < i; ++k)
				s += A[k][i] * x[k] * inv[k - 1];
			x[i] = (x[i] - s) * rAii;
		}
	}

	// For overdetermined systems, check consistency
	if (n > m) {
		for (let r = 0; r < R; ++r) {
			for (let i = rank; i < n; ++i) {
				if (Math.abs(X[r][i]) > 1e-10)
					return undefined;
			}
		}
	}

	// Backward: solve L^T * X = Y  (lower-triangular, unit diagonal)
	for (let i = rank - 2; i >= 0; --i) {
		const rAii = inv[i];
		for (let r = 0; r < R; ++r) {
			const x = X[r];
			let s = 0;
			for (let k = i + 1; k < rank; ++k)
				s += A[k][i] * x[k];
			x[i] = x[i] - s * rAii;
		}
	}

	// Resize solution to m rows
	const Y = X.map(x => Array.from({ length: m }, (_, i) => x[i] ?? 0));

	if (perm) {
		const invperm: number[] = [];
		for (let i = 0; i < m; i++)
			invperm[perm[i]] = i;
		return Y.map(x => invperm.map(i => x[i]));
	}
	return Y;
}

export function LUSolveBareissTransposeMultiT<T extends bareissOps<T>>(A: T[][], X: T[][], perm?: number[]) {
	const m = A.length;
	const n = A[0].length;
	const R = X.length;
	const rank = Math.min(m, n);

	const inv: T[] = Array.from({ length: rank }, (_, k) => A[k][k].recip());

	// Forward: solve U^T * Y = B  (upper-triangular)
	for (let r = 0; r < R; ++r)
		if (X[r][0])
			X[r][0] = X[r][0].mul(inv[0]);
	for (let i = 1; i < rank; ++i) {
		const rAii = A[i - 1][i - 1].div(A[i][i]);
		for (let r = 0; r < R; ++r) {
			let	s = A[0][i].mul(X[r][0]);
			for (let k = 1; k < i; ++k)
				s = s.add(A[k][i].mul(X[r][k]).mul(inv[k - 1]));
			X[r][i] = X[r][i].sub(s).mul(rAii);
		}
	}

	// For overdetermined systems, check consistency
	if (n > m) {
		for (let r = 0; r < R; ++r) {
			for (let i = rank; i < n; ++i) {
				if (X[r][i].sign() !== 0)
					return undefined;
			}
		}
	}

	// Backward: solve L^T * X = Y  (lower-triangular)
	for (let i = rank - 2; i >= 0; --i) {
		const rAii = inv[i];
		for (let r = 0; r < R; ++r) {
			let s = A[i + 1][i].mul(X[r][i + 1]);
			for (let k = i + 2; k < rank; ++k)
				s = s.add(A[k][i].mul(X[r][k]));
			X[r][i] = X[r][i].sub(s.mul(rAii));
		}
	}

	// Resize solution to m rows
	const zero = X[0][0].sub(X[0][0]);
	const Y = X.map(x => Array.from({ length: m }, (_, i) => x[i] ?? zero));

	if (perm) {
		const invperm: number[] = [];
		for (let i = 0; i < m; i++)
			invperm[perm[i]] = i;
		return Y.map(x => invperm.map(i => x[i]));
	}
	return Y;
}

// Column-major nullspace: accepts A as array of columns (each column length m)
// Returns an array of column vectors (each length n) forming a basis for the nullspace
// mutates A
export function nullspaceN(A: number[][]): number[][] {
	const n = A.length; // number of columns
	if (n === 0)
		return [];

	const m		= A[0].length; // number of rows

	fillHoles(A, m, 0);

	// LU decomposition to determine rank and pivot structure
	const { perm } = LUDecomposeBareiss(A);
	const rank = perm.length;

	// All rows zero -> entire space is the nullspace
	if (rank === 0)
		return Array.from({ length: n }, (_, j) => Array.from({ length: n }, (_, i) => i === j ? 1 : 0));

	// Free columns (in permuted order) are those at indices >= rank
	// Solve for each free column, then map result back to original column position via perm
	const result: number[][] = [];
	for (let i = rank; i < n; i++) {
		const sol = LUSolveBareiss(A, Array.from({ length: m }, (_, j) => j === i ? 1 : 0), perm);
		if (sol)
			result.push(sol);
	}
	return result;
}


// Column-major nullspace: accepts A as array of columns (each column length m)
// Returns an array of column vectors (each length n) forming a basis for the nullspace
// mutates A
export function nullspaceT<T extends bareissOps<T> & hasop<'from',T>>(A: T[][]): T[][] {
	const n = A.length; // number of columns
	if (n === 0)
		return [];

	const m		= A[0].length; // number of rows
	const zero	= A[0][0].from(0);
	const one	= A[0][0].from(1);
	
	fillHoles(A, m, zero);

	// LU decomposition to determine rank and pivot structure
	const { perm, rperm } = LUDecomposeBareissT(A, 3);
	const rank = perm.length;

	// All columns are free; return standard basis vectors for the full space
	if (rank === 0)
		return Array.from({ length: n }, (_, i) => Array.from({ length: n }, (_, j) => i === j ? one : zero));

	// Free columns (in permuted order) are those at indices >= rank
	// Solve for each free column, then map result back to original column position via perm
	const result: T[][] = [];
	for (let i = rank; i < n; i++) {
		const sol = LUSolveBareissT(A, Array.from({ length: m }, (_, j) => j === i ? one : zero), perm);
		if (sol)
			result.push(sol);
	}
	return result;
}
