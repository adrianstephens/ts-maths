import { hasop, canop, builtinNumber } from './core';
import gen from './gen';
import complex, { complexT, complexOps, complexFor } from './complex';
import { verticalArray, verticalStyles } from './string';

//-----------------------------------------------------------------------------
// Vector/Matrix helpers
//-----------------------------------------------------------------------------

export interface Matrix<T extends canop<'mul'|'add'>> {
	c: T[][];
	rows: number;
	cols: number;

	dup()				: Matrix<T>;
	from(b: T|number)	: Matrix<T>;
	from<U extends hasop<'mul'|'add'|'from'>>(this: Matrix<U>, b: number) : Matrix<T>;
	scale(b: T|number)	: Matrix<T>;
	add(b: Matrix<T>)	: Matrix<T>;
	mul(b: Matrix<T>)	: Matrix<T>;
	div(b: Matrix<T>)	: Matrix<T> | undefined;
	mulVec(b: T[])		: T[];
	nullspace()			: Matrix<T>;
	trace()				: T;
	det()				: T;
	characteristic()	: T[];
	eigenvalues()		: complexFor<T>[];
	eigenVector(e: T)	: T[];
	inverse()			: Matrix<T> | undefined;
	fillHoles(b: T)		: Matrix<T>;
	isZero()			: boolean;
}

function matrixString<T>(c: T[][]) {
	const all = c.map(c => {
		const col = c.map(String);
		const width = Math.max(...col.map(s => s.length));
		for (let i = 0; i < col.length; i++)
			col[i] = col[i].padStart(width / 2, ' ').padEnd(width, ' ');
		return col;
	});
	return verticalArray(Array.from({ length: all[0].length }, (_, r) => all.map(col => col[r]).join('  ')), verticalStyles.bigBraces);
}

class _tempMatN {
	constructor(public c: number[][]) {}
	get rows() { return this.c[0].length; }
	get cols() { return this.c.length; }

	dup()				{ return new _tempMatN(this.c.map(i => i.slice())); }
	mulVec(b: number[])	{
		const r: number[] = [];
		for (const i in b)
			addScaleVecN(r, this.c[i], b[i]);
		return r;
	}

	trace() {
		const n = this.cols;
		let s = this.c[0][0];
		for (let i = 1; i < n; i++)
			s += this.c[i][i];
		return s;
	}
	characteristic() {
		const B = this.dup();
		const n = this.cols;
		const coeffs: number[] = [];

		for (let k = 1; k < n; ++k) {
			const ck = B.trace() / -k;
			coeffs.push(ck);
			for (let i = 0; i < n; i++) {
				B.c[i][i] += ck;
				B.c[i] = this.mulVec(B.c[i]);
			}
		}
		coeffs.push(B.trace() / -n);
		return coeffs.reverse();
	}
	eigenvalues()			{
		const	tol		= 1e-12;
		const	maxIter	= Math.max(1000, 100 * this.cols);
		const	eigs: complex[] = [];

		let		m	= this.cols;
		const 	A	= this.c;
		for (let iter = 0; m > 1 && iter < maxIter; iter++) {
			const a = A[m - 2][m - 2];
			const b = A[m - 1][m - 2];
			const c = A[m - 2][m - 1];
			const d = A[m - 1][m - 1];

			// Check deflation by looking at entry (m-1, m-2)
			if (Math.abs(c) <= tol * (Math.abs(a) + Math.abs(d))) {
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
				A[j][j] -= mu;
			const { Q, R } = QRN(A, m, m);
			const RQ = Matrix(R).mul(Matrix(Q));

			// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
			for (let j = 0; j < m; ++j) {
				for (let i = 0; i < m; ++i)
					A[j][i] = RQ.c[j][i] + (i === j ? mu : 0);
			}
		}

		// If not fully converged, extract remaining eigenvalues from trailing blocks
		while (m > 1) {
			const a = A[m - 2][m - 2];
			const b = A[m - 1][m - 2];
			const c = A[m - 2][m - 1];
			const d = A[m - 1][m - 1];

			if (Math.abs(c) <= tol * (Math.abs(a) + Math.abs(d))) {
				eigs.push(complex(d, 0));
				m -= 1;

			} else {
				const tr = a + d;
				const s = complex.sqrt(tr * tr - 4 * (a * d - b * c));
				eigs.push(complex(tr).add(s), complex(tr).sub(s));
				m -= 2;
			}
		}

		if (m === 1)
			eigs.push(complex((A[0])[0], 0));

		// eigenvalues collected bottom-up; reverse to have original order
		return eigs.reverse();
	}
	
	eigenvector(e: number)	{
		const n = this.cols;
		for (let i = 0; i < n; i++)
			this.c[i][i] -= e;
		return this.nullspace();
	}
	nullspace() {
		const n = this.cols;
		if (n === 0)
			return [];
		const { perm, rank } = LUDecomposeBareiss(this.c, 3);
		if (rank === n)
			return [];

		const nullspace: number[][] = [];
		const invperm	= invertPerm(perm);

		// Back substitution to find nullspace basis
		for (let col = rank; col < n; col++) {
			const vec	= LUbackSubstituteN(this.c, rank, this.c[col]);
			vec[col]	= 1;
			nullspace.push(permute(vec, invperm));
		}

		return new _matN(nullspace);
	}
	det() {
		const { swaps, rank } = LUDecomposeBareiss(this.c);
		return rank < this.cols ? 0 : (swaps & 1 ? -this.c[rank - 1][rank - 1] : this.c[rank - 1][rank - 1]);
	}
	inverse() {
		const { perm, swaps } = LUDecomposeBareiss(this.c);
		const inv	= LUSolveBareissMulti(this.c, Matrix.identity(this.cols, 1).fillHoles(0).c, swaps ? perm : undefined);
		if (inv)
			return new _matN(inv);
	}
	div(b: _tempMatN) {
		const { perm, swaps } = LUDecomposeBareiss(b.c);
		const sol	= LUSolveBareissMulti(b.c, this.c, swaps ? perm : undefined);
		if (sol)
			return new _matN(sol);
	}

	toString(multiline = true) {
		return multiline ? matrixString(this.c) : `(${this.c.join(', ')})`;
	}
}
export function tempMatN(c: number[][]) { return new _tempMatN(c); }

class _matN extends _tempMatN {
	dupTemp()	{ const n = this.rows; return new _tempMatN(this.c.map(c => Array.from({length: n}, (_, i) => c[i] ?? 0))); }
	dup()		{ return new _matN(this.c.map(i => i.slice())); }
	fillHoles(b: number)	{ fillHoles(this.c, this.rows, b); return this; }

	from(b: number) {
		const rows	= this.rows;
		const cols	= this.cols;
		const m		= Array.from({length: cols}, () => Array<number>(rows));
		const n		= Math.min(cols, rows);
		for (let i = 0; i < n; i++)
			m[i][i] = b;
		return new _matN(m);
	}

	scale(b: number)		{ return new _matN(this.c.map(c => scaleVecN(c, b))); }
	add(b: _matN)			{ return new _matN(this.c.map((a, i) => addVecN(a, b.c[i]))); }
	mul(b: _matN)			{ return new _matN(b.c.map(b => this.mulVec(b))); }

	trace() {
		const n = this.cols;
		let s = this.c[0][0] ?? 0;
		for (let i = 1; i < n; i++)
			s += this.c[i][i] ?? 0;
		return s;
	}
	characteristic()		{
		const B = this.dup();
		const n = this.cols;
		const coeffs: number[] = [];

		for (let k = 1; k < n; ++k) {
			const ck = B.trace() / -k;
			coeffs.push(ck);
			for (let i = 0; i < n; i++) {
				B.c[i][i] += ck;
				B.c[i] = this.mulVec(B.c[i]);
			}
		}
		coeffs.push(B.trace() / -n);
		return coeffs.reverse();
	}
	eigenvalues()			{ return this.dupTemp().eigenvalues(); }
	eigenvector(e: number)	{ return this.dupTemp().eigenvector(e); }
	nullspace()				{ return this.dupTemp().nullspace(); }
	det()					{ return this.dupTemp().det(); }
	inverse()				{ return this.dupTemp().inverse(); }
	div(b: _matN)			{ return this.dupTemp().div(b.dupTemp()); }
	isZero()				{ return this.c.every(c => c.every(v => !v)); }
}

class _tempMatT<T extends hasop<'mul'|'add'>> {
	constructor(public c: T[][]) {}
	get rows() { return this.c[0].length; }
	get cols() { return this.c.length; }

	dup()			{ return new _tempMatT(this.c.map(i => i.slice())); }
	mulVec(b: T[]) 	{
		const r: T[] = [];
		for (const i in b)
			addScaleVecT(r, this.c[i], b[i]);
		return r;
	}

	trace()					{
		const n = this.cols;
		let s = this.c[0][0];
		for (let i = 1; i < n; i++)
			s = s.add(this.c[i][i]);
		return s;
	}
	characteristic<U extends hasop<'add'|'mul'|'scale'|'from'>>(this: _tempMatT<U>)	{
		const B = this.dup();
		const n = this.cols;
		const coeffs: U[] = [];

		for (let k = 1; k < n; ++k) {
			const ck = B.trace().scale(-1 / k);
			coeffs.push(ck);
			for (let i = 0; i < n; i++) {
				B.c[i][i] = B.c[i][i].add(ck);
				B.c[i] = this.mulVec(B.c[i]);
			}
		}
		coeffs.push(B.trace().scale(-1 / n));
		return coeffs.reverse();
	}
	eigenvalues<U extends complexOps<U> & hasop<'recip'|'lt'>>(this: _tempMatT<U>)	{
		const 	tol		= 1e-12;
		const	maxIter	= Math.max(1000, 100 * this.cols);
		const	eigs: complexT<U>[] = [];

		let		m = this.cols;
		const	A = this.c;
		for (let iter = 0; m > 1 && iter < maxIter; iter++) {
			const a = A[m - 2][m - 2];
			const b = A[m - 1][m - 2];
			const c = A[m - 2][m - 1];
			const d = A[m - 1][m - 1];

			// Check deflation by looking at subdiagonal entry (row m-1, col m-2) => cols[m-2][m-1]
			if (c.abs().lt(a.abs().add(d.abs()).scale(tol))) {
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
				A[j][j] = A[j][j].sub(mu);
			const { Q, R } = QRT(A, m, m);
			const RQ = Matrix(R).mul(Matrix(Q));

			// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
			for (let j = 0; j < m; ++j) {
				for (let i = 0; i < m; ++i)
					A[j][i] = i == j ? RQ.c[j][i].add(mu) : RQ.c[j][i];
			}
		}

		// If not fully converged, extract remaining eigenvalues from trailing blocks
		while (m > 1) {
			const a = A[m - 2][m - 2];
			const b = A[m - 1][m - 2];
			const c = A[m - 2][m - 1];
			const d = A[m - 1][m - 1];

			if (c.abs().lt(a.abs().add(d.abs()).scale(tol))) {
				eigs.push(complexT(d));
				m -= 1;

			} else {
				const tr = a.add(d);
				const s = complexT.sqrt(tr.mul(tr).sub(a.mul(d).sub(b.mul(c)).scale(4)));
				eigs.push(complexT(tr).add(s), complexT(tr).sub(s));
				m -= 2;
			}
		}

		if (m === 1)
			eigs.push(complexT((A[0])[0]));

		// eigenvalues collected bottom-up; reverse to have original order
		return eigs.reverse();
	}
	eigenvector<U extends bareissOps<U> & hasop<'from'>>(this: _tempMatT<U>, e: U)	{
		const n = this.cols;
		for (let i = 0; i < n; i++)
			this.c[i][i] = this.c[i][i].sub(e);
		return this.nullspace();
	}
	nullspace<U extends bareissOps<U> & hasop<'from', U>>(this: _tempMatT<U>) {
		const n = this.cols;
		if (n === 0)
			return [];
		const { perm, rank } = LUDecomposeBareissT(this.c, 3);
		if (rank === n)
			return [];

		const nullspace: U[][] = [];
		const invperm	= invertPerm(perm);
		const one		= this.c[0][0].from(1);

		// Back substitution to find nullspace basis
		for (let col = rank; col < n; col++) {
			const vec	= LUbackSubstituteT(this.c, rank, this.c[col]);
			vec[col]	= one;
			nullspace.push(permute(vec, invperm));
		}

		return new _matT(nullspace);
	}
	det<U extends bareissOps<U> & hasop<'from', U>>(this: _tempMatT<U>) {
		const { swaps, rank } = LUDecomposeBareissT(this.c);
		return rank < this.cols ? 0 : (swaps & 1 ? -this.c[rank - 1][rank - 1] : this.c[rank - 1][rank - 1]);
	}
	inverse<U extends bareissOps<U> & hasop<'from', U>>(this: _tempMatT<U>) {
		const t		= this.c[0][0];
		const { perm, swaps } = LUDecomposeBareissT(this.c);
		const inv	= LUSolveBareissMultiT(this.c, Matrix.identity(this.cols, t.from(1)).fillHoles(t.from(0)).c, swaps ? perm : undefined);
		if (inv)
			return new _matT(inv);
	}
	div<U extends bareissOps<U> & hasop<'from'>>(this: _tempMatT<U>, b: _tempMatT<U>) {
		const { perm, swaps } = LUDecomposeBareissT(b.c);
		const sol	= LUSolveBareissMultiT(b.c, this.c, swaps ? perm : undefined);
		if (sol)
			return new _matT(sol);
	}

	toString(multiline = true) {
		return multiline ? matrixString(this.c) : `(${this.c.join(', ')})`;
	}

}
export function tempMatT<T extends hasop<'mul'|'add'>>(c: T[][]) { return new _tempMatT(c); }

class _matT<T extends hasop<'mul'|'add'>> extends _tempMatT<T> {
	dupTemp<U extends T & hasop<'sub'>>(this: _matT<U>) : _tempMatT<U>	{
		const n = this.rows;
		const zero = this.c[0][0].sub(this.c[0][0]);
		return new _tempMatT(this.c.map(c => Array.from({length: n}, (_, i) => c[i] ?? zero)));
	}
	dup() 				{ return new _matT<T>(this.c.map(i => i.slice())); }
	fillHoles(b: T) 	{ fillHoles(this.c, this.rows, b); return this; }

	from<U extends hasop<'mul'|'add'|'from'>>(this: _matT<U>, b: number) : _matT<T>;
	from(b: T) : _matT<T>;
	from(b: T|number) {
		if (typeof b === 'number') {
			if (!hasop('from')(this.c[0][0]))
				throw new Error('');
			b = this.c[0][0].from(b);
		}
		const rows	= this.rows;
		const cols	= this.cols;
		const m		= Array.from({length: cols}, () => Array<T>(rows));
		const n		= Math.min(cols, rows);
		for (let i = 0; i < n; i++)
			m[i][i] = b;
		return new _matT(m);
	}

	scale(b: T): _matT<T>;
	//scale<U extends hasop<'scale'>>(this: U, b: number): _matT<T>;
	scale(b: T|number) {
		if (typeof b === 'number') {
			if (!hasop('scale')(this.c[0][0]))
				throw new Error('');
			return new _matT(this.c.map(c => scaleVecTN(c as (T & hasop<'scale'>)[], b)));
		}
		return new _matT(this.c.map(c => scaleVecT(c, b)));
	}

	add(b: _matT<T>) 	{ return new _matT(this.c.map((a, i) => addVecT(a, b.c[i]))); }
	mul(b: _matT<T>) 	{ return new _matT(b.c.map(b => this.mulVec(b))); }

	characteristic<U extends hasop<'add'|'mul'|'scale'|'from'>>(this: _matT<U>)	{
		const B = this.dup();
		const n = this.cols;
		const coeffs: U[] = [];

		for (let k = 1; k < n; ++k) {
			const ck = B.trace().scale(-1 / k);
			coeffs.push(ck);
			for (let i = 0; i < n; i++) {
				B.c[i][i] = B.c[i][i].add(ck);
				B.c[i] = this.mulVec(B.c[i]);
			}
		}
		coeffs.push(B.trace().scale(-1 / n));
		return coeffs.reverse();
	}


	eigenvalues<U extends complexOps<U> & hasop<'recip'|'lt'>>(this: _matT<U>)	{ return this.dupTemp().eigenvalues(); }
	eigenVector<U extends bareissOps<U> & hasop<'from'>>(this: _matT<U>, e: U)	{ return this.dupTemp().eigenvector(e); }
	nullspace<U extends bareissOps<U> & hasop<'from', U>>(this: _matT<U>)		{ return this.dupTemp().nullspace(); }
	det<U extends bareissOps<U> & hasop<'from', U>>(this: _matT<U>)				{ return this.dupTemp().det(); }
	inverse<U extends bareissOps<U> & hasop<'from', U>>(this: _matT<U>)			{ return this.dupTemp().inverse(); }
	div<U extends bareissOps<U> & hasop<'from', U>>(this: _matT<U>, b: _matT<U>){ return this.dupTemp().div(b.dupTemp()); }
	isZero<U extends hasop<'mul'|'add'|'sign'>>(this: _matT<U>)					{ return this.c.every(c => c.every(v => !v.sign())); }
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
	<T extends canop<'mul' | 'add'>>(c: T[][]): Matrix<T>;
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

export function basisVec<T>(width: number, i: number, zero: T, value: T) {
	const v = makeVec(width, zero);
	v[i] = value;
	return v;
}

export function makeMat<T>(width: number, height: number, value: T) {
	return Array.from({length: height}, () => Array.from({length: width}, () => value));
}

export function copyFillHolesVec<T>(v: T[], n: number, t: T) {
	return Array.from({length: n}, (_, i) => v[i] ?? t);
}

//-----------------------------------------------------------------------------
// unexported (internal) helpers
//-----------------------------------------------------------------------------

function fillHoles<T>(A: T[][], n: number, t: T) {
	for (const c of A)
		for (let i = 0; i < n; i++)
			c[i] ??= t;
	return A;
}
/*
function negVecN<T extends builtinNumber>(A: T[]): T[] {
	return A.map(a => -a) as T[];
}
function negVecT<T extends hasop<'neg',T>>(A: T[]): T[] {
	return A.map(a => a.neg());
}
*/
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

// compute T^-1 M T
export function similarityTransform0(M: Matrix<number>, T: Matrix<number>, Tinv: Matrix<number>, Tperm: number[], Trank: number): Matrix<number> {
	return Matrix(T.c.map(i => LUSolveBareiss(Tinv.c, permute(M.mulVec(i), Tperm), Trank) ?? makeVec(T.c.length, 0)));
}

export function similarityTransform(M: Matrix<number>, T: Matrix<number>): Matrix<number> {
	const Tinv				= T.dup();
	const { perm, rank }	= LUDecomposeBareiss(Tinv.c);
	return similarityTransform0(M, T, Tinv, perm, rank);
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
// minimal polynomial
//-----------------------------------------------------------------------------

export function computeMinimalPolynomial<T extends bareissType>(nextVector: () => Generator<T[]>): T[] | undefined {
	const	A: T[][]		= [];
	const	rperm: number[]	= [];
	let		m				= 0;

	for (const v of nextVector()) {
		m = Math.max(m, v.length);

		const n = A.length;
		if (n === 0) {
			A.push(v);

		} else if (typeof v[0] === 'number') {
			const dep = LUBareissIncremental(A as number[][], v as number[], rperm);
			if (dep)
				return LUbackSubstituteN(A as number[][], n, dep).slice(0, n) as T[];

		} else {
			const dep = LUBareissIncrementalT(A as bareissOps<any>[][], v as bareissOps<any>[], rperm);
			if (dep)
				return LUbackSubstituteT(A as bareissOps<any>[][], n, dep).slice(0, n) as T[];
		}

		if (A.length > m)
			break;
	}
}

//-----------------------------------------------------------------------------
// LU Decomposition and Solvers
//-----------------------------------------------------------------------------

export type bareissOps<T> = hasop<'neg'|'add'|'sub'|'mul'|'div'|'recip'|'sign', T>;
export type bareissType = number | bareissOps<any>;

function invertPerm(perm: number[]) {
	const inv: number[] = [];
	for (let i = 0; i < perm.length; i++)
		inv[perm[i]] = i;
	return inv;
}

export function permute<T>(array: T[], perm: number[]) {
	return perm.map(i => array[i]);
}

function LUbackSubstituteN(A: number[][], rank: number, col: number[]): number[] {
	for (let i = rank - 1; i >= 0; i--) {
		let sum = col[i];
		for (let j = i + 1; j < rank; j++)
			sum += A[j][i] * col[j];

		col[i] = -sum / A[i][i];
	}
	return col;
}

function LUbackSubstituteT<T extends bareissOps<T>>(A: T[][], rank: number, col: T[]): T[] {
	for (let i = rank - 1; i >= 0; i--) {
		let sum = col[i];
		for (let j = i + 1; j < rank; j++)
			sum = sum.add(A[j][i].mul(col[j]));

		col[i] = sum.div(A[i][i]).neg();
	}
	return col;
}

export function LUDecomposeBareiss(A: number[][], pivot = 1) {
	const	m		= A.length;
	const	n		= A[0].length;
	const	cperm	= Array.from({length: m}, (_,i) => i);
	const	rperm	= Array.from({length: n}, (_,i) => i);
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

	const rank = Math.min(m, n);
	for (let k = 0; k < rank; k++) {
		if (pivot && !A[k][k]) {
			if (!(((pivot & 1) && pivotCol(k)) || ((pivot & 2) && pivotRow(k))))
				return { swaps, rank: k, perm: cperm, rperm };
		}

		const akk = A[k][k];
		for (let i = k + 1; i < m; i++) {
			const Ai = A[i], Ak = A[k];
			for (let j = k + 1; j < n; j++)
				Ai[j] = (Ai[j] * akk - Ai[k] * Ak[j]) / prev;
		}
		prev = akk;
	}
	return { swaps, rank, perm: cperm, rperm };
}

export function LUDecomposeBareissT<T extends bareissOps<T>>(A: T[][], pivot = 1) {
	const	m		= A.length;
	const	n		= A[0].length;
	const	cperm	= Array.from({length: m}, (_,i) => i);
	const	rperm	= Array.from({length: n}, (_,i) => i);
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

	const rank = Math.min(m, n);
	for (let k = 0; k < rank; k++) {
		if (pivot && A[k][k].sign() === 0) {
			if (!(((pivot & 1) && pivotCol(k)) || ((pivot & 2) && pivotRow(k))))
				return { swaps, rank: k, perm: cperm, rperm };
		}

		const akk = A[k][k];
		for (let i = k + 1; i < m; i++) {
			const Ai = A[i], Ak = A[k];
			for (let j = k + 1; j < n; j++) {
				const t = Ai[j].mul(akk).sub(Ai[k].mul(Ak[j]));
				Ai[j] = prev ? t.div(prev) : t;
			}
		}
		prev = akk;
	}
	return { swaps, rank, perm: cperm, rperm };
}


export function LUBareissIncremental(A: number[][], v: number[], rperm: number[]): number[] | undefined {
	const	n	= A.length;
	const	m	= v.length;
	for (let i = rperm.length; i < m; i++)
		rperm.push(i);

	// Apply all previous elimination steps to the new column
	if (n > 0) {
		v = permute(v, rperm);

		let prev = 1;
		for (let k = 0; k < n; k++) {
			const Ak	= A[k];
			const akk	= Ak[k];
			const mk	= Ak.length;
			for (let j = k + 1; j < mk; j++)
				v[j] = (v[j] * akk - v[k] * Ak[j]) / prev;
			for (let j = mk; j < m; j++)
				v[j] = v[j] * akk / prev;
			
			prev = akk;
		}

		//find pivot
		let row = n;
		while (row < m && !v[row])
			++row;

		if (row === m)
			return v;

		if (row !== n) {
			for (let j = 0; j < n; j++)
				[A[j][n], A[j][row]] = [A[j][row], A[j][n]];
			[v[n], v[row]] = [v[row], v[n]];
			[rperm[n], rperm[row]] = [rperm[row], rperm[n]];
		}
	}
	A.push(v);
}

export function LUBareissIncrementalT<T extends bareissOps<T>>(A: T[][], v: T[], rperm: number[]): T[] | undefined {
	const	n	= A.length;
	const	m	= v.length;
	for (let i = rperm.length; i < m; i++)
		rperm.push(i);

	// Apply all previous elimination steps to the new column
	if (n > 0) {
		v = permute(v, rperm);

		let prev: T | undefined;
		for (let k = 0; k < n; k++) {
			const Ak	= A[k];
			const akk	= Ak[k];
			const mk	= Ak.length;
			for (let j = k + 1; j < mk; j++) {
				const t = v[j].mul(akk).sub(v[k].mul(Ak[j]));
				v[j] = prev ? t.div(prev) : t;
			}
			for (let j = mk; j < m; j++) {
				const t = v[j].mul(akk);
				v[j] = prev ? t.div(prev) : t;
			}
			prev = akk;
		}

		//find pivot
		let row = n;
		while (row < m && !v[row].sign())
			++row;

		if (row === m)
			return v;

		if (row !== n) {
			for (let j = 0; j < n; j++)
				[A[j][n], A[j][row]] = [A[j][row], A[j][n]];
			[v[n], v[row]] = [v[row], v[n]];
			[rperm[n], rperm[row]] = [rperm[row], rperm[n]];
		}
	}
	A.push(v);
}


export function LUSolveBareiss(A: number[][], X: number[], rank?: number) {
	const m		= A.length;
	const n		= A[0].length;
	const Y		= Array<number>(n);

	rank	??= Math.min(m, n);
	if (rank > 0) {
		const inv = Array.from({ length: rank }, (_, k) => 1 / A[k][k]);

		// Forward: solve L * Y = B  (lower-triangular)
		Y[0] = X[0] * inv[0];
		for (let i = 1; i < rank; ++i) {
			const	Ai	= A[i];
			let		s	= Ai[0] * Y[0];
			for (let k = 1; k < i; ++k)
				s += Ai[k] * Y[k] * inv[k - 1];
			Y[i] = (X[i] - s) * A[i - 1][i - 1] / Ai[i];
		}

		// For overdetermined systems, check consistency
		if (rank < m) {
			for (let i = rank; i < m; i++) {
				const	Ai	= A[i];
				let		s	= 0;
				for (let k = 0; k < rank; ++k)
					s += Ai[k] * Y[k];
				if (Math.abs(X[i] - s) > 1e-10)
					return undefined;
			}
		}

		// Backward: solve U * X = Y  (upper-triangular)
		for (let i = rank - 2; i >= 0; --i) {
			const	Ai	= A[i];
			let		s	= 0;
			for (let k = i + 1; k < rank; ++k)
				s += Ai[k] * Y[k];
			Y[i] -= s * inv[i];
		}
	}

	for (let i = rank; i < n; ++i)
		Y[i] = 0;
	return Y;
}

export function LUSolveBareissT<T extends bareissOps<T>>(A: T[][], X: T[], rank?: number) {
	const m		= A.length;
	const n		= A[0].length;
	const zero	= X[0].sub(X[0]);
	const Y		= Array<T>(n);

	rank	??= Math.min(m, n);
	if (rank > 0) {
		const inv	= Array.from({ length: rank }, (_, k) => A[k][k].recip());

		// Forward: solve L * Y = B  (lower-triangular)
		Y[0] = X[0].mul(inv[0]);
		for (let i = 1; i < rank; ++i) {
			const	Ai	= A[i];
			let		s	= Ai[0].mul(Y[0]);
			for (let k = 1; k < i; ++k)
				s = s.add(Ai[k].mul(Y[k]).mul(inv[k - 1]));
			Y[i] = X[i].sub(s).mul(A[i - 1][i - 1]).div(Ai[i]);
		}

		// For overdetermined systems, check consistency
		if (rank < m) {
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
			const	Ai	= A[i];
			let		s	= Ai[i + 1].mul(Y[i + 1]);
			for (let k = i + 2; k < rank; ++k)
				s = s.add(Ai[k].mul(Y[k]));
			Y[i] = Y[i].sub(s.mul(inv[i]));
		}
	}

	for (let i = rank; i < n; ++i)
		Y[i] = zero;
	return Y;
}

//A: LU factorisation, X: transposed
export function LUSolveBareissMulti(A: number[][], X: number[][], perm?: number[]) {
	const m		= A.length;
	const n		= A[0].length;
	const R		= X.length;
	const rank	= perm ? perm.length : Math.min(m, n);
	const inv	= Array.from({ length: rank }, (_, k) => 1 / A[k][k]);
	const Y		= Array.from({ length: n }, () => Array<number>(R));

	// Forward: solve L * Y = B  (lower-triangular)
	for (let r = 0; r < R; ++r)
		Y[0][r] = X[0][r] * inv[0];
	for (let i = 1; i < rank; ++i) {
		const Ai	= A[i];
		const rAii	= A[i - 1][i - 1] / Ai[i];
		for (let r = 0; r < R; ++r) {
			let s = Ai[0] * Y[0][r];
			for (let k = 1; k < i; ++k)
				s += Ai[k] * Y[k][r] * inv[k - 1];
			Y[i][r] = (X[i][r] - s) * rAii;
		}
	}

	// For overdetermined systems, check consistency
	if (m > n) {
		for (let i = rank; i < m; ++i) {
			const Ai = A[i];
			for (let r = 0; r < R; ++r) {
				let s = 0;
				for (let k = 0; k < rank; ++k)
					s += Ai[k] * Y[k][r];
				if (Math.abs(Y[i][r] - s) > 1e-10)
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
				s += Ai[k] * Y[k][r];
			Y[i][r] -= s * rAii;
		}
	}

	if (perm) {
		const invperm = invertPerm(perm);
		return Array.from({ length: n }, (_, i) => {
			const pi = invperm[i];
			return pi === undefined ? makeVec(R, 0) : Array.from({ length: R }, (_, r) => Y[pi][r]);
		});
	}
	return Y;
}

export function LUSolveBareissMultiT<T extends bareissOps<T>>(A: T[][], X: T[][], perm?: number[]) {
	const m		= A.length;
	const n		= A[0].length;
	const R		= X.length;
	const rank	= perm ? perm.length : Math.min(m, n);
	const inv	= Array.from({ length: rank }, (_, k) => A[k][k].recip());
	const Y		= Array.from({ length: n }, () => Array<T>(R));

	// Forward: solve L * Y = B  (lower-triangular)
	for (let r = 0; r < R; ++r)
		Y[0][r] = X[0][r].mul(inv[0]);
	for (let i = 1; i < rank; ++i) {
		const Ai	= A[i];
		const rAii	= A[i - 1][i - 1].div(Ai[i]);
		for (let r = 0; r < R; ++r) {
			let s = Ai[0].mul(Y[0][r]);
			for (let k = 1; k < i; ++k)
				s = s.add(Ai[k].mul(Y[k][r]).mul(inv[k - 1]));
			Y[i][r] = X[i][r].sub(s).mul(rAii);
		}
	}

	// For overdetermined systems, check consistency
	if (m > n) {
		for (let i = rank; i < m; ++i) {
			const Ai = A[i];
			for (let r = 0; r < R; ++r) {
				let s = Ai[0].mul(Y[0][r]);
				for (let k = 1; k < rank; ++k)
					s = s.add(Ai[k].mul(Y[k][r]));
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
			let s = Ai[i + 1].mul(Y[i + 1][r]);
			for (let k = i + 2; k < rank; ++k)
				s = s.add(Ai[k].mul(Y[k][r]));
			Y[i][r] = Y[i][r].sub(s.mul(rAii));
		}
	}

	if (perm) {
		const zero = X[0][0].sub(X[0][0]);
		const invperm = invertPerm(perm);
		return Array.from({ length: n }, (_, i) => {
			const pi = invperm[i];
			return pi === undefined ? makeVec(R, zero) : Array.from({ length: R }, (_, r) => Y[pi][r]);
		});
	}
	return Y;
}
/*
// solve A^T * X = B where B is an array of RHS columns (each length n).
export function LUSolveBareissTranspose(A: number[][], X: number[], rank?: number) {
	const m		= A.length;
	const n		= A[0].length;
	const Y		= Array<number>(m);

	rank	??= Math.min(m, n);
	if (rank > 0) {
		const inv	= Array.from({ length: rank }, (_, k) => 1 / A[k][k]);

		// Forward: solve U^T * Y = B  (upper-triangular)
		Y[0] = X[0] * inv[0];
		for (let i = 1; i < rank; ++i) {
			let s	= 0;
			for (let k = 1; k < i; ++k)
				s += A[k][i] * Y[k] * inv[k - 1];
			Y[i] = (X[i] - s) * A[i - 1][i - 1] / A[i][i];
		}

		// For overdetermined systems, check consistency
		if (rank < n) {
			for (let i = rank; i < n; ++i) {
				if (Math.abs(Y[i]) > 1e-10)
					return undefined;
			}
		}

		// Backward: solve L^T * X = Y  (lower-triangular, unit diagonal)
		for (let i = rank - 2; i >= 0; --i) {
			let s = 0;
			for (let k = i + 1; k < rank; ++k)
				s += A[k][i] * Y[k];
			Y[i] -= s * inv[i];
		}

	}
	return Y;
}

// Multi-RHS version: solve A^T * X = B where B is an array of RHS columns (each length n).
export function LUSolveBareissTransposeMulti(A: number[][], X: number[][], perm?: number[]) {
	const m		= A.length;
	const n		= A[0].length;
	const R		= X.length;
	const rank	= perm ? perm.length : Math.min(m, n);
	const inv	= Array.from({ length: rank }, (_, k) => 1 / A[k][k]);
	const Y		= Array.from({ length: R }, () => Array<number>(m));

	// Forward: solve U^T * Y = B  (upper-triangular)
	for (let r = 0; r < R; ++r)
		Y[r][0] = X[r][0] * inv[0];
	for (let i = 1; i < rank; ++i) {
		const rAii = A[i - 1][i - 1] / A[i][i];
		for (let r = 0; r < R; ++r) {
			const y = Y[r];
			let s = 0;
			for (let k = 1; k < i; ++k)
				s += A[k][i] * y[k] * inv[k - 1];
			y[i] = (X[r][i] - s) * rAii;
		}
	}

	// For overdetermined systems, check consistency
	if (n > m) {
		for (let r = 0; r < R; ++r) {
			for (let i = rank; i < n; ++i) {
				if (Math.abs(Y[r][i]) > 1e-10)
					return undefined;
			}
		}
	}

	// Backward: solve L^T * X = Y  (lower-triangular, unit diagonal)
	for (let i = rank - 2; i >= 0; --i) {
		const rAii = inv[i];
		for (let r = 0; r < R; ++r) {
			const y = Y[r];
			let s = 0;
			for (let k = i + 1; k < rank; ++k)
				s += A[k][i] * y[k];
			y[i] -= s * rAii;
		}
	}

	if (perm) {
		const invperm = invertPerm(perm);
		return Y.map(y => invperm.map(i => y[i]));
	}
	return Y;
}

export function LUSolveBareissTransposeMultiT<T extends bareissOps<T>>(A: T[][], X: T[][], perm?: number[]) {
	const m		= A.length;
	const n		= A[0].length;
	const R		= X.length;
	const rank	= perm ? perm.length : Math.min(m, n);
	const inv	= Array.from({ length: rank }, (_, k) => A[k][k].recip());
	const Y		= Array.from({ length: R }, () => Array<T>(m));

	// Forward: solve U^T * Y = B  (upper-triangular)
	for (let r = 0; r < R; ++r)
		Y[r][0] = X[r][0].mul(inv[0]);
	for (let i = 1; i < rank; ++i) {
		const rAii = A[i - 1][i - 1].div(A[i][i]);
		for (let r = 0; r < R; ++r) {
			const y = Y[r];
			let	s = A[0][i].mul(y[0]);
			for (let k = 1; k < i; ++k)
				s = s.add(A[k][i].mul(y[k]).mul(inv[k - 1]));
			y[i] = X[r][i].sub(s).mul(rAii);
		}
	}

	// For overdetermined systems, check consistency
	if (n > m) {
		for (let r = 0; r < R; ++r) {
			for (let i = rank; i < n; ++i) {
				if (Y[r][i].sign() !== 0)
					return undefined;
			}
		}
	}

	// Backward: solve L^T * X = Y  (lower-triangular)
	for (let i = rank - 2; i >= 0; --i) {
		const rAii = inv[i];
		for (let r = 0; r < R; ++r) {
			const y = Y[r];
			let s = A[i + 1][i].mul(y[i + 1]);
			for (let k = i + 2; k < rank; ++k)
				s = s.add(A[k][i].mul(y[k]));
			y[i] = y[i].sub(s.mul(rAii));
		}
	}

	if (perm) {
		const invperm = invertPerm(perm);
		return Y.map(y => invperm.map(i => y[i]));
	}
	return Y;
}
*/