import { scalar, has } from './core';

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
export function LUSolveBareissMultiT<T extends scalar<T> & has<'recip'>>(A: T[][], X: T[][], perm?: number[]) {
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
