import { test, assert,  approxArray } from './test';
import real from '../dist/real';
import { vector, mat, E6, floatN, float3x3, float2, float2x2, float3, float4, float4x4 } from '../dist/vector';


test('faddeevLeVerrier - identity 3x3', () => {
	const I3 = float3x3.identity();
	const p = I3.characteristic();
	const expectCoeffs = [1, -3, 3, -1];
	const coeffs = [1, ...p.c.slice().reverse()];
	assert(approxArray(coeffs, expectCoeffs), `expected ${expectCoeffs}, got ${coeffs}`);
});

test('faddeevLeVerrier - diagonal 2x2', () => {
	// diag(2,3) => polynomial λ^2 - 5λ + 6
	const D = float2x2(float2(2, 0), float2(0, 3));
	const p = D.characteristic();
	const expectCoeffs = [1, -5, 6];
	const coeffs = [1, ...p.c.slice().reverse()];
	assert(approxArray(coeffs, expectCoeffs), `expected ${expectCoeffs}, got ${coeffs}`);
});

test('faddeevLeVerrier - 2x2 example', () => {
	// matrix [[1,2],[3,4]] with columns (1,3) and (2,4)
	const M = float2x2(float2(1, 3), float2(2, 4));
	const p = M.characteristic();
	// characteristic polynomial: λ^2 - 5λ -2
	const expectCoeffs = [1, -5, -2];
	const coeffs = [1, ...p.c.slice().reverse()];
	assert(approxArray(coeffs, expectCoeffs), `expected ${expectCoeffs}, got ${coeffs}`);
});

test('eigen_2x2 - real distinct', () => {
	const A = float2x2(float2(3, 1), float2(0, 2));
	const eigs = A.eigenvalues();
	if (eigs.length !== 2) throw new Error('expected 2 eigenvalues');
	const vals = eigs.map(e => e.r).sort((a,b) => a-b);
	if (!(real.approx(vals[0],2) && real.approx(vals[1],3)))
		throw new Error('real distinct eigenvalues mismatch: ' + JSON.stringify(vals));
});

test('eigen_2x2 - complex conjugate', () => {
	const A = float2x2(float2(0, 2), float2(-2, 0));
	const eigs = A.eigenvalues();
	if (eigs.length !== 2) throw new Error('expected 2 eigenvalues');
	const has2i = eigs.some(e => Math.abs(e.r) < 1e-9 && Math.abs(e.i - 2) < 1e-9);
	const hasMinus2i = eigs.some(e => Math.abs(e.r) < 1e-9 && Math.abs(e.i + 2) < 1e-9);
	if (!has2i || !hasMinus2i) throw new Error('complex conjugate eigenvalues mismatch: ' + JSON.stringify(eigs));
});

test('eigen_3x3 - diag 1,2,3', () => {
	const A = float3x3(float3(1,0,0), float3(0,2,0), float3(0,0,3));
	const eigs = A.eigenvalues();
	if (eigs.length !== 3) throw new Error('expected 3 eigenvalues');
	const vals = eigs.map(e => e.r ?? 0).sort((a,b) => a-b);
	if (!(real.approx(vals[0],1) && real.approx(vals[1],2) && real.approx(vals[2],3)))
		throw new Error('3x3 diagonal eigenvalues mismatch: ' + JSON.stringify(vals));
});

test('eigen_4x4 - diag 1..4', () => {
	const A = float4x4(float4(1,0,0,0), float4(0,2,0,0), float4(0,0,3,0), float4(0,0,0,4));
	const eigs = A.eigenvalues();
	if (eigs.length !== 4) throw new Error('expected 4 eigenvalues');
	const vals = eigs.map(e => e.r).sort((a,b) => a-b);
	if (!(real.approx(vals[0],1) && real.approx(vals[1],2) && real.approx(vals[2],3) && real.approx(vals[3],4)))
		throw new Error('4x4 diagonal eigenvalues mismatch: ' + JSON.stringify(vals));
});

test('eigen_6x6_vector - diagonal', () => {
	const diag = [10, 20, 30, 40, 50, 60];
	// build column-major matrix: columns are floatN vectors of length 6
	const cols: Record<string, floatN> = {};
	for (let j = 0; j < 6; ++j) {
		const vals = Array(6).fill(0);
		vals[j] = diag[j];
		cols[E6[j]] = new floatN(...vals);
	}

	const m = mat(cols);
	const eigs = m.eigenvalues();
	const reals = eigs.map(z => z.r).sort((a, b) => a - b);
	const expected = diag.slice().sort((a, b) => a - b);
	const tol = 1e-8;
	if (reals.length !== expected.length)
		throw new Error(`unexpected eigencount ${reals.length}`);
	for (let i = 0; i < expected.length; ++i) {
		if (Math.abs(reals[i] - expected[i]) > tol)
			throw new Error(`eigenvalue mismatch at ${i}: got ${reals[i]} expected ${expected[i]}`);
	}
});

test('eigen_6x6_vector - diagonal', () => {
    const diag = [10, 20, 30, 40, 50, 60];
    const cols: Record<E6, vector<E6>> = {
        x: vector(E6, 10, 0, 0, 0, 0, 0),
        y: vector(E6, 0, 20, 0, 0, 0, 0),
        z: vector(E6, 0, 0, 30, 0, 0, 0),
        w: vector(E6, 0, 0, 0, 40, 0, 0),
        c5: vector(E6, 0, 0, 0, 0, 50, 0),
        c6: vector(E6, 0, 0, 0, 0, 0, 60),
    };
    const m = mat(cols);
    const eigs = m.eigenvalues();
    assert(eigs.length === 6, `expected 6 eigenvalues got ${eigs.length}`);
    const vals = eigs.map(e => e.r).sort((a, b) => a - b);
    const expected = diag.slice().sort((a, b) => a - b);
    for (let i = 0; i < 6; ++i)
        assert(real.approx(vals[i], expected[i]), `eigen ${i} mismatch ${vals[i]} != ${expected[i]}`);
});

function sortComplexList(arr: {r:number,i:number}[]) {
    return arr
        .map(e => [e.r, e.i] as [number, number])
        .sort((a,b) => a[0] === b[0] ? a[1] - b[1] : a[0] - b[0]);
}

function assertEigenMultiset(eigs: {r:number,i:number}[], expected: {r:number,i:number}[], tol = 1e-8) {
    const s1 = sortComplexList(eigs);
    const s2 = sortComplexList(expected);
    assert(s1.length === s2.length, `expected ${s2.length} eigenvalues got ${s1.length}`);
    for (let i = 0; i < s1.length; ++i) {
        const [r1,i1] = s1[i];
        const [r2,i2] = s2[i];
        assert(real.approx(r1, r2, tol) && real.approx(i1, i2, tol), `eigen ${i} mismatch (${r1},${i1}) != (${r2},${i2})`);
    }
}

// 1) Similarity transform: A = P * D * P^{-1} should have eigenvalues = diagonal of D
test('eigen_6x6_similar - similarity transform preserves diagonal eigenvalues', () => {
    const D = [2,3,4,5,6,7];
    const Pcols = {
        x: vector(E6, 1,0,0,0,0,0),
        y: vector(E6, 1,1,0,0,0,0),
        z: vector(E6, 1,2,1,0,0,0),
        w: vector(E6, 1,3,3,1,0,0),
        c5: vector(E6, 1,4,6,4,1,0),
        c6: vector(E6, 1,5,10,10,5,1),
    };
    const P = mat(Pcols);
    // scale columns of P by diagonal D to get P*D
    const PDcols = {
        x: Pcols.x.scale(D[0]),
        y: Pcols.y.scale(D[1]),
        z: Pcols.z.scale(D[2]),
        w: Pcols.w.scale(D[3]),
        c5: Pcols.c5.scale(D[4]),
        c6: Pcols.c6.scale(D[5]),
    };
    const PD = mat(PDcols);
    const Pinv = P.inverse();
    const A = PD.matmul(Pinv);
    const eigs = A.eigenvalues();
    const expected = D.map(v => ({ r: v, i: 0 }));
    assertEigenMultiset(eigs, expected);
});

// 2) Upper-triangular 6x6: eigenvalues are diagonal entries
test('eigen_6x6_tri - upper-triangular eigenvalues are diagonals', () => {
    const diag = [11, 12, 13, 14, 15, 16];
    // Build upper-triangular columns: column j has non-zero entries for rows 0..j
    const cols = {
        x: vector(E6, diag[0], 1, 2, 3, 4, 5),
        y: vector(E6, 0, diag[1], 6, 7, 8, 9),
        z: vector(E6, 0, 0, diag[2], 10, 11, 12),
        w: vector(E6, 0, 0, 0, diag[3], 13, 14),
        c5: vector(E6, 0, 0, 0, 0, diag[4], 15),
        c6: vector(E6, 0, 0, 0, 0, 0, diag[5]),
    };
    const A = mat(cols);
    const eigs = A.eigenvalues();
    const expected = diag.map(v => ({ r: v, i: 0 }));
    assertEigenMultiset(eigs, expected);
});

// 3) Block-diagonal with a 2x2 rotation block (complex conjugate pair) + 4 real diagonals
test('eigen_6x6_block - block diagonal with 2x2 complex pair', () => {
    const diag = [21, 22, 23, 24];
    // 2x2 rotation block [[0, -1],[1, 0]] has eigenvalues i and -i
    // columns for 6x6 block-diagonal: columns are full 6-length vectors
    const cols = {
        x: vector(E6, diag[0], 0, 0, 0, 0, 0),
        y: vector(E6, 0, diag[1], 0, 0, 0, 0),
        z: vector(E6, 0, 0, diag[2], 0, 0, 0),
        w: vector(E6, 0, 0, 0, diag[3], 0, 0),
		// column 5: first column of 2x2 block -> [0,1] placed at rows c5,c6
		c5: vector(E6, 0, 0, 0, 0, 0, 1),
		// column 6: second column of 2x2 block -> [-1,0]
		c6: vector(E6, 0, 0, 0, 0, -1, 0),
    };
    const A = mat(cols);
    const eigs = A.eigenvalues();
    const expected = [
        { r: diag[0], i: 0 },
        { r: diag[1], i: 0 },
        { r: diag[2], i: 0 },
        { r: diag[3], i: 0 },
        { r: 0, i: 1 },
        { r: 0, i: -1 },
    ];
    assertEigenMultiset(eigs, expected, 1e-7);
});
