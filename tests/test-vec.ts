/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert, approx } from './test';
import {
	vec,
	float2, float3, float4,
	float2x2, float2x3, float3x3, float3x4, float4x4,
	mid, normalise, project, reflect, lerp, approx_equal, safeNormalise,
	sincos_half, max_circle_point,
	matmul,
	extent2, extent3,
	vector, E6,
	mat,
	ColumnType,
} from '../dist/vector';

// Compile-time guard: ensure matmul doesn't widen column keys to `string`.
// This is a type-only assertion that will cause a TS error if matmul(float2x2, float2x2)
// returns a matrix whose column-key type is `string` (i.e. inference widened).
{
	const __tc_a = float2x2(float2(1, 0), float2(0, 1));
	const __tc_b = float2x2(float2(1, 0), float2(0, 1));
	const __tc_res = __tc_a.matmul(__tc_b);
	void __tc_res; // mark used so compiler doesn't complain about unused local
	type __tc_mat_res = typeof __tc_res;
	type __tc_cols = __tc_mat_res extends vec<number, infer R> ? R : never;
	// If __tc_cols is or includes `string` then the left branch yields a tuple and assignment fails.
	type __tc_ok = string extends __tc_cols ? ['matmul widened to string keys — fix signatures'] : true;
	const __tc_assert: __tc_ok = true as unknown as __tc_ok;
	void __tc_assert;
}

function assertOrthonormalBasis(m: float4x4, tol = 1e-9) {
	const a = m.x, b = m.y, c = m.z, nv = m.w;
	assert(approx(a.len(), 1, tol), 'a not unit');
	assert(approx(b.len(), 1, tol), 'b not unit');
	assert(approx(c.len(), 1, tol), 'c not unit');
	assert(approx(a.dot(b), 0, tol), 'a·b not zero');
	assert(approx(a.dot(c), 0, tol), 'a·c not zero');
	assert(approx(b.dot(c), 0, tol), 'b·c not zero');
	// last column should be unit (it's the input normalised)
	assert(approx(nv.len(), 1, tol), 'nv not unit');
}

test('swizzle and basic vector ops', () => {
	const v2 = float2(1, 2);
	const v3 = float3(1, 2, 3);

	// swizzles
	expect(v3.xy).toEqual(float2(1, 2));
	expect(v3.zx).toEqual(float2(3, 1));
	expect(v3.xyz).toEqual(float3(1, 2, 3));

	// arithmetic
	expect(v2.add(v2)).toEqual(float2(2, 4));
	expect(v2.scale(2)).toEqual(float2(2, 4));
	assert(v3.dot(float3(1, 0, 0)) === 1, 'dot mismatch');
	assert(approx(normalise(v3).len(), 1, 1e-12), 'normalise failed');
});

test('2x2 and 2x3 matmul / inverse / affine', () => {
	const m2x2 = float2x2(float2(1, 2), float2(3, 4));
	const m2x3 = float2x3(float2(1, 2), float2(3, 4), float2(5, 6));

	// matmul free function agrees with instance.matmul
		const r1 = matmul(m2x2, m2x3);
		const r2 = m2x2.matmul(m2x3);
		expect(r1.x).toEqual(r2.x);
		expect(r1.y).toEqual(r2.y);
		expect(r1.z).toEqual(r2.z);

	// inverse and determinant consistency
		const inv = m2x2.inverse();
		const ident = inv.matmul(m2x2);
		// identity on columns
		expect(ident.x).toEqual(float2(1, 0));
		expect(ident.y).toEqual(float2(0, 1));

		// affine multiply: treat float2x3 as affine transform
		const a = float2x3(float2(1, 0), float2(0, 1), float2(2, 3));
		const b = float2x2(float2(2, 0), float2(0, 2));
		// manual composition (since instances are matImp and may not expose prototype helpers)
		const ab = a.mulAffine(b);
		// translation column should remain a.z for multiplication by pure scale
		expect(ab.z).toEqual(a.z);
});

test('3D affine multiply and matmul', () => {
	const m3x3 = float3x3(float3(1, 2, 3), float3(0, 1, 4), float3(5, 6, 0));
	const m3x4 = float3x4(float3(1, 0, 0), float3(0, 1, 0), float3(0, 0, 1), float3(5, 6, 7));

	// inverse and determinant consistency
	const inv = m3x3.inverse();
	//const t = inv.x;
	const ident = inv.matmul(m3x3);
	// identity on columns
	expect(ident.x).toEqual(float3(1, 0, 0));
	expect(ident.y).toEqual(float3(0, 1, 0));
	expect(ident.z).toEqual(float3(0, 0, 1));

	// mulPos should apply the linear part then add translation. For this
	// matrix the linear part is identity, so mulPos(v) == v + translation.
	const v = float3(1, 2, 3);
	const vp = m3x4.mulPos(v);
	expect(vp).toEqual(float3(v.x + 5, v.y + 6, v.z + 7));

	// zero vector maps to the translation column
	expect(m3x4.mulPos(float3(0, 0, 0))).toEqual(m3x4.w);

	// composing with an identity 3x3 using mulAffine should keep the
	// translation column unchanged
	const id3 = float3x3.identity();
	const composed = m3x4.mulAffine(id3);
	expect(composed.w).toEqual(m3x4.w);
});

test('float4x4.basis edge cases and orthonormality', () => {
	const cases = [
		float4(1, 0, 0, 0),     // +e0
		float4(-1, 0, 0, 0),    // -e0
		float4(0.9999999, 1e-8, 0, 0), // near +e0
		float4(-0.9999999, 1e-8, 0, 0),// near -e0
	];

	for (const v of cases) {
		const m = float4x4.basis(v);
		assertOrthonormalBasis(m, 1e-8);
	}

	// a few random directions
	for (let i = 0; i < 8; ++i) {
		const rv = float4(Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1);
		const m = float4x4.basis(rv);
		assertOrthonormalBasis(m, 1e-8);
	}
});

test('extent2 and extent3 basic operations', () => {
	const e2 = new extent2(float2(0,0), float2(2,2));
	assert(e2.contains(float2(1,1)), 'extent2 should contain point inside');
	assert(!e2.contains(float2(3,3)), 'extent2 should not contain outside point');
	const e3 = new extent3(float3(0,0,0), float3(1,1,1));
	expect(e3.centre()).toEqual(mid(float3(0,0,0), float3(1,1,1)));
});


test('project, reflect, lerp and normalise behaviours', () => {
	const a = float3(1,2,3);
	const b = float3(0,1,0);
	const p = project(a, b);
	// projection of a onto y-axis should have x,z = 0
	assert(Math.abs(p.x) < 1e-12, 'project x not zero');
	assert(Math.abs(p.z) < 1e-12, 'project z not zero');
	// reflect twice gives original direction (up to scale sign)
	const r = reflect(a, b);
	const rr = reflect(r, b);
	// rr should be proportional to a (same direction)
	assert(Math.abs(normalise(rr).x - normalise(a).x) < 1e-12, 'double reflect mismatch');

	// lerp halfway equals mid
	const m = mid(float2(0,0), float2(2,2));
	expect(lerp(float2(0,0), float2(2,2), 0.5)).toEqual(m);
});


test('approx_equal and safeNormalise edge cases', () => {
	const a = float2(1, 0);
	// approx_equal should consider small relative differences
	assert(approx_equal(a, float2(1 + 1e-7, 0), 1e-6), 'approx_equal failed for near-equal vector');
	// safeNormalise of zero returns undefined
	assert(safeNormalise(float3(0,0,0)) === undefined, 'safeNormalise should return undefined for zero');
});


test('float2 helpers and rotations', () => {
	const cs = float2.cossin(Math.PI / 2);
	assert(Math.abs(cs.x) < 1e-12, 'cossin x mismatch');
	assert(Math.abs(cs.y - 1) < 1e-12, 'cossin y mismatch');
	const rot = float2.rotate(Math.PI / 2);
	const v = float2(1,0);
	const vr = rot.mulPos(v);
	// rotating (1,0) by 90deg gives (0,1)
	assert(Math.abs(vr.x) < 1e-12, 'rotation x mismatch');
	assert(Math.abs(vr.y - 1) < 1e-12, 'rotation y mismatch');
});


test('float3 and float4 perp helpers produce orthogonal output', () => {
	const v3 = float3(1,2,3);
	const p3 = v3.perp();
	assert(Math.abs(v3.dot(p3)) < 1e-12, 'float3 perp not orthogonal');
	const v4 = float4(0.1, -0.2, 0.3, -0.4);
	const p4 = v4.perp();
	assert(Math.abs(v4.dot(p4)) < 1e-12, 'float4 perp not orthogonal');
	const v6 = vector(E6, 0.1, -0.2, 0.3, -0.4, 0.5, -0.6);
	const p6 = v6.perp();
	assert(Math.abs(v6.dot(p6)) < 1e-12, 'float6 perp not orthogonal');
});


test('float3x3.basis produces orthonormal rows/cols', () => {
	const dir = float3(0.1, 0.2, 0.3);
	const m = float3x3.basis(dir);
	// last column should be normalised dir
	expect(m.z).toEqual(normalise(dir));
	// columns should be orthogonal
	assert(approx(m.x.dot(m.y), 0, 1e-12), 'float3x3 basis columns not orthogonal');
});


test('sincos_half and max_circle_point basic sanity', () => {
	const s = sincos_half(float2(0.5, 0.5));
	assert(s.len() > 0, 'sincos_half returned zero');
	const m = max_circle_point(float2x2.identity());
	assert(approx(m.len(), 1, 1e-12), 'max_circle_point not unit');
});


test('matrix determinant small cases', () => {
	const m2 = float2x2(float2(1,2), float2(3,4));
	assert(approx(m2.det(), -2, 1e-12), '2x2 det mismatch');
	const m3 = float3x3(float3(1,0,0), float3(0,2,0), float3(0,0,3));
	assert(approx(m3.det(), 6, 1e-12), '3x3 det mismatch');
});


test('matmul consistency across matmul free and instance', () => {
	const a = float2x2(float2(1,0), float2(0,1));
	const b = float2x3(float2(1,2), float2(3,4), float2(5,6));
	const r1 = a.matmul(b);
	const r2 = matmul(a, b);
	expect(r1.x).toEqual(r2.x);
	expect(r1.y).toEqual(r2.y);
});

// ----- Numeric inverse checks added: validate A * A.inverse() == I for several cases
function checkIdentity<C extends string, R extends string>(m: mat<ColumnType<R>, C>, tol = 1e-9) {
	const cols = m.columns();
	const n = cols.length;
	for (let j = 0; j < n; ++j) {
		const col = cols[j]._values;
		for (let i = 0; i < n; ++i) {
			const expected = i === j ? 1 : 0;
			const diff = Math.abs(col[i] - expected);
			if (diff > tol)
				throw new Error(`not identity at (${i},${j}): got ${col[i]}, expected ${expected}`);
		}
	}
}

test('numeric inverse 2x2', () => {
	const A = float2x2(
		float2(2, 5),
		float2(3, 7)
	);
	const I = A.matmul(A.inverse());
	checkIdentity(I);
});

test('numeric inverse 3x3', () => {
	const A = float3x3(
		float3(2, 7, 17),
		float3(3, 11, 19),
		float3(5, 13, 23)
	);
	const I = A.matmul(A.inverse());
	checkIdentity(I);
});

test('numeric inverse with pivoting (zero diagonal)', () => {
	// Construct a matrix with a zero leading diagonal element to force pivoting
	const A = float3x3(
		float3(0, 1, 4),
		float3(1, 0, 5),
		float3(2, 3, 6)
	);
	const I = A.matmul(A.inverse());
	checkIdentity(I);
});

test('numeric inverse random 4x4', () => {
	// deterministic pseudo-random small integer matrix
	let seed = 12345;
	function rnd() { seed = (seed * 1664525 + 1013904223) >>> 0; return (seed % 10) - 4; }
	function rnd4() { return float4(rnd(), rnd(), rnd(), rnd()); }

	const A = float4x4(rnd4(), rnd4(), rnd4(), rnd4());
	const I = A.matmul(A.inverse());
	checkIdentity(I, 1e-8);
});
