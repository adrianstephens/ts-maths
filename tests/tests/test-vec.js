"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable @typescript-eslint/no-unused-vars */
const test_1 = require("./test");
const real_1 = __importDefault(require("../dist/real"));
const vector_1 = require("../dist/vector");
// Compile-time guard: ensure matmul doesn't widen column keys to `string`.
// This is a type-only assertion that will cause a TS error if matmul(float2x2, float2x2)
// returns a matrix whose column-key type is `string` (i.e. inference widened).
{
    const __tc_a = (0, vector_1.float2x2)((0, vector_1.float2)(1, 0), (0, vector_1.float2)(0, 1));
    const __tc_b = (0, vector_1.float2x2)((0, vector_1.float2)(1, 0), (0, vector_1.float2)(0, 1));
    const __tc_res = __tc_a.matmul(__tc_b);
    void __tc_res; // mark used so compiler doesn't complain about unused local
    const __tc_assert = true;
    void __tc_assert;
}
function assertOrthonormalBasis(m, tol = 1e-9) {
    const a = m.x, b = m.y, c = m.z, nv = m.w;
    (0, test_1.assert)(real_1.default.approx(a.len(), 1, tol), 'a not unit');
    (0, test_1.assert)(real_1.default.approx(b.len(), 1, tol), 'b not unit');
    (0, test_1.assert)(real_1.default.approx(c.len(), 1, tol), 'c not unit');
    (0, test_1.assert)(real_1.default.approx(a.dot(b), 0, tol), 'a·b not zero');
    (0, test_1.assert)(real_1.default.approx(a.dot(c), 0, tol), 'a·c not zero');
    (0, test_1.assert)(real_1.default.approx(b.dot(c), 0, tol), 'b·c not zero');
    // last column should be unit (it's the input normalised)
    (0, test_1.assert)(real_1.default.approx(nv.len(), 1, tol), 'nv not unit');
}
(0, test_1.test)('swizzle and basic vector ops', () => {
    const v2 = (0, vector_1.float2)(1, 2);
    const v3 = (0, vector_1.float3)(1, 2, 3);
    // swizzles
    (0, test_1.expect)(v3.xy).toEqual((0, vector_1.float2)(1, 2));
    (0, test_1.expect)(v3.zx).toEqual((0, vector_1.float2)(3, 1));
    (0, test_1.expect)(v3.xyz).toEqual((0, vector_1.float3)(1, 2, 3));
    // arithmetic
    (0, test_1.expect)(v2.add(v2)).toEqual((0, vector_1.float2)(2, 4));
    (0, test_1.expect)(v2.scale(2)).toEqual((0, vector_1.float2)(2, 4));
    (0, test_1.assert)(v3.dot((0, vector_1.float3)(1, 0, 0)) === 1, 'dot mismatch');
    (0, test_1.assert)(real_1.default.approx((0, vector_1.normalise)(v3).len(), 1, 1e-12), 'normalise failed');
});
(0, test_1.test)('2x2 and 2x3 matmul / inverse / affine', () => {
    const m2x2 = (0, vector_1.float2x2)((0, vector_1.float2)(1, 2), (0, vector_1.float2)(3, 4));
    const m2x3 = (0, vector_1.float2x3)((0, vector_1.float2)(1, 2), (0, vector_1.float2)(3, 4), (0, vector_1.float2)(5, 6));
    // matmul free function agrees with instance.matmul
    const r1 = (0, vector_1.matmul)(m2x2, m2x3);
    const r2 = m2x2.matmul(m2x3);
    (0, test_1.expect)(r1.x).toEqual(r2.x);
    (0, test_1.expect)(r1.y).toEqual(r2.y);
    (0, test_1.expect)(r1.z).toEqual(r2.z);
    // inverse and determinant consistency
    const inv = m2x2.inverse();
    const ident = inv.matmul(m2x2);
    // identity on columns
    (0, test_1.expect)(ident.x).toEqual((0, vector_1.float2)(1, 0));
    (0, test_1.expect)(ident.y).toEqual((0, vector_1.float2)(0, 1));
    // affine multiply: treat float2x3 as affine transform
    const a = (0, vector_1.float2x3)((0, vector_1.float2)(1, 0), (0, vector_1.float2)(0, 1), (0, vector_1.float2)(2, 3));
    const b = (0, vector_1.float2x2)((0, vector_1.float2)(2, 0), (0, vector_1.float2)(0, 2));
    // manual composition (since instances are matImp and may not expose prototype helpers)
    const ab = a.mulAffine(b);
    // translation column should remain a.z for multiplication by pure scale
    (0, test_1.expect)(ab.z).toEqual(a.z);
});
(0, test_1.test)('3D affine multiply and matmul', () => {
    const m3x3 = (0, vector_1.float3x3)((0, vector_1.float3)(1, 2, 3), (0, vector_1.float3)(0, 1, 4), (0, vector_1.float3)(5, 6, 0));
    const m3x4 = (0, vector_1.float3x4)((0, vector_1.float3)(1, 0, 0), (0, vector_1.float3)(0, 1, 0), (0, vector_1.float3)(0, 0, 1), (0, vector_1.float3)(5, 6, 7));
    // inverse and determinant consistency
    const inv = m3x3.inverse();
    //const t = inv.x;
    const ident = inv.matmul(m3x3);
    // identity on columns
    (0, test_1.expect)(ident.x).toEqual((0, vector_1.float3)(1, 0, 0));
    (0, test_1.expect)(ident.y).toEqual((0, vector_1.float3)(0, 1, 0));
    (0, test_1.expect)(ident.z).toEqual((0, vector_1.float3)(0, 0, 1));
    // mulPos should apply the linear part then add translation. For this
    // matrix the linear part is identity, so mulPos(v) == v + translation.
    const v = (0, vector_1.float3)(1, 2, 3);
    const vp = m3x4.mulPos(v);
    (0, test_1.expect)(vp).toEqual((0, vector_1.float3)(v.x + 5, v.y + 6, v.z + 7));
    // zero vector maps to the translation column
    (0, test_1.expect)(m3x4.mulPos((0, vector_1.float3)(0, 0, 0))).toEqual(m3x4.w);
    // composing with an identity 3x3 using mulAffine should keep the
    // translation column unchanged
    const id3 = vector_1.float3x3.identity();
    const composed = m3x4.mulAffine(id3);
    (0, test_1.expect)(composed.w).toEqual(m3x4.w);
});
(0, test_1.test)('float4x4.basis edge cases and orthonormality', () => {
    const cases = [
        (0, vector_1.float4)(1, 0, 0, 0), // +e0
        (0, vector_1.float4)(-1, 0, 0, 0), // -e0
        (0, vector_1.float4)(0.9999999, 1e-8, 0, 0), // near +e0
        (0, vector_1.float4)(-0.9999999, 1e-8, 0, 0), // near -e0
    ];
    for (const v of cases) {
        const m = vector_1.float4x4.basis(v);
        assertOrthonormalBasis(m, 1e-8);
    }
    // a few random directions
    for (let i = 0; i < 8; ++i) {
        const rv = (0, vector_1.float4)(Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1);
        const m = vector_1.float4x4.basis(rv);
        assertOrthonormalBasis(m, 1e-8);
    }
});
(0, test_1.test)('extent2 and extent3 basic operations', () => {
    const e2 = new vector_1.float2.extent((0, vector_1.float2)(0, 0), (0, vector_1.float2)(2, 2));
    (0, test_1.assert)(e2.contains((0, vector_1.float2)(1, 1)), 'extent2 should contain point inside');
    (0, test_1.assert)(!e2.contains((0, vector_1.float2)(3, 3)), 'extent2 should not contain outside point');
    const e3 = new vector_1.float3.extent((0, vector_1.float3)(0, 0, 0), (0, vector_1.float3)(1, 1, 1));
    (0, test_1.expect)(e3.centre()).toEqual((0, vector_1.mid)((0, vector_1.float3)(0, 0, 0), (0, vector_1.float3)(1, 1, 1)));
});
(0, test_1.test)('project, reflect, lerp and normalise behaviours', () => {
    const a = (0, vector_1.float3)(1, 2, 3);
    const b = (0, vector_1.float3)(0, 1, 0);
    const p = (0, vector_1.project)(a, b);
    // projection of a onto y-axis should have x,z = 0
    (0, test_1.assert)(Math.abs(p.x) < 1e-12, 'project x not zero');
    (0, test_1.assert)(Math.abs(p.z) < 1e-12, 'project z not zero');
    // reflect twice gives original direction (up to scale sign)
    const r = (0, vector_1.reflect)(a, b);
    const rr = (0, vector_1.reflect)(r, b);
    // rr should be proportional to a (same direction)
    (0, test_1.assert)(Math.abs((0, vector_1.normalise)(rr).x - (0, vector_1.normalise)(a).x) < 1e-12, 'double reflect mismatch');
    // lerp halfway equals mid
    const m = (0, vector_1.mid)((0, vector_1.float2)(0, 0), (0, vector_1.float2)(2, 2));
    (0, test_1.expect)((0, vector_1.lerp)((0, vector_1.float2)(0, 0), (0, vector_1.float2)(2, 2), 0.5)).toEqual(m);
});
(0, test_1.test)('approx_equal and safeNormalise edge cases', () => {
    const a = (0, vector_1.float2)(1, 0);
    // approx_equal should consider small relative differences
    (0, test_1.assert)((0, vector_1.approx_equal)(a, (0, vector_1.float2)(1 + 1e-7, 0), 1e-6), 'approx_equal failed for near-equal vector');
    // safeNormalise of zero returns undefined
    (0, test_1.assert)((0, vector_1.safeNormalise)((0, vector_1.float3)(0, 0, 0)) === undefined, 'safeNormalise should return undefined for zero');
});
(0, test_1.test)('float2 helpers and rotations', () => {
    const cs = vector_1.float2.cossin(Math.PI / 2);
    (0, test_1.assert)(Math.abs(cs.x) < 1e-12, 'cossin x mismatch');
    (0, test_1.assert)(Math.abs(cs.y - 1) < 1e-12, 'cossin y mismatch');
    const rot = vector_1.float2.rotate(Math.PI / 2);
    const v = (0, vector_1.float2)(1, 0);
    const vr = rot.mulPos(v);
    // rotating (1,0) by 90deg gives (0,1)
    (0, test_1.assert)(Math.abs(vr.x) < 1e-12, 'rotation x mismatch');
    (0, test_1.assert)(Math.abs(vr.y - 1) < 1e-12, 'rotation y mismatch');
});
(0, test_1.test)('float3 and float4 perp helpers produce orthogonal output', () => {
    const v3 = (0, vector_1.float3)(1, 2, 3);
    const p3 = v3.perp();
    (0, test_1.assert)(Math.abs(v3.dot(p3)) < 1e-12, 'float3 perp not orthogonal');
    const v4 = (0, vector_1.float4)(0.1, -0.2, 0.3, -0.4);
    const p4 = v4.perp();
    (0, test_1.assert)(Math.abs(v4.dot(p4)) < 1e-12, 'float4 perp not orthogonal');
    const v6 = (0, vector_1.vector)(vector_1.E6, 0.1, -0.2, 0.3, -0.4, 0.5, -0.6);
    const p6 = v6.perp();
    (0, test_1.assert)(Math.abs(v6.dot(p6)) < 1e-12, 'float6 perp not orthogonal');
});
(0, test_1.test)('float3x3.basis produces orthonormal rows/cols', () => {
    const dir = (0, vector_1.float3)(0.1, 0.2, 0.3);
    const m = vector_1.float3x3.basis(dir);
    // last column should be normalised dir
    (0, test_1.expect)(m.z).toEqual((0, vector_1.normalise)(dir));
    // columns should be orthogonal
    (0, test_1.assert)(real_1.default.approx(m.x.dot(m.y), 0, 1e-12), 'float3x3 basis columns not orthogonal');
});
(0, test_1.test)('sincos_half and max_circle_point basic sanity', () => {
    const s = (0, vector_1.sincos_half)((0, vector_1.float2)(0.5, 0.5));
    (0, test_1.assert)(s.len() > 0, 'sincos_half returned zero');
    const m = (0, vector_1.max_circle_point)(vector_1.float2x2.identity());
    (0, test_1.assert)(real_1.default.approx(m.len(), 1, 1e-12), 'max_circle_point not unit');
});
(0, test_1.test)('matrix determinant small cases', () => {
    const m2 = (0, vector_1.float2x2)((0, vector_1.float2)(1, 2), (0, vector_1.float2)(3, 4));
    (0, test_1.assert)(real_1.default.approx(m2.det(), -2, 1e-12), '2x2 det mismatch');
    const m3 = (0, vector_1.float3x3)((0, vector_1.float3)(1, 0, 0), (0, vector_1.float3)(0, 2, 0), (0, vector_1.float3)(0, 0, 3));
    (0, test_1.assert)(real_1.default.approx(m3.det(), 6, 1e-12), '3x3 det mismatch');
});
(0, test_1.test)('matmul consistency across matmul free and instance', () => {
    const a = (0, vector_1.float2x2)((0, vector_1.float2)(1, 0), (0, vector_1.float2)(0, 1));
    const b = (0, vector_1.float2x3)((0, vector_1.float2)(1, 2), (0, vector_1.float2)(3, 4), (0, vector_1.float2)(5, 6));
    const r1 = a.matmul(b);
    const r2 = (0, vector_1.matmul)(a, b);
    (0, test_1.expect)(r1.x).toEqual(r2.x);
    (0, test_1.expect)(r1.y).toEqual(r2.y);
});
// ----- Numeric inverse checks added: validate A * A.inverse() == I for several cases
function checkIdentity(m, tol = 1e-9) {
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
(0, test_1.test)('numeric inverse 2x2', () => {
    const A = (0, vector_1.float2x2)((0, vector_1.float2)(2, 5), (0, vector_1.float2)(3, 7));
    const I = A.matmul(A.inverse());
    checkIdentity(I);
});
(0, test_1.test)('numeric inverse 3x3', () => {
    const A = (0, vector_1.float3x3)((0, vector_1.float3)(2, 7, 17), (0, vector_1.float3)(3, 11, 19), (0, vector_1.float3)(5, 13, 23));
    const I = A.matmul(A.inverse());
    checkIdentity(I);
});
(0, test_1.test)('numeric inverse with pivoting (zero diagonal)', () => {
    // Construct a matrix with a zero leading diagonal element to force pivoting
    const A = (0, vector_1.float3x3)((0, vector_1.float3)(0, 1, 4), (0, vector_1.float3)(1, 0, 5), (0, vector_1.float3)(2, 3, 6));
    const I = A.matmul(A.inverse());
    checkIdentity(I);
});
(0, test_1.test)('numeric inverse random 4x4', () => {
    // deterministic pseudo-random small integer matrix
    let seed = 12345;
    function rnd() { seed = (seed * 1664525 + 1013904223) >>> 0; return (seed % 10) - 4; }
    function rnd4() { return (0, vector_1.float4)(rnd(), rnd(), rnd(), rnd()); }
    const A = (0, vector_1.float4x4)(rnd4(), rnd4(), rnd4(), rnd4());
    const I = A.matmul(A.inverse());
    checkIdentity(I, 1e-8);
});
//# sourceMappingURL=test-vec.js.map