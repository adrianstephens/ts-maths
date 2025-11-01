import { test, assert } from './test';
import quaternion, { unitQuaternion } from '../dist/quaternion';
import { float4, float3x3, float3, normalise } from '../dist/vector';

const TOL = 1e-12;

function checkNormal(q: unitQuaternion, msg: string) {
	assert(Math.abs(q.v.len() - 1) < TOL, msg);
}
function checkQuats(a: quaternion, b: quaternion, msg: string) {
	assert(a.v.sub(b.v).len() < TOL, msg);
}

test('unit_quaternion basic norm invariants', () => {
    const q1 = unitQuaternion(float4(0.1, 0.2, 0.3, 0.9));
    const q2 = unitQuaternion(float4(-0.2, 0.5, 0.1, 0.8));

    checkNormal(q1,'q1 not unit');
    checkNormal(q2,'q2 not unit');

    const neg = q1.neg();
    checkNormal(neg,'neg not unit');

    const conj = q1.conj();
    checkNormal(conj,'conj not unit');

    const mul = q1.mul(q2);
    checkNormal(mul,'mul not unit');

    const div = q1.div(q2);
    checkNormal(div,'div not unit');

    const pw = unitQuaternion.pow(q1, 0.5);
    checkNormal(pw,'pow not unit');

    const s = unitQuaternion.slerp(q1, q2, 0.3);
    checkNormal(s,'slerp not unit');
});

test('quaternion normalise and closest', () => {
    const qn = quaternion(float4(2, 0, 0, 0.5));
    const un = qn.normalise();
    checkNormal(un,'normalise did not produce unit');

    const a = unitQuaternion(float4(0.1, 0.2, 0.3, 0.9));
    const b = unitQuaternion(float4(-0.1, -0.2, -0.3, -0.9));
    // a and b are opposites; closest should return a or its neg so dot >= 0
    const c = a.closest(b);
    assert(c.v.dot(b.v) >= 0, 'closest did not choose the nearer sign');
});

test('unitQuaternion.fromEulerZYX and fromAxisAngle produce unit quaternions', () => {
    const angles = float3(0.3, -0.5, 1.2);
    const qe = unitQuaternion.fromEulerZYX(angles);
    checkNormal(qe,'fromEulerZYX result not unit');

    const axis = float3(1, 2, 3);
    const qaa = unitQuaternion.fromAxisAngle(axis, Math.PI / 3);
    checkNormal(qaa,'fromAxisAngle result not unit');

    // rotating the axis by the quaternion should leave it unchanged
    const m = qaa.to3x3();
    const ra = m.mul(axis);
    const an = normalise(axis);
    const ran = normalise(ra);
    assert(an.sub(ran).len() < TOL, 'axis should be invariant under rotation about itself');
});

test('unitQuaternion.toAxisAngle and toEulerZYX round-trip', () => {
    const axis = float3(1, 2, 3);
    const angle = Math.PI * 0.4;
    const q = unitQuaternion.fromAxisAngle(axis, angle);
    const aa = q.toAxisAngle();
    // axis direction may be flipped when angle==0 but here angle>0
    const an = normalise(axis);
    const aout = normalise(aa.axis);
    assert(an.sub(aout).len() < TOL, 'toAxisAngle axis mismatch');
    assert(Math.abs(aa.angle - angle) < 1e-12, 'toAxisAngle angle mismatch');

    const angles = float3(0.3, -0.5, 1.2);
    const qe = unitQuaternion.fromEulerZYX(angles);
    const out = qe.toEulerZYX();
    assert(Math.abs(out.x - angles.x) < 1e-12, 'toEulerZYX x mismatch');
    assert(Math.abs(out.y - angles.y) < 1e-12, 'toEulerZYX y mismatch');
    assert(Math.abs(out.z - angles.z) < 1e-12, 'toEulerZYX z mismatch');
});

test('unitQuaternion.pow and log semantics', () => {
    const q = unitQuaternion(float4(0.2, 0.3, 0.4, 0.8));
    const p1 = unitQuaternion.pow(q, 1);
    checkQuats(p1, q, 'pow(q,1) should equal q');

    const lg = unitQuaternion.log(q);
    // log of a unit quaternion has scalar part 0 (since ln(|q|)=0)
    // Implementation returns a quaternion with scalar = 0 for unit inputs
    assert(Math.abs(lg.v.w) < 1e-12, 'log of unit quaternion should have zero scalar part');
});

test('slerp and squad produce unit quaternions', () => {
    const a = unitQuaternion(float4(0.1, 0.2, 0.3, 0.9));
    const b = unitQuaternion(float4(-0.2, 0.5, 0.1, 0.8));
    const s = unitQuaternion.slerp(a, b, 0.25);
    checkNormal(s,'slerp not unit');
    const sq = unitQuaternion.squad(a, a, b, b, 0.3);
    checkNormal(sq,'squad not unit');
});

test('from3x3 round-trip', () => {
    const q = unitQuaternion(float4(0.2, 0.3, 0.4, 0.8));
    const m = q.to3x3();
    const q2 = unitQuaternion.from3x3(m);
    checkNormal(q2,'from3x3 result not unit');

    const m2 = q2.to3x3();
    // compare columns component-wise (explicit to avoid any casts)
    assert(m.x.sub(m2.x).len() < TOL, 'matrix round-trip mismatch (x)');
    assert(m.y.sub(m2.y).len() < TOL, 'matrix round-trip mismatch (y)');
    assert(m.z.sub(m2.z).len() < TOL, 'matrix round-trip mismatch (z)');
});

test('from3x3 non-orthonormal behavior', () => {
    const q = unitQuaternion(float4(0.2, 0.3, 0.4, 0.8));
    const m = q.to3x3();
    // make non-orthonormal by scaling first column
    const bad	= float3x3(m.x.scale(1.5), m.y, m.z);
    const qbad	= unitQuaternion.from3x3(bad);
    const mback	= qbad.to3x3();
    // expect the conversion to a quaternion+rotation to NOT reproduce the non-orthonormal input
    const dx = Math.abs(mback.x.x - bad.x.x) + Math.abs(mback.x.y - bad.x.y) + Math.abs(mback.x.z - bad.x.z);
    assert(dx > 1e-6, 'from3x3 silently accepted non-orthonormal matrix');
});

test('unit_quat.rotate and quat.transform behaviour', () => {
    const uq = unitQuaternion(float4(0.2, 0.3, 0.4, 0.8));
    const v = float3(1.0, -0.3, 2.5);
    const m = uq.to3x3();
    const mv = m.mul(v);
    const rv = uq.rotate(v);
    assert(mv.sub(rv).len() < TOL, 'unitQuaternion.rotate should match to3x3 * v');

    // non-unit quaternion: transform should equal q * v * q.conj()
    const q = quaternion(float4(0.5, -0.7, 1.1, 0.3));
    const v2 = float3(0.3, -0.2, 2.0);
    const m2 = q.to3x3();
    const mmv = m2.mul(v2);
    const tv = q.transform(v2);
    assert(mmv.sub(tv).len() < TOL, 'quaternion.transform should match to3x3 * v (q v q*)');
});
