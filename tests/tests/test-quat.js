"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
const test_1 = require("./test");
const quaternion_1 = __importStar(require("../dist/quaternion"));
const vector_1 = require("../dist/vector");
const TOL = 1e-12;
function checkNormal(q, msg) {
    (0, test_1.assert)(Math.abs(q.v.len() - 1) < TOL, msg);
}
function checkQuats(a, b, msg) {
    (0, test_1.assert)(a.v.sub(b.v).len() < TOL, msg);
}
(0, test_1.test)('unit_quaternion basic norm invariants', () => {
    const q1 = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(0.1, 0.2, 0.3, 0.9));
    const q2 = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(-0.2, 0.5, 0.1, 0.8));
    checkNormal(q1, 'q1 not unit');
    checkNormal(q2, 'q2 not unit');
    const neg = q1.neg();
    checkNormal(neg, 'neg not unit');
    const conj = q1.conj();
    checkNormal(conj, 'conj not unit');
    const mul = q1.mul(q2);
    checkNormal(mul, 'mul not unit');
    const div = q1.div(q2);
    checkNormal(div, 'div not unit');
    const pw = quaternion_1.unitQuaternion.pow(q1, 0.5);
    checkNormal(pw, 'pow not unit');
    const s = quaternion_1.unitQuaternion.slerp(q1, q2, 0.3);
    checkNormal(s, 'slerp not unit');
});
(0, test_1.test)('quaternion normalise and closest', () => {
    const qn = (0, quaternion_1.default)((0, vector_1.float4)(2, 0, 0, 0.5));
    const un = qn.normalise();
    checkNormal(un, 'normalise did not produce unit');
    const a = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(0.1, 0.2, 0.3, 0.9));
    const b = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(-0.1, -0.2, -0.3, -0.9));
    // a and b are opposites; closest should return a or its neg so dot >= 0
    const c = a.closest(b);
    (0, test_1.assert)(c.v.dot(b.v) >= 0, 'closest did not choose the nearer sign');
});
(0, test_1.test)('unitQuaternion.fromEulerZYX and fromAxisAngle produce unit quaternions', () => {
    const angles = (0, vector_1.float3)(0.3, -0.5, 1.2);
    const qe = quaternion_1.unitQuaternion.fromEulerZYX(angles);
    checkNormal(qe, 'fromEulerZYX result not unit');
    const axis = (0, vector_1.float3)(1, 2, 3);
    const qaa = quaternion_1.unitQuaternion.fromAxisAngle(axis, Math.PI / 3);
    checkNormal(qaa, 'fromAxisAngle result not unit');
    // rotating the axis by the quaternion should leave it unchanged
    const m = qaa.to3x3();
    const ra = m.mul(axis);
    const an = (0, vector_1.normalise)(axis);
    const ran = (0, vector_1.normalise)(ra);
    (0, test_1.assert)(an.sub(ran).len() < TOL, 'axis should be invariant under rotation about itself');
});
(0, test_1.test)('unitQuaternion.toAxisAngle and toEulerZYX round-trip', () => {
    const axis = (0, vector_1.float3)(1, 2, 3);
    const angle = Math.PI * 0.4;
    const q = quaternion_1.unitQuaternion.fromAxisAngle(axis, angle);
    const aa = q.toAxisAngle();
    // axis direction may be flipped when angle==0 but here angle>0
    const an = (0, vector_1.normalise)(axis);
    const aout = (0, vector_1.normalise)(aa.axis);
    (0, test_1.assert)(an.sub(aout).len() < TOL, 'toAxisAngle axis mismatch');
    (0, test_1.assert)(Math.abs(aa.angle - angle) < 1e-12, 'toAxisAngle angle mismatch');
    const angles = (0, vector_1.float3)(0.3, -0.5, 1.2);
    const qe = quaternion_1.unitQuaternion.fromEulerZYX(angles);
    const out = qe.toEulerZYX();
    (0, test_1.assert)(Math.abs(out.x - angles.x) < 1e-12, 'toEulerZYX x mismatch');
    (0, test_1.assert)(Math.abs(out.y - angles.y) < 1e-12, 'toEulerZYX y mismatch');
    (0, test_1.assert)(Math.abs(out.z - angles.z) < 1e-12, 'toEulerZYX z mismatch');
});
(0, test_1.test)('unitQuaternion.pow and log semantics', () => {
    const q = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(0.2, 0.3, 0.4, 0.8));
    const p1 = quaternion_1.unitQuaternion.pow(q, 1);
    checkQuats(p1, q, 'pow(q,1) should equal q');
    const lg = quaternion_1.unitQuaternion.log(q);
    // log of a unit quaternion has scalar part 0 (since ln(|q|)=0)
    // Implementation returns a quaternion with scalar = 0 for unit inputs
    (0, test_1.assert)(Math.abs(lg.v.w) < 1e-12, 'log of unit quaternion should have zero scalar part');
});
(0, test_1.test)('slerp and squad produce unit quaternions', () => {
    const a = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(0.1, 0.2, 0.3, 0.9));
    const b = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(-0.2, 0.5, 0.1, 0.8));
    const s = quaternion_1.unitQuaternion.slerp(a, b, 0.25);
    checkNormal(s, 'slerp not unit');
    const sq = quaternion_1.unitQuaternion.squad(a, a, b, b, 0.3);
    checkNormal(sq, 'squad not unit');
});
(0, test_1.test)('from3x3 round-trip', () => {
    const q = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(0.2, 0.3, 0.4, 0.8));
    const m = q.to3x3();
    const q2 = quaternion_1.unitQuaternion.from3x3(m);
    checkNormal(q2, 'from3x3 result not unit');
    const m2 = q2.to3x3();
    // compare columns component-wise (explicit to avoid any casts)
    (0, test_1.assert)(m.x.sub(m2.x).len() < TOL, 'matrix round-trip mismatch (x)');
    (0, test_1.assert)(m.y.sub(m2.y).len() < TOL, 'matrix round-trip mismatch (y)');
    (0, test_1.assert)(m.z.sub(m2.z).len() < TOL, 'matrix round-trip mismatch (z)');
});
(0, test_1.test)('from3x3 non-orthonormal behavior', () => {
    const q = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(0.2, 0.3, 0.4, 0.8));
    const m = q.to3x3();
    // make non-orthonormal by scaling first column
    const bad = (0, vector_1.float3x3)(m.x.scale(1.5), m.y, m.z);
    const qbad = quaternion_1.unitQuaternion.from3x3(bad);
    const mback = qbad.to3x3();
    // expect the conversion to a quaternion+rotation to NOT reproduce the non-orthonormal input
    const dx = Math.abs(mback.x.x - bad.x.x) + Math.abs(mback.x.y - bad.x.y) + Math.abs(mback.x.z - bad.x.z);
    (0, test_1.assert)(dx > 1e-6, 'from3x3 silently accepted non-orthonormal matrix');
});
(0, test_1.test)('unit_quat.rotate and quat.transform behaviour', () => {
    const uq = (0, quaternion_1.unitQuaternion)((0, vector_1.float4)(0.2, 0.3, 0.4, 0.8));
    const v = (0, vector_1.float3)(1.0, -0.3, 2.5);
    const m = uq.to3x3();
    const mv = m.mul(v);
    const rv = uq.rotate(v);
    (0, test_1.assert)(mv.sub(rv).len() < TOL, 'unitQuaternion.rotate should match to3x3 * v');
    // non-unit quaternion: transform should equal q * v * q.conj()
    const q = (0, quaternion_1.default)((0, vector_1.float4)(0.5, -0.7, 1.1, 0.3));
    const v2 = (0, vector_1.float3)(0.3, -0.2, 2.0);
    const m2 = q.to3x3();
    const mmv = m2.mul(v2);
    const tv = q.transform(v2);
    (0, test_1.assert)(mmv.sub(tv).len() < TOL, 'quaternion.transform should match to3x3 * v (q v q*)');
});
//# sourceMappingURL=test-quat.js.map