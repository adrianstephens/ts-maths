"use strict";
/* eslint-disable @typescript-eslint/no-unused-vars */
Object.defineProperty(exports, "__esModule", { value: true });
const index_1 = require("../dist/index");
const a = (0, index_1.float2)(1, 2);
const b = (0, index_1.float3)(3, 4, 5);
const sum = a.add((0, index_1.float2)(1, 1)); // (2, 3)
const dot = a.dot((0, index_1.float2)(2, 3)); // 8
const len = a.len(); // 2.236...
const swiz = b.xy; // float2(3, 4)
const ext = new index_1.float3.extent(b, b);
//### Complex Numbers
const index_2 = require("../dist/index");
const z1 = (0, index_2.complex)(3, 4); // 3 + 4i
const z2 = (0, index_2.complex)(1, -1); // 1 - i
const sum2 = z1.add(z2); // 4 + 3i
const product = z1.mul(z2); // 7 + i
const magnitude = z1.abs(); // 5
const phase = z1.arg(); // 0.927...
const exp_z = index_2.complex.exp(z1);
const sin_z = index_2.complex.sin(z1);
//### Polynomials
const index_3 = require("../dist/index");
const p = (0, index_3.Polynomial)([-2, 0, 1]); // x^2 - 2
const roots = p.realRoots(); // [sqrt(2), -sqrt(2)]
const value = p.evaluate(3); // 7
const deriv = p.deriv(); // 2x
//### Quaternions
const index_4 = require("../dist/index");
const q = index_4.unitQuaternion.fromAxisAngle((0, index_1.float3)(0, 0, 1), Math.PI / 2);
const v = (0, index_1.float3)(1, 0, 0);
const rotated = q.transform(v); // Rotates v by 90° around z
//### Geometry
const index_5 = require("../dist/index");
const p0 = (0, index_1.float2)(0, 0), p1 = (0, index_1.float2)(1, 1);
const plane = index_5.plane2.fromVerts(p0, p1);
const dist = plane.dist((0, index_1.float2)(2, 2));
//### Statistics
const index_6 = require("../dist/index");
const stats = index_6.statistics1.from([1, 2, 3, 4, 5]);
console.log(stats.mean, stats.variance, stats.standardDeviation);
//# sourceMappingURL=test2.js.map