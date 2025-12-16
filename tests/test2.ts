/* eslint-disable @typescript-eslint/no-unused-vars */

import { float2, float3 } from '../dist/index';

const a = float2(1, 2);
const b = float3(3, 4, 5);

const sum = a.add(float2(1, 1));     // (2, 3)
const dot = a.dot(float2(2, 3));     // 8
const len = a.len();                 // 2.236...
const swiz = b.xy;                   // float2(3, 4)

const ext = new float3.extent(b, b);

//### Complex Numbers

import {complex} from '../dist/index';

const z1 = complex(3, 4);           // 3 + 4i
const z2 = complex(1, -1);          // 1 - i

const sum2 = z1.add(z2);             // 4 + 3i
const product = z1.mul(z2);         // 7 + i
const magnitude = z1.abs();         // 5
const phase = z1.arg();             // 0.927...
const exp_z = complex.exp(z1);
const sin_z = complex.sin(z1);

//### Polynomials

import { Polynomial } from '../dist/index';

const p = Polynomial([-2, 0, 1]);     // x^2 - 2
const roots = p.realRoots();          // [sqrt(2), -sqrt(2)]
const value = p.evaluate(3);          // 7
const deriv = p.deriv();              // 2x

//### Quaternions

import { unitQuaternion } from '../dist/index';

const q = unitQuaternion.fromAxisAngle(float3(0, 0, 1), Math.PI / 2);
const v = float3(1, 0, 0);
const rotated = q.transform(v); // Rotates v by 90° around z

//### Geometry

import { plane2 } from '../dist/index';

const p0 = float2(0, 0), p1 = float2(1, 1);
const plane = plane2.fromVerts(p0, p1);
const dist = plane.dist(float2(2, 2));

//### Statistics

import { statistics1 } from '../dist/index';

const stats = statistics1.from([1, 2, 3, 4, 5]);
console.log(stats.mean, stats.variance, stats.standardDeviation);

