/* eslint-disable no-restricted-syntax */

import { expect, test } from './test';
import {vec, E2, float2, float3, float2x2} from '../src/vector';
import {bezier2} from '../src/geometry';


const b = new bezier2<float2>(float2(0,0), float2(1,1), float2(2,0));
const c = b.split(0.5);
const d = c[0];
console.log(d);

const myVec2 = float2(1,2);
const myVec3 = float3(1,2,3);

test('swizzle properties', () => {
	expect(myVec3.xy).toEqual(float2(1, 2));
	expect(myVec3.xz).toEqual(float2(1, 3));
	expect(myVec3.yx).toEqual(float2(2, 1));
	expect(myVec3.yz).toEqual(float2(2, 3));
	expect(myVec3.zx).toEqual(float2(3, 1));
	expect(myVec3.zy).toEqual(float2(3, 2));
  
	expect(myVec3.xyz).toEqual(float3(1, 2, 3));
	expect(myVec3.xzy).toEqual(float3(1, 3, 2));
	expect(myVec3.yxz).toEqual(float3(2, 1, 3));
	expect(myVec3.yzx).toEqual(float3(2, 3, 1));
	expect(myVec3.zxy).toEqual(float3(3, 1, 2));
	expect(myVec3.zyx).toEqual(float3(3, 2, 1));
});


myVec3.selfAdd(myVec3);
console.log(myVec3);

const m2x2 = float2x2(float2(1,2), float2(3,4));
console.log(m2x2);
const m2x2i = m2x2.inverse();
console.log(m2x2i);
const mtest: vec<float2, E2> = m2x2;
console.log(m2x2i.matmul(mtest));
console.log(m2x2i.matmul(m2x2 as vec<float2, E2>));
