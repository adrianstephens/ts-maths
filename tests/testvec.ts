/* eslint-disable no-restricted-syntax */

import { expect, test } from './test';
import {float2, float2b, float3} from '../src/vector';

const myVec2 = float2(1,2);
console.log((myVec2 as any)[Symbol.for('debug.description')]);

for (const i of myVec2)
	console.log(i);

const testy = myVec2.yx[1];

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


myVec3.addeq(myVec3);
console.log(myVec3);