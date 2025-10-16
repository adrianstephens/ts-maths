
/* eslint-disable no-restricted-syntax */

import { sincos, sin2cos } from "./core";
import { float4, float3x3, float3 } from "./vector";

// Concatenate a float3 and a scalar into a float4
export function concat(v: float3, w: number): float4 {
	return float4(v.x, v.y, v.z, w);
}

class _quat {
	constructor(public v: float4) {}

	neg(): 		quaternion		{ return quaternion(this.v.neg()); }
	conj(): 	quaternion		{ return quaternion(this.v.mul(float4(-1,-1,-1,+1))); }

	mul(b: quaternion): quaternion	{
		return quaternion.fromVS(b.v.xyz.scale(this.v.w).add(this.v.xyz.scale(b.v.w)).sub(this.v.xyz.cross(b.v.xyz)), this.v.w * b.v.w - this.v.xyz.dot(b.v.xyz));
	}
	div(b: quaternion) { return b.conj().mul(this); }

	to3x3(): float3x3 {
		const	v2	= this.v.add(this.v);
		const	d	= v2.xyz.mul(this.v.xyz);
		const	t	= v2.xyz.mul(this.v.yzx);
		const	u	= v2.xyz.mul(this.v.www);
		const	a	= t.xyz.sub(u.zxy);
		const	b	= t.zxy.add(u.yzx);

		const e	= float3(1,1,1).sub(d.yzx).sub(d.zxy);
		return float3x3(
			float3(e.x, a.x, b.x),
			float3(b.y, e.y, a.y),
			float3(a.z, b.z, e.z)
		);
	}
	toString()	{ return this.v.toString(); }
}

export type quaternion = _quat;

export const quaternion = Object.assign(
	function (v: float4) {
		return new _quat(v);
	},
	{// statics
	fromVS(v: float3, w: number): quaternion {
    	return quaternion(concat(v, w));
	},
	from3x3(mat: float3x3) : quaternion {
		const {x, y, z} = mat;

		const	a	= float4(1, z.y, x.z, y.x);
		const	b	= float4(x.x, -y.z, 0, 0);
		const	c	= float4(y.y, 0, -z.x, 0);
		const	d	= float4(z.z, 0, 0, -x.y);

		let	q: float4;
		q	= a.add(b).add(c).add(d);
		if (q.x > 1)
			return quaternion(q.yzwx.scale(0.5 / Math.sqrt(q.x)));

		q	= a.add(b).sub(c).sub(d);
		if (q.x > 1)
			return quaternion(q.xwzy.scale(0.5 / Math.sqrt(q.x)));

		q	= a.sub(b).add(c).sub(d);
		if (q.x > 1)
			return quaternion(q.wxyz.scale(0.5 / Math.sqrt(q.x)));

		q	= a.sub(b).sub(c).add(d);
		return quaternion(q.zyxw.scale(0.5 / Math.sqrt(q.x)));

	},
	pow(q: quaternion, y: number) {
		const	t	= q.v.xyz.len();
		const	s	= Math.sin(Math.asin(t) * y);
		return quaternion.fromVS(q.v.xyz.scale(t ? s / t : 1), sin2cos(s));
	},
	exp(q: quaternion) {
		const	t	= q.v.xyz.len();
		const	sc	= sincos(t);
		return quaternion(concat(q.v.xyz.scale(sc.s / t), sc.c).scale(Math.exp(q.v.w)));
	},
	log(q: quaternion): quaternion {
		const	v		= q.v.xyz;
		const	vlen	= v.len();
		const	qnorm	= q.v.len();
		const	scale	= vlen > 0 ? Math.acos(q.v.w / qnorm) / vlen : 0;
		return quaternion.fromVS(v.scale(scale), Math.log(qnorm));
	}
});

export default quaternion;
