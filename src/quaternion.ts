
import { sincos, sin2cos } from "./core";
import { float4, float3x3, float3, normalise } from "./vector";

// Concatenate a float3 and a scalar into a float4
function concat(v: float3, w: number): float4 {
	return float4(v.x, v.y, v.z, w);
}

class _quat {
	constructor(public v: float4) {}
	create(v: float4): this { return new (this.constructor as any)(v); }

	neg(): 		this		{ return this.create(this.v.neg()); }
	conj(): 	this		{ return this.create(this.v.mul(float4(-1,-1,-1,+1))); }

	add(b: this)			{ return quaternion(this.v.add(b.v)); }
	sub(b: this)			{ return quaternion(this.v.sub(b.v)); }
	mul(b: this): this	{
		return this.create(concat(
			b.v.xyz.scale(this.v.w).add(this.v.xyz.scale(b.v.w)).sub(this.v.xyz.cross(b.v.xyz)),
			this.v.w * b.v.w - this.v.xyz.dot(b.v.xyz)
		));
	}
	div(b: this): this		{ return b.conj().mul(this); }

	scale(n: number) 		{ return new _quat(this.v.scale(n)); }
	recip():		this	{ const s = 1 / this.magsq(); return this.create(this.v.mul(float4(-s, -s, -s, +s))); }
	mag()					{ return this.v.len(); }
	magsq()					{ return this.v.lensq(); }
	normalise()				{ return unitQuaternion(this.v); }
	closest(b: quaternion): this	{ return this.v.dot(b.v) < 0 ? this.neg() : this; }
	
	to3x3(): float3x3 {
		const	v2	= this.v.scale(2);
		const	t	= v2.xyz.mul(this.v.yzx);
		const	u	= v2.xyz.mul(this.v.www);
		const	a	= t.xyz.sub(u.zxy);
		const	b	= t.zxy.add(u.yzx);

		const	d	= this.v.mul(this.v);
		const 	e	= d.www.add(d.xyz).sub(d.yzx).sub(d.zxy);
		return float3x3(
			float3(e.x, a.x, b.x),
			float3(b.y, e.y, a.y),
			float3(a.z, b.z, e.z)
		);
	}

	transform(v: float3): float3 {
		const v0 = this.v.xyz;
		const w0 = this.v.w;

		const v1 = v.scale(w0).sub(v0.cross(v));
		const w1 = v0.dot(v);

		return v0.scale(w1).add(v1.scale(w0)).add(v1.cross(v0));
	}

	toString()	{ return this.v.toString(); }
}

export type quaternion = _quat;
export const quaternion = Object.assign(
	function (v: float4) {
		return new _quat(v);
	},
	{// statics
	exp(q: quaternion) {
		const	t		= q.v.xyz.len();
		const	sc		= sincos(t);
		return quaternion(concat(q.v.xyz.scale(sc.s / t), sc.c).scale(Math.exp(q.v.w)));
	},
	log(q: quaternion): quaternion {
		const	v		= q.v.xyz;
		const	vlen	= v.len();
		const	qnorm	= q.v.len();
		const	scale	= vlen > 0 ? Math.acos(q.v.w / qnorm) / vlen : 0;
		return quaternion(concat(v.scale(scale), Math.log(qnorm)));
	},
});

export default quaternion;

class _unitQuat extends _quat {
	recip():	this	{ return this.conj(); }
	cosang(b: this):	number	{ return this.v.dot(b.v); }

	to3x3(): float3x3 {
		const	v2	= this.v.scale(2);
		const	t	= v2.xyz.mul(this.v.yzx);
		const	u	= v2.xyz.mul(this.v.www);
		const	a	= t.xyz.sub(u.zxy);
		const	b	= t.zxy.add(u.yzx);

		const	d	= v2.xyz.mul(this.v.xyz);
		const	e	= float3(1,1,1).sub(d.yzx).sub(d.zxy);
		return float3x3(
			float3(e.x, a.x, b.x),
			float3(b.y, e.y, a.y),
			float3(a.z, b.z, e.z)
		);
	}

	rotate(v: float3): float3 {
		const p = this.v.xyz;
		const t = p.cross(v).scale(2);
		return v.sub(t.scale(this.v.w)).add(p.cross(t));
	}

	toAxisAngle(): { axis: float3, angle: number } {
		// q = (p, w) where p is vector part and w scalar part
		const p = this.v.xyz;
		const w = this.v.w;
		const angle = 2 * Math.acos(w);
		const s = Math.sqrt(1 - w * w);
		if (s < 1e-12) {
			// angle is approximately 0, return arbitrary axis
			return { axis: float3(1, 0, 0), angle: 0 };
		}
		return { axis: p.scale(1 / s), angle };
	}

	toEulerZYX(): float3 {
		// Use standard quaternion -> ZYX (yaw-pitch-roll) conversion.
		// q = (x,y,z,w)
		const x = this.v.x, y = this.v.y, z = this.v.z, w = this.v.w;
		// roll (X)
		const rx = Math.atan2(2 * (w * x + y * z), 1 - 2 * (x * x + y * y));
		// pitch (Y) -- use asin of the sine term and clamp for numerical safety
		const sy = 2 * (w * y - z * x);
		const ry = Math.asin(Math.max(-1, Math.min(1, sy)));
		// yaw (Z)
		const rz = Math.atan2(2 * (w * z + x * y), 1 - 2 * (y * y + z * z));
		return float3(rx, ry, rz);
	}
}

export type unitQuaternion = _unitQuat;
export const unitQuaternion = Object.assign(
	function (v: float4) { return new _unitQuat(normalise(v)); },
	{// statics
	identity()		{ return new _unitQuat(float4(0, 0, 0, 1)); },
	from3x3(mat: float3x3) : unitQuaternion {
		//let {x, y, z} = mat;
		const	x	= normalise(mat.x);
		const	y	= normalise(mat.y.sub(x.scale(x.dot(mat.y))));
		const	z	= x.cross(y);

		const	a	= float4(1, z.y, x.z, y.x);
		const	b	= float4(x.x, -y.z, 0, 0);
		const	c	= float4(y.y, 0, -z.x, 0);
		const	d	= float4(z.z, 0, 0, -x.y);

		let	q: float4;
		q	= a.add(b).add(c).add(d);
		if (q.x > 1)
			return new _unitQuat(q.yzwx.scale(0.5 / Math.sqrt(q.x)));

		q	= a.add(b).sub(c).sub(d);
		if (q.x > 1)
			return new _unitQuat(q.xwzy.scale(0.5 / Math.sqrt(q.x)));

		q	= a.sub(b).add(c).sub(d);
		if (q.x > 1)
			return new _unitQuat(q.wxyz.scale(0.5 / Math.sqrt(q.x)));

		q	= a.sub(b).sub(c).add(d);
		return new _unitQuat(q.zyxw.scale(0.5 / Math.sqrt(q.x)));
	},
	fromEulerZYX(angles: float3): unitQuaternion {
		const	half	= angles.scale(0.5);
		const	{s: sx, c:cx}	= sincos(half.x);
		const	{s: sy, c:cy}	= sincos(half.y);
		const	{s: sz, c:cz}	= sincos(half.z);
		return new _unitQuat(float4(
			sx * cy * cz - cx * sy * sz,
			cx * sy * cz + sx * cy * sz,
			cx * cy * sz - sx * sy * cz,
			cx * cy * cz + sx * sy * sz
		));
	},
	fromAxisAngle(axis: float3, angle: number): unitQuaternion {
		const { s, c } = sincos(angle * 0.5);
		return new _unitQuat(concat(normalise(axis).scale(s), c));
	},
	between(a: float3, b: float3) {
		const half	= a.add(b).scale(0.5);
		const w		= a.dot(half);
		return new _unitQuat(Math.abs(w) < 1e-4 ? float4(1,0,0,0) : normalise(concat(half.cross(a), w)));
	},

	pow(q: unitQuaternion, y: number) {
		const	t	= q.v.xyz.len();
		const	s	= Math.sin(Math.asin(t) * y);
		return q.create(concat(q.v.xyz.scale(t ? s / t : 1), sin2cos(s)));
	},
	log(q: unitQuaternion): quaternion {
		const	v		= q.v.xyz;
		const	vlen	= v.len();
		const	scale	= vlen > 0 ? Math.acos(q.v.w / vlen) / vlen : 0;
		return quaternion(concat(v.scale(scale), 0));
	},

	slerp(a: unitQuaternion, b: unitQuaternion, t: number) : unitQuaternion {
		const cosom = a.cosang(b);
		let scalex = 1 - t;
		let scaley = Math.sign(cosom) * t;
		if (Math.abs(cosom) < 0.99) {
			const omega = Math.acos(cosom);
			scalex = Math.sin(scalex * omega);
			scaley = Math.sin(scaley * omega);
		}
		return a.create(normalise(a.v.scale(scalex).add(b.v.scale(scaley))));
	},

	squad(q0: unitQuaternion, a: unitQuaternion, b: unitQuaternion, q1: unitQuaternion, t: number): unitQuaternion {
		return this.slerp(this.slerp(q0, q1, t), this.slerp(a, b, t), 2 * (1 - t) * t);
	}

});

