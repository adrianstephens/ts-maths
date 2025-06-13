/* eslint-disable no-restricted-syntax */

export type vec<T, E extends string> = {
	[K in E]: T;
}

/*
function add_swizzle<O>(obj: O, comps: (keyof O)[], set: boolean, make: (...comps: any[]) => any) {
	Object.defineProperty(obj, comps.join(''), {
		get()				{ return make(...comps.map(c => this[c])); },
		...(set ? {set(v: vec<any, E2>)	{ comps.forEach((c, i) => this[c] = v[i]); }} : {})
	});
}
*/

type E2 = 'x'|'y';
type swiz2<T2, T, E extends string> = {[K in `${E}${E}`]: T2;} & {[Symbol.iterator](): Iterator<T>; [i: number]: T;}

function add_alias<O, K extends keyof O>(obj: O, a: string, k: K, set: boolean) {
    Object.defineProperty(obj, a, {
        get(): O[K] { return this[k]; },
        ...(set ? {set(v: O[K]) { this[k] = v; }} : {})
    });
}

function add_indices<O, K extends keyof O>(obj: O, k: readonly K[], set: boolean) {
	k.forEach((k, i) => add_alias(obj, i.toString(), k, set));
}


function add_swizzle2<O>(obj: O, x: keyof O, y: keyof O, set: boolean, make: (x: any, y: any) => any) {
	Object.defineProperty(obj, (x as string) + (y as string), {
		get()				{ return make(this[x], this[y]); },
		...(set ? {set(v: vec<any, E2>)	{ this[x] = v.x; this[y] = v.y; }} : {})
	});
}
function add_swizzles2<T>(obj: vec<T, E2>, make2: (x: T, y: T) => any) {
	Object.defineProperty(obj, Symbol.iterator, { value: function () { return Object.values(this)[Symbol.iterator](); } });

	const fields = ["x", "y"] as const;
	add_indices(obj, fields, true);

	for (const x of fields) {
		for (const y of fields)
			add_swizzle2(obj, x, y, x !== y, make2);
	}
}

type E3 = E2|'z';
type swiz3<T3, T2, T, E extends string> = swiz2<T2, T, E> & {[K in `${E}${E}${E}`]: T3;}

function add_swizzle3<O>(obj: O, x: keyof O, y: keyof O, z: keyof O, set: boolean, make: (x: any, y: any, z: any) => any) {
	Object.defineProperty(obj, (x as string) + (y as string) + (z as string), {
		get(this: O)				{ return make(this[x], this[y], this[z]); },
		...(set ? {set(this: O, v: vec<any,E3>) { this[x] = v.x; this[y] = v.y; this[z] = v.z; }} : {})
	});
}
function add_swizzles3<T>(obj: vec<T,E3>, make2: (x: T, y: T) => any, make3: (x: T, y: T, z: T) => any) {
	const fields = ["x", "y", "z"] as const;
	add_indices(obj, fields, true);

	for (const x of fields) {
		for (const y of fields) {
			const set2 = x !== y;
			add_swizzle2(obj, x, y, set2, make2);
			for (const z of fields)
				add_swizzle3(obj, x, y, z, set2 && x !== z && y !== z, make3);
		}
	}
}

type E4 = E3|'w';
type swiz4<T4, T2, T3, T, E extends string> = swiz3<T3, T2, T, E> & {[K in `${E}${E}${E}${E}`]: T4;}

function add_swizzle4<O>(obj: O, x: keyof O, y: keyof O, z: keyof O, w: keyof O, set:boolean, make: (x: any, y: any, z: any, w: any) => any) {
	return {
		get(this: O)				{ return make(this[x], this[y], this[z], this[w]); },
		...(set ? {set(this: O, v: vec<any,E4>)	{ this[x] = v.x; this[y] = v.y; this[z] = v.z; this[w] = v.w; }} : {})
	};
}
function add_swizzles4<T, T2, T3, T4>(obj: vec<T,E4>, make2: (x: T, y: T) => T2, make3: (x: T, y: T, z: T) => T3, make4: (x: T, y: T, z: T, w: T) => T4) {
	const fields = ["x", "y", "z", "w"] as const;
	add_indices(obj, fields, true);

	for (const x of fields) {
		for (const y of fields) {
			const set2 = x !== y;
			add_swizzle2(obj, x, y, set2, make2);
			for (const z of fields) {
				const set3 = set2 && x !== z && y !== z;
				add_swizzle3(obj, x, y, z, set3, make3);
				for (const w of fields)
					add_swizzle4(obj, x, y, z, w, set3 && x !== w && y !== w && z !== w, make4);
			}
		}
	}
}

//export type vecn<T, K extends 2|3|4> = K extends 2 ? vec<T, E2> : K extends 3 ? vec<T, E3> : vec<T, E4>;
//export type vec2<T>	= vec<T, E2>;
//export type vec3<T>	= vec<T, E3>;
//export type vec4<T>	= vec<T, E4>;

function hasz<T, E extends E2>(a: vec<T, E>): a is vec<T, E3> { return 'z' in a; }
function hasw<T, E extends E3>(a: vec<T, E>): a is vec<T, E4> { return 'w' in a; }

export abstract class ops<C extends ops<C>> {
	abstract dupe(): 			C;
	abstract neg(): 			C;
	abstract abs():	 			C;
	abstract mul(b: number):	C;
	abstract vmul(b: C):		C;
	abstract vdiv(b: C):		C;
	abstract add(b: C): 		C;
	abstract sub(b: C): 		C;
	abstract min(b: C): 		C;
	abstract max(b: C): 		C;
	abstract equal(b: C): 		boolean;
	abstract dot(b: C): 		number;
	abstract perp(): 			C;

	lensq()					{ return this.dot(this as unknown as C); }
	len()					{ return Math.sqrt(this.lensq()); }
	muleq(b: number)		{ Object.assign(this, this.mul(b)); }
	vmuleq(b: C)			{ Object.assign(this, this.vmul(b)); }
	addeq(b: C) 			{ Object.assign(this, this.add(b)); }
	subeq(b: C) 			{ Object.assign(this, this.sub(b)); }
	clamp(min: C, max: C)	{ return this.min(max).min(max); }
}

function ownProps(obj: Record<string, any>) {
	return Object.fromEntries(Object.getOwnPropertyNames(obj).filter(name => name !== 'constructor').map(name => [name, obj[name]]));
}

export function mid<C extends ops<C>>(a: C, b: C) 	{ return a.add(b).mul(0.5); }
//export function lensq<C extends ops<C>>(a: C)		{ return a.dot(a);}
//export function len<C extends ops<C>>(a: C)			{ return Math.sqrt(lensq(a));}

export function normalise<C extends ops<C>>(a: C)				{ return a.mul(1 / a.len());}
export function project<C extends ops<C>>(a: C, b: C)			{ return b.mul(a.dot(b) / b.lensq()); }
export function reflect<C extends ops<C>>(a: C, b: C)			{ return project(a, b).mul(2).sub(a); }
export function lerp<C extends ops<C>>(a: C, b: C, t: number)	{ return a.add(b.sub(a).mul(t)); }

export function approx_equal<C extends ops<C>>(a: C, b: C, tol = 1e-9) {
	const d = a.sub(b).abs();
	const t = a.abs().max(b.abs()).mul(tol);
	return t.max(d).equal(t);
}

class extent<C extends ops<C>> {
	constructor(public min:C, public max:C) {}
	extent() {
		return this.max.sub(this.min);
	}
	centre() {
		return mid(this.min, this.max);
	}
	add(p: C) {
		this.min = this.min.min(p);
		this.max = this.max.max(p);
	}
	combine(b: extent<C>) {
		this.min = this.min.min(b.min);
		this.max = this.max.max(b.max);
	}
	encompasses(b: extent<C>) {
		return this.min.min(b.min) === this.min && this.max.max(b.max) === this.max;
	}
	overlaps(b: extent<C>) {
		return this.min.max(b.min) === this.min && this.max.min(b.max) === this.max;
	}
	contains(p: C) {
		return this.min.max(p) === this.min && this.max.min(p) === this.max;
	}
	clamp(p: C) {
		return p.min(this.max).max(this.min);
	}
}

//-----------------------------------------------------------------------------
// 1D
//-----------------------------------------------------------------------------

export class extent1 {
	constructor(
		public min	= Infinity,
		public max	= -Infinity
	) {}
	extent() {
		return this.max - this.min;
	}
	centre() {
		return (this.min + this.max) * 0.5;
	}
	add(p: number) {
		this.min = Math.min(this.min, p);
		this.max = Math.max(this.max, p);
	}
	combine(b: extent1) {
		this.min = Math.min(this.min, b.min);
		this.max = Math.max(this.max, b.max);
	}
	encompasses(b: extent1) {
		return this.min <= b.min && this.max >= b.max;
	}
	overlaps(b: extent1) {
		return this.min <= b.max && this.max >= b.min;
	}
	contains(p: number) {
		return this.min <= p && this.max >= p;
	}
	clamp(p: number) {
		return Math.min(Math.max(p, this.min), this.max);
	}
}

//-----------------------------------------------------------------------------
// 2D
//-----------------------------------------------------------------------------

export interface float2 extends vec<number, E2>, swiz2<float2, number, E2>, ops<float2> {
	cross(b: float2): number;
	atan2(): number;
}

export const float2 = Object.assign(
	function (this: float2, x: number, y: number) {
		if (!this)
			return new float2(x, y);
		this.x = x;
		this.y = y;
	} as {
		(x: number, y: number): float2;		// Callable signature
		new (x: number, y: number): float2;	// Constructor signature
	},
	{// statics
		zero()					{ return float2(0, 0); },
		cossin(angle: number)	{ return float2(Math.cos(angle), Math.sin(angle)); },
		translate(z: float2) {
			return float2x3(float2(1, 0), float2(0,1), z);
		},
		scale(s: {x: number, y: number}|number) {
			if (typeof s === 'number')
				s = float2(s, s);
			return float2x2(float2(s.x, 0), float2(0, s.y));
		},
		rotate(t: number) {
			const s = Math.sin(t);
			const c = Math.cos(t);
			return float2x2(float2(c, s), float2(-s, c));
		}
	}
);

Object.assign(float2.prototype, ownProps(ops.prototype), {
	dupe(this: float2)				{ return float2(this.x, this.y);},
	neg(this: float2)				{ return float2(-this.x, -this.y);},
	abs(this: float2)				{ return float2(Math.abs(this.x), Math.abs(this.y));},
	mul(this: float2, b: number)	{ return float2(this.x * b, this.y * b);},
	vmul(this: float2, b: float2)	{ return float2(this.x * b.x, this.y * b.y);},
	vdiv(this: float2, b: float2)	{ return float2(this.x / b.x, this.y / b.y);},
	add(this: float2, b: float2)	{ return float2(this.x + b.x, this.y + b.y);},
	sub(this: float2, b: float2)	{ return float2(this.x - b.x, this.y - b.y);},
	min(this: float2, b: float2)	{ return float2(Math.min(this.x, b.x), Math.min(this.y, b.y));},
	max(this: float2, b: float2)	{ return float2(Math.max(this.x, b.x), Math.max(this.y, b.y));},
	equal(this: float2, b: float2)	{ return this.x === b.x && this.y === b.y;},
	dot(this: float2, b: float2)	{ return this.x * b.x + this.y * b.y;},
	perp(this: float2)				{ return float2(-this.y, this.x); },
	cross(this: float2, b: float2)	{ return this.x * b.y - this.y * b.x;},
	atan2(this: float2)				{ return Math.atan2(this.y, this.x);},
	toString(this: float2)			{ return `(${this.x}, ${this.y})`; },
	[Symbol.for("debug.description")](this: float2) { return this.toString(); }
});
add_swizzles2(float2.prototype, float2);

export class extent2 extends extent<float2> {
	static fromCentreSize(centre: float2, size: float2) {
        const half = size.mul(0.5);
        return new extent2(centre.sub(half), centre.add(half));
    }
	static from<U extends Iterable<float2>>(items: U) {
		const ext = new extent2;
		for (const i of items)
			ext.add(i);
		return ext;
	}

	constructor(
		min	= float2(Infinity, Infinity),
		max = float2(-Infinity, -Infinity),
	) {
		super(min, max);
	}
	overlaps(b: extent<float2>) {
		return this.min.x <= b.max.x && this.max.x >= b.min.x && this.min.y <= b.max.y && this.max.y >= b.min.y;
	}
	encompasses(b: extent2) {
		return this.min.x <= b.min.x && this.max.x >= b.max.x && this.min.y <= b.min.y && this.max.y >= b.max.y;
	}
	contains(b: float2) {
		return this.min.x <= b.x && this.max.x >= b.x && this.min.y <= b.y && this.max.y >= b.y;
	}
}

export type float2x2	= vec<float2, E2>;
export type float2x3	= vec<float2, E3>;
export type float2x4	= vec<float2, E4>;

export function float2x2(x: float2, y: float2): float2x2			{ return {x, y}; }
export function float2x3(x: float2, y: float2, z: float2): float2x3	{ return {x, y, z}; }
export function float2x4(m: float2x3, w: float2): float2x4			{ return {x: m.x, y: m.y, z: m.z, w}; }
export const identity2x3	= float2x3(float2(1, 0), float2(0, 1), float2(0, 0));

export function mul2x2(m: float2x2, v: float2)				{ return m.x.mul(v.x).add(m.y.mul(v.y)); }
export function mul2x3(m: float2x3|float2x2, v: float2)		{ const t = mul2x2(m, v); return hasz(m) ? t.add(m.z) : t; }

export function matmul2(a: float2x3, b: float2x2): float2x3;
export function matmul2(a: float2x2, b: float2x3): float2x3;
export function matmul2(a: float2x3, b: float2x3): float2x3;
export function matmul2(a: float2x2, b: float2x2): float2x2;
export function matmul2(a: float2x3|float2x2, b: float2x3|float2x2): float2x3|float2x2 {
	return {
		x: mul2x3(a, b.x),
		y: mul2x3(a, b.y),
		...(hasz(b) ? {z: mul2x3(a, b.z)} : hasz(a) ? {z: a.z} : {})
	};
}

export function det2x2(m: float2x2)	{
	return m.x.cross(m.y);
}

export function inverse2x2(m: float2x2)	{
	const r = 1 / det2x2(m);
	return float2x2(float2(m.y.y * r, -m.x.y * r), float2(-m.y.x * r, m.x.x * r));
}

export function inverse2x3(m: float2x3)	{
	const i = inverse2x2(m);
	return m.z ? float2x3(i.x, i.y, mul2x2(i, m.z)) : i;
}

export function sincos_half(sc: float2) {
	const x = Math.sqrt(0.5 * (1 + sc.x));
	const y = Math.sqrt(0.5 * (1 - sc.x));
	return float2(x, sc.y < 0 ? -y : y);
}

// returns point on unit circle where |M.v| is largest
export function max_circle_point(m: float2x2) {
	const d = m.x.dot(m.y);
	if (d) {
		const sc2 = normalise(float2(m.x.x * m.x.x + m.x.y * m.x.y - m.y.x * m.y.x - m.y.y * m.y.y, d * 2));
		return sincos_half(sc2);
	}
	return float2(1, 0);
}

//-----------------------------------------------------------------------------
// 3D
//-----------------------------------------------------------------------------

export interface float3 extends vec<number, E3>, swiz3<float3, float2, number, E3>, ops<float3> {
	cross(b: float3): float3;
}

export const float3 = Object.assign(
	function (this: float3, x: number, y: number, z: number) {
		if (!this)
			return new float3(x,y,z);
		this.x = x;
		this.y = y;
		this.z = z;
	} as {
		(x: number, y: number, z: number): float3;		// Callable signature
		new (x: number, y: number, z: number): float3;	// Constructor signature
	},
	{// statics
		zero() { return float3(0, 0, 0); },
		translate(w: float3) {
			return float3x4(float3(1, 0, 0), float3(0,1,0), float3(0,0,1), w);
		},
		scale(s: vec<number,E3> | number) {
			if (typeof s === 'number')
				s = float3(s, s, s);
			return float3x3(float3(s.x, 0, 0), float3(0, s.y, 0), float3(0, 0, s.z));
		},
	}
);

Object.assign(float3.prototype, ownProps(ops.prototype), {
	dupe(this: float3)				{ return float3(this.x, this.y, this.z);},
	neg(this: float3)				{ return float3(-this.x, -this.y, -this.z);},
	abs(this: float3)				{ return float3(Math.abs(this.x), Math.abs(this.y), Math.abs(this.z));},
	mul(this: float3, b: number)	{ return float3(this.x * b, this.y * b, this.z * b);},
	vmul(this: float3, b: float3)	{ return float3(this.x * b.x, this.y * b.y, this.z * b.z);},
	vdiv(this: float3, b: float3)	{ return float3(this.x / b.x, this.y / b.y, this.z / b.z);},
	add(this: float3, b: float3)	{ return float3(this.x + b.x, this.y + b.y, this.z + b.z);},
	sub(this: float3, b: float3)	{ return float3(this.x - b.x, this.y - b.y, this.z - b.z);},
	min(this: float3, b: float3)	{ return float3(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z));},
	max(this: float3, b: float3)	{ return float3(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z));},
	equal(this: float3, b: float3)	{ return this.x === b.x && this.y === b.y && this.z === b.z;},
	dot(this: float3, b: float3)	{ return this.x * b.x + this.y * b.y + this.z * b.z;},
	perp(this: float3)				{ return this; },//TBD
	cross(this: float3, b: float3)	{ return (b.vmul(this.zxy).sub(this.vmul(b.zxy))).zxy; },
	toString(this: float3)			{ return `(${this.x}, ${this.y}, ${this.z})`; },
	[Symbol.for("debug.description")](this: float3) { return this.toString(); }
});
add_swizzles3(float3.prototype, float2, float3);

export class extent3 extends extent<float3> {
	constructor(
		min	= float3(Infinity, Infinity, Infinity),
		max = float3(-Infinity, -Infinity, -Infinity),
	) {
		super(min, max);
	}
}

export type float3x3	= vec<float3,E3>;
export type float3x4	= vec<float3,E4>;

export function float3x3(x: float3, y: float3, z: float3): float3x3	{ return {x, y, z}; }
export function float3x4(x: float3, y: float3, z: float3, w: float3): float3x4	{ return {x, y, z, w}; }
export const identity3x4	= float3x4(float3(1, 0, 0), float3(0, 1, 0), float3(0, 0, 1), float3(0, 0, 0));

export function mul3x3(m: float3x3, v: float3)				{ return m.x.mul(v.x).add(m.y.mul(v.y)).add(m.z.mul(v.z)); }
export function mul3x4(m: float3x4|float3x3, v: float3)		{ const t = mul3x3(m, v); return hasw(m) ? t.add(m.w) : t; }

export function matmul3(a: float3x4, b: float3x3): float3x4;
export function matmul3(a: float3x3, b: float3x4): float3x4;
export function matmul3(a: float3x4, b: float3x4): float3x4;
export function matmul3(a: float3x3, b: float3x3): float3x3;
export function matmul3(a: float3x4|float3x3, b: float3x4|float3x3): float3x4|float3x3 {
	return {
		x: mul3x3(a, b.x),
		y: mul3x3(a, b.y),
		z: mul3x3(a, b.z),
		...(hasw(b) ? {z: mul3x4(a, b.w)} : hasw(a) ? {z: a.w} : {})
	};
}

export function det3x3(m: float3x3)	{
	return m.x.dot(m.y.cross(m.z));
}

export function transpose3x3(m: float3x3)	{
	return float3x3(
		float3(m.x.x, m.y.x, m.z.x),
		float3(m.x.y, m.y.y, m.z.y),
		float3(m.x.z, m.y.z, m.z.z)
	);
}

export function inverse3x3(m: float3x3)	{
	const cofactors = float3x3(
		m.y.cross(m.z),
		m.z.cross(m.x),
		m.x.cross(m.y)
	);
	const rdet = 1 / m.x.dot(cofactors.x);
	return transpose3x3(float3x3(
		cofactors.x.mul(rdet),
		cofactors.y.mul(rdet),
		cofactors.z.mul(rdet)
	));
}

export function inverse3x4(m: float3x4)	{
	const i = inverse3x3(m);
	return float3x4(i.x, i.y, i.z, mul3x3(i, m.w));
}

//-----------------------------------------------------------------------------
// 4D
//-----------------------------------------------------------------------------

export interface float4 extends vec<number,E4>, swiz4<float4, float3, float2, number, E4>, ops<float4> {}

export const float4 = Object.assign(
	function (this: float4, x: number, y: number, z: number, w: number) {
		if (!this)
			return new float4(x, y, z, w);
		this.x = x;
		this.y = y;
		this.z = z;
		this.w = w;
	} as {
		(x: number, y: number, z: number, w: number): float4;		// Callable signature
		new (x: number, y: number, z: number, w: number): float4;	// Constructor signature
	},
	{// statics
		zero() { return float4(0, 0, 0, 0); },
		scale(s: vec<number,E4> |number) {
			if (typeof s === 'number')
				s = float4(s, s, s, s);
			return float4x4(float4(s.x, 0, 0, 0), float4(0, s.y, 0, 0), float4(0, 0, s.z, 0), float4(0, 0, 0, s.w));
		},
	}
);

Object.assign(float4.prototype, ownProps(ops.prototype), {
	dupe(this: float4)				{ return float4(this.x, this.y, this.z, this.w);},
	neg(this: float4)				{ return float4(-this.x, -this.y, -this.z, -this.w);},
	abs(this: float4)				{ return float4(Math.abs(this.x), Math.abs(this.y), Math.abs(this.z), Math.abs(this.w));},
	mul(this: float4, b: number)	{ return float4(this.x * b, this.y * b, this.z * b, this.w * b);},
	vmul(this: float4, b: float4)	{ return float4(this.x * b.x, this.y * b.y, this.z * b.z, this.w * b.w);},
	vdiv(this: float4, b: float4)	{ return float4(this.x / b.x, this.y / b.y, this.z / b.z, this.w / b.w);},
	add(this: float4, b: float4)	{ return float4(this.x + b.x, this.y + b.y, this.z + b.z, this.w + b.w);},
	sub(this: float4, b: float4)	{ return float4(this.x - b.x, this.y - b.y, this.z - b.z, this.w - b.w);},
	min(this: float4, b: float4)	{ return float4(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z), Math.min(this.w, b.w));},
	max(this: float4, b: float4)	{ return float4(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z), Math.max(this.w, b.w));},
	equal(this: float4, b: float4)	{ return this.x === b.x && this.y === b.y && this.z === b.z && this.w === b.w;},
	dot(this: float4, b: float4)	{ return this.x * b.x + this.y * b.y + this.z * b.z + this.w * b.w;},
	perp(this: float4)				{ return this; },//TBD
	lensq(this: float4)				{ return this.dot(this);},
	len(this: float4)				{ return Math.sqrt(this.lensq());},
	toString(this: float4)			{ return `(${this.x}, ${this.y}, ${this.z}, ${this.w})`; },
	[Symbol.for("debug.description")](this: float4) { return this.toString(); }

});
add_swizzles4(float4.prototype, float2, float3, float4);

export type float4x4	= vec<float4,E4>;
export function float4x4(x: float4, y: float4, z: float4, w: float4): float4x4	{ return {x, y, z, w}; }

//-----------------------------------------------------------------------------
// Statistics
//-----------------------------------------------------------------------------

export class statistics1 {
	sum		= 0;
	sum2	= 0;
	min		= 0;
	max		= 0;
	count	= 0;

	static from<U extends Iterable<number>>(items: U) {
		const stats = new statistics1;
		for (const i of items)
			stats.add(i);
		return stats;
	}

	add(v: number) {
		if (!this.count) {
			this.min = v;
			this.max = v;
		} else {
			this.min = Math.min(this.min, v);
			this.max = Math.max(this.max, v);
		}
		this.sum += v;
		this.sum2 += v * v;
		this.count++;
		return this;
	}
	get mean() {
		return this.sum / this.count;
	}
	get variance() {
		const mean = this.mean;
		return this.sum2 / this.count - mean * mean;
	}
	get standardDeviation() {
		return Math.sqrt(this.variance);
	}
}

interface WithZero<T> {
    zero(): T;
}
export class statistics<T extends ops<T> & WithZero<T>> {
	sum:	T;
	sum2:	T;
	min:	T;
	max:	T;
	count	= 0;

	static from<T extends ops<T> & WithZero<T>, U extends Iterable<T>>(items: U, Type?: WithZero<T>) {
		if (!Type) {
			const first = items[Symbol.iterator]().next();
			if (first.done)
				throw new Error("Cannot create statistics from empty iterable");
			Type = first.value.constructor as (new (...args: any[]) => T) & WithZero<T>;
		}
		const stats = new statistics<T>(Type);
		for (const item of items) {
			stats.add(item);
		}
		return stats;
	}

	constructor(public Type: WithZero<T>) {
		this.sum	= Type.zero();
		this.sum2	= Type.zero();
		this.min	= Type.zero();
		this.max	= Type.zero();
	}

	add(v: T) {
		if (!this.count) {
			this.min = v;
			this.max = v;
		} else {
			this.min = this.min.min(v);
			this.max = this.max.max(v);
		}
		this.sum.addeq(v);
		this.sum2.addeq(v.vmul(v));
		this.count++;
		return this;
	}
	get mean() {
		return this.sum.mul(1 / this.count);
	}
	get variance() {
		const mean = this.mean;
		return this.sum2.mul(1 / this.count).sub(mean.vmul(mean));
	}
	//standardDeviation() {
	//	return this.variance.sqrt();
	//}
}

//-----------------------------------------------------------------------------
// experiment
//-----------------------------------------------------------------------------
/*
export interface float2b extends vec<number, E2>, swiz2<float2b, number, E2>, ops<float2b> {
	cross(b: float2b): number;
	atan2(): number;
}

function makeSwizzled<T>(values: T[], prop: string, fields: string, T2: (x: T, y: T)=>any) {
	const s = [...prop].map(c => values[fields.indexOf(c)]);
	switch (values.length) {
		case 2: return T2(s[0], s[1]);
		default: return;
	}
}


export const float2b = Object.assign(
	function (this: float2b, x: number, y: number) {
		if (!this)
			return new float2b(x, y);
		this.x = x;
		this.y = y;
		return new Proxy(this, {
			get(target, prop) {
				switch (typeof prop) {
					case 'number':	return Object.values(target)[prop];
					case 'symbol':
						if (prop === Symbol.iterator)
							return Object.values(target)[Symbol.iterator];
						break;
					case 'string':
						if (prop[0] <= '9')
							return Object.values(target)[parseInt(prop)];
						if (prop === 'length')
							return 2;
						if (prop.length > 1 && (prop[0] === 'x' || prop[0] === 'y'))
							return makeSwizzled(Object.values(target), prop, 'xy', float2b);
						break;
				}
				return target[prop as keyof typeof target];
			},
			set(target, prop, value) {
				if (typeof prop === 'number')
					Object.values(target)[prop] = value;
				else
					target[prop as keyof typeof target] = value;
				return true;
			}
		});
	} as {
		(x: number, y: number): float2b;		// Callable signature
		new (x: number, y: number): float2b;	// Constructor signature
	},
	{// statics
		zero()					{ return float2b(0, 0); },
		cossin(angle: number)	{ return float2b(Math.cos(angle), Math.sin(angle)); },
		translate(z: float2b) {
			return float2x3(float2b(1, 0), float2b(0,1), z);
		},
		scale(s: {x: number, y: number}|number) {
			if (typeof s === 'number')
				s = float2b(s, s);
			return float2x2(float2b(s.x, 0), float2b(0, s.y));
		},
		rotate(t: number) {
			const s = Math.sin(t);
			const c = Math.cos(t);
			return float2x2(float2b(c, s), float2b(-s, c));
		}
	}
);

Object.assign(float2b.prototype, ownProps(ops.prototype), {
	neg(this: float2b)				{ return float2b(-this.x, -this.y);},
	abs(this: float2b)				{ return float2b(Math.abs(this.x), Math.abs(this.y));},
	mul(this: float2b, b: number)	{ return float2b(this.x * b, this.y * b);},
	vmul(this: float2b, b: float2b)	{ return float2b(this.x * b.x, this.y * b.y);},
	add(this: float2b, b: float2b)	{ return float2b(this.x + b.x, this.y + b.y);},
	sub(this: float2b, b: float2b)	{ return float2b(this.x - b.x, this.y - b.y);},
	min(this: float2b, b: float2b)	{ return float2b(Math.min(this.x, b.x), Math.min(this.y, b.y));},
	max(this: float2b, b: float2b)	{ return float2b(Math.max(this.x, b.x), Math.max(this.y, b.y));},
	equal(this: float2b, b: float2b)	{ return this.x === b.x && this.y === b.y;},
	dot(this: float2b, b: float2b)	{ return this.x * b.x + this.y * b.y;},
	perp(this: float2b)				{ return float2b(-this.y, this.x); },
	cross(this: float2b, b: float2b)	{ return this.x * b.y - this.y * b.x;},
	atan2(this: float2b)				{ return Math.atan2(this.y, this.x);}
});
*/
//-----------------------------------------------------------------------------
// colour
//-----------------------------------------------------------------------------

export interface rgb {
	r: number,
	g: number,
	b: number,
}
export interface hsv {
	h: number,
	s: number,
	v: number,
}

function rgb_to_hsv(r: number, g: number, b: number): hsv {
	const v = Math.max(Math.max(r, g), b);
	const c = v - Math.min(Math.min(r, g), b);
	return {
		h: c == 0 ? 0 : ((v == r ? g - b : v == g ? b - r : r - g) / c + (v == r ? 0 : v == g ? 2 : 4)) / 6,
		s: v == 0 ? 0 : c / v,
		v
	};
}

function hsv_to_rgb(h: number, s: number, v: number): rgb {
	const	c = v * s;
	const	m = v - c;
	let		x = v, y = v;

	const	i = Math.floor(h * 3);
	const	f = h * 3 - i;
	if (f >= 0.5)
		x -= c * (f - 0.5);
	else
		y -= c * (0.5 - f);

	switch (i) {
		case 0: return { r: x, g: y, b: m };
		case 1: return { r: m, g: x, b: y };
		case 2: return { r: y, g: m, b: x };
		default: return { r: 0, g: 0, b: 0 };
	}
}

export class colour implements rgb {
	static fromHSV(h: number, s: number, v: number)	{
		const rgb = hsv_to_rgb(h, s, v);
		return new colour(rgb.r, rgb.g, rgb.b);
	}
	static fromRGB(r: number, g: number, b: number)	{
		return new colour(r, g, b);
	}
	constructor(public r: number, public g: number, public b: number) {}
	rgb()	{ return this; }
	hsv()	{ return rgb_to_hsv(this.r, this.g, this.b); }
}