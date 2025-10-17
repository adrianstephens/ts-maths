/* eslint-disable no-restricted-syntax */
import {ops} from './core';

// Core axis types
export type E2 = 'x'|'y';
export type E3 = E2|'z';
export type E4 = E3|'w';

// Vector and matrix types
export type vec<T, E extends string> = {
	[K in E]: T
};

// Extract the component key names (e.g. 'x'|'y'|'z') from a vector-like type
type ExtractE<T> = { [K in keyof T]: T[K] extends number ? K : never }[keyof T] & string;

// Abstract vector operations
export abstract class vops<C extends vops<C>> extends ops<C> {
	create(...args: number[]): C {
		const ctor = this.constructor as new (...args: number[]) => C;
		return new ctor(...args);
	}
//	[K: number]: number;
//	[Symbol.iterator](): Iterator<number> {
//		return Object.values(this)[Symbol.iterator]();
//	}

	abstract dupe(): C;
	abstract abs(): C;
	abstract min(b: C): C;
	abstract max(b: C): C;
	abstract equal(b: C): boolean;
	abstract dot(b: C): number;
	abstract perp(): C;
	lensq() 				{ return this.dot(this as unknown as C); }
	len() 					{ return Math.sqrt(this.lensq()); }
	selfScale(b: number) 	{ Object.assign(this, this.scale(b)); }
	selfMul(b: C) 			{ Object.assign(this, this.mul(b)); }
	selfAdd(b: C) 			{ Object.assign(this, this.add(b)); }
	selfSub(b: C) 			{ Object.assign(this, this.sub(b)); }
	clamp(min: C, max: C) 	{ return this.max(min).min(max); }
}

type vops2<E extends string> = vops<vec<number, E> & vops<any>>;

// Matrix interface and implementation
export interface matOps<C extends vops<C>, R extends string> {
	mul(v: vec<number, R>): C;
	matmul<R2 extends string, M2 extends matOps<vops2<R>, R2>>(m: M2): matOps<C, R2>;
	inverse(): matOps<C, R>;
	det(): number;
}

class matImp<C extends vops<C>, R extends string> implements matOps<C, R> {
	x!: C;
	constructor(cols: vec<C, R>) {
		Object.assign(this, cols);
	}
	create(...cols: C[]): matOps<C, R> {
		return new matImp(Object.fromEntries(Object.keys(this).map((k, i) => [k, cols[i]])) as vec<C, R>);
	}
	columns(): C[] {
		return Object.values(this) as C[];
	}
	private colKeys(): R[] {
		return Object.keys(this) as R[];
	}
	private rowKeys(): ExtractE<C>[] {
		return Object.keys(this.x) as ExtractE<C>[];
	}

	mul(v: vec<number, R>): C {
		const m1	= this as {[K in R]: C};
		const keys	= this.colKeys();
		const r		= m1[keys[0]].scale(v[keys[0]]);
		for (const k of keys.slice(1))
			r.selfAdd(m1[k].scale(v[k]));
		return r;
	}
	matmul<R2 extends string, M2 extends matOps<vops2<R>, R2>>(m: M2): matOps<C, R2> {
		const out: any = {};
		for (const k in m)
			out[k] = this.mul(m[k] as vec<number, R>);

		return new matImp<C,R2>(out as vec<C, R2>);
	}
	matmul0<C2 extends vops<C2>, R2 extends string>(m: vec<C2, R2>) {
		const out: any = {};
		for (const k in m)
			out[k] = this.mul(m[k] as vec<number, R>);
		return new matImp<C,R2>(out as vec<C, R2>);
	}
	inverse(): matOps<C, R> {
		const keys 	= this.rowKeys();
		const mat	= this.columns().map(row => row.dupe());
		const n		= mat.length;
		const inv	= Array.from({length: n}, (_, i) => this.x.create(...Array.from({length: n}, (_, j) => i === j ? 1 : 0)));
		for (let i = 0; i < n; ++i) {
			const pivot = mat[i][keys[i]] as unknown as number;
			if (pivot === 0)
				throw new Error('Matrix is singular');
			mat[i].selfScale(1 / pivot);
			inv[i].selfScale(1 / pivot);
			for (let k = 0; k < n; ++k) {
				if (k === i)
					continue;
				const factor = mat[k][keys[i]] as unknown as number;
				mat[k].selfSub(mat[i].scale(factor));
				inv[k].selfSub(inv[i].scale(factor));
			}
		}
		return this.create(...inv);
	}
	det(): number {
		function recurse(m: number[][]) {
			const n = m.length;
			if (n === 1)
				return m[0][0];
			if (n === 2)
				return m[0][0] * m[1][1] - m[0][1] * m[1][0];
			let sum = 0;
			for (let col = 0; col < n; ++col) {
				const minor = m.slice(1).map(row => row.filter((_, j) => j !== col));
				sum += (col % 2 === 0 ? 1 : -1) * m[0][col] * recurse(minor);
			}
			return sum;
		}
		return recurse(this.columns().map(row => Object.values(row) as number[]));
	}
}

export function matClass<C extends vops<C>, R extends string>() {
	return matImp as new (cols: vec<C, R>) => matOps<C, R>;
}

// Generic matrix multiply
export function matmul<C extends vops<C>, R extends string, BC extends string>(
	a: matOps<C, R>,
	b: matOps<vops2<R>, BC>
) {//}: vec<C, BC> {
	return a.matmul(b);
}

//-----------------------------------------------------------------------------
// swizzles
//-----------------------------------------------------------------------------

/*
function add_alias<O, K extends keyof O>(obj: O, a: string, k: K, set: boolean) {
	Object.defineProperty(obj, a, {
		get(): O[K] { return this[k]; },
		...(set ? { set(v: O[K]) { this[k] = v; } } : {})
	});
}

function add_indices<O, K extends keyof O>(obj: O, k: readonly K[], set: boolean) {
	k.forEach((k, i) => add_alias(obj, i.toString(), k, set));
}
*/
type swiz2<T2, E extends string> = { [K in`${E}${E}`]: T2; };
type swiz3<T3, T2, E extends string> = swiz2<T2, E> & { [K in`${E}${E}${E}`]: T3; };
type swiz4<T4, T3, T2, E extends string> = swiz3<T3, T2, E> & { [K in`${E}${E}${E}${E}`]: T4; }

function add_swizzle2<O>(obj: O, x: keyof O, y: keyof O, set: boolean, make: (x: any, y: any) => any) {
	Object.defineProperty(obj, (x as string) + (y as string), {
		get() { return make(this[x], this[y]); },
		...(set ? { set(v: vec<any, E2>) { this[x] = v.x; this[y] = v.y; } } : {})
	});
}
function add_swizzles2<T>(obj: vec<T, E2>, make2: (x: T, y: T) => any) {
	//Object.defineProperty(obj, Symbol.iterator, { value: function () { return Object.values(this)[Symbol.iterator](); } });
	const fields = ['x', 'y'] as const;
	//add_indices(obj, fields, true);

	for (const x of fields) {
		for (const y of fields)
			add_swizzle2(obj, x, y, x !== y, make2);
	}
}

function add_swizzle3<O>(obj: O, x: keyof O, y: keyof O, z: keyof O, set: boolean, make: (x: any, y: any, z: any) => any) {
	Object.defineProperty(obj, (x as string) + (y as string) + (z as string), {
		get(this: O) { return make(this[x], this[y], this[z]); },
		...(set ? { set(this: O, v: vec<any, E3>) { this[x] = v.x; this[y] = v.y; this[z] = v.z; } } : {})
	});
}
function add_swizzles3<T>(obj: vec<T, E3>, make2: (x: T, y: T) => any, make3: (x: T, y: T, z: T) => any) {
	const fields = ['x', 'y', 'z'] as const;
	//add_indices(obj, fields, true);

	for (const x of fields) {
		for (const y of fields) {
			const set2 = x !== y;
			add_swizzle2(obj, x, y, set2, make2);
			for (const z of fields)
				add_swizzle3(obj, x, y, z, set2 && x !== z && y !== z, make3);
		}
	}
}

function add_swizzle4<O>(obj: O, x: keyof O, y: keyof O, z: keyof O, w: keyof O, set: boolean, make: (x: any, y: any, z: any, w: any) => any) {
	return {
		get(this: O) { return make(this[x], this[y], this[z], this[w]); },
		...(set ? { set(this: O, v: vec<any, E4>) { this[x] = v.x; this[y] = v.y; this[z] = v.z; this[w] = v.w; } } : {})
	};
}
function add_swizzles4<T, T2, T3, T4>(obj: vec<T, E4>, make2: (x: T, y: T) => T2, make3: (x: T, y: T, z: T) => T3, make4: (x: T, y: T, z: T, w: T) => T4) {
	const fields = ['x', 'y', 'z', 'w'] as const;
	//add_indices(obj, fields, true);

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

function ownProps(obj: Record<string, any>) {
	return Object.fromEntries(Object.getOwnPropertyNames(obj).filter(name => name !== 'constructor').map(name => [name, obj[name]]));
}

//-----------------------------------------------------------------------------
// general vector extent
//-----------------------------------------------------------------------------

class extent<C extends vops<C>> {
	constructor(public min: C, public max: C) {
	}
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
// general vector functions
//-----------------------------------------------------------------------------

export function mid<C extends vops<C>>(a: C, b: C) {
	return a.add(b).scale(0.5);
}
export function normalise<C extends vops<C>>(a: C) {
	return a.scale(1 / a.len());
}
export function project<C extends vops<C>>(a: C, b: C) {
	return b.scale(a.dot(b) / b.lensq());
}
export function reflect<C extends vops<C>>(a: C, b: C) {
	return project(a, b).scale(2).sub(a);
}
export function lerp<C extends vops<C>>(a: C, b: C, t: number) {
	return a.add(b.sub(a).scale(t));
}

export function approx_equal<C extends vops<C>>(a: C, b: C, tol = 1e-9) {
	const d = a.sub(b).abs();
	const t = a.abs().max(b.abs()).scale(tol);
	return t.max(d).equal(t);
}


//-----------------------------------------------------------------------------
// 2D
//-----------------------------------------------------------------------------

export interface float2 extends vec<number, E2>, swiz2<float2, E2>, vops<float2> {
	cross(b: float2): number;
	atan2(): number;
}

export const float2 = Object.assign(
	function(this: float2, x: number, y: number) {
		if (!this)
			return new float2(x, y);
		this.x = x;
		this.y = y;
	} as {
		(x: number, y: number): float2;		// Callable signature
		new (x: number, y: number): float2; // Constructor signature
	}, {
	// statics
	zero() {
		return float2(0, 0);
	},
	cossin(angle: number) {
		return float2(Math.cos(angle), Math.sin(angle));
	},
	translate(z: float2) {
		return float2x3(float2(1, 0), float2(0, 1), z);
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
});

Object.assign(float2.prototype, ownProps(vops.prototype), {
	dupe(this: float2) 				{ return float2(this.x, this.y); },
	neg(this: float2) 				{ return float2(-this.x, -this.y); },
	abs(this: float2) 				{ return float2(Math.abs(this.x), Math.abs(this.y)); },
	scale(this: float2, b: number) 	{ return float2(this.x * b, this.y * b); },
	mul(this: float2, b: float2) 	{ return float2(this.x * b.x, this.y * b.y); },
	div(this: float2, b: float2) 	{ return float2(this.x / b.x, this.y / b.y); },
	add(this: float2, b: float2) 	{ return float2(this.x + b.x, this.y + b.y); },
	sub(this: float2, b: float2) 	{ return float2(this.x - b.x, this.y - b.y); },
	min(this: float2, b: float2) 	{ return float2(Math.min(this.x, b.x), Math.min(this.y, b.y)); },
	max(this: float2, b: float2) 	{ return float2(Math.max(this.x, b.x), Math.max(this.y, b.y)); },
	equal(this: float2, b: float2) 	{ return this.x === b.x && this.y === b.y; },
	dot(this: float2, b: float2) 	{ return this.x * b.x + this.y * b.y; },
	perp(this: float2) 				{ return float2(-this.y, this.x); },
	cross(this: float2, b: float2) 	{ return this.x * b.y - this.y * b.x; },
	atan2(this: float2) 			{ return Math.atan2(this.y, this.x); },
	toString(this: float2) 			{ return `(${this.x}, ${this.y})`; },
	[Symbol.for("debug.description")](this: float2) { return this.toString(); }
});
add_swizzles2(float2.prototype, float2);

export class extent2 extends extent<float2> {
	static fromCentreExtent(centre: float2, size: float2) {
		const half = size.scale(0.5);
		return new extent2(centre.sub(half), centre.add(half));
	}
	static from<U extends Iterable<float2>>(items: U) {
		const ext = new extent2;
		for (const i of items)
			ext.add(i);
		return ext;
	}

	constructor(
		min = float2(Infinity, Infinity),
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

export type float2x2 = matOps<float2, E2> & vec<float2, E2> & {
	mulPos(v: float2): float2;
};
export const float2x2 = Object.assign(
	function(x: float2, y: float2) {
		return new (matClass<float2, E2>())({x, y}) as float2x2;
	}, {
	// statics
	identity() {
		return float2x2(float2(1,0), float2(0,1));
	},
});
//extra instance methods
Object.assign(float2x2.prototype, {
	mulPos(this: float2x2,v: float2) { return this.mul(v); },
});

export type float2x3 = matOps<float2, E3> & vec<float2, E3> & {
	mulPos(v: float2): float2;
	mulAffine(this: float2x3, b: float2x3|float2x2): float2x3;
};
export const float2x3 = Object.assign(
	function(x: float2, y: float2, z: float2) {
		return new (matClass<float2, E3>())({x, y, z}) as float2x3;
	}, {
	// statics
	identity() {
		return float2x3(float2(1,0), float2(0,1), float2(0,0));
	},
	
});
//extra instance methods
Object.assign(float2x3.prototype, {
	mulPos(this: float2x3,v: float2) { return this.mul({...v, z: 1}); },
	mulAffine(this: float2x3, b: float2x3|float2x2) { return mulAffine2x3(this, b); }
});

function mulAffine2x3(a: float2x3, b: float2x3|float2x2): float2x3 {
	return float2x3(
		a.x.scale(b.x.x).add(a.y.scale(b.x.y)),
		a.x.scale(b.y.x).add(a.y.scale(b.y.y)),
		'z' in b
			? a.x.scale(b.z.x).add(a.y.scale(b.z.y)).add(a.z)
		 	: a.z
	);
}

//-----------------------------------------------------------------------------
// 3D
//-----------------------------------------------------------------------------

export interface float3 extends vec<number, E3>, swiz3<float3, float2, E3>, vops<float3> {
	cross(b: float3): float3;
}

export const float3 = Object.assign(
	function(this: float3, x: number, y: number, z: number) {
		if (!this)
			return new float3(x, y, z);
		this.x = x;
		this.y = y;
		this.z = z;
	} as {
		(x: number, y: number, z: number): float3;	   // Callable signature
		new (x: number, y: number, z: number): float3; // Constructor signature
	}, {
	// statics
	zero() {
		return float3(0, 0, 0);
	},
	translate(w: float3) {
		return float3x4(float3(1, 0, 0), float3(0, 1, 0), float3(0, 0, 1), w);
	},
	scale(s: vec<number, E3>|number) {
		if (typeof s === 'number')
			s = float3(s, s, s);
		return float3x3(float3(s.x, 0, 0), float3(0, s.y, 0), float3(0, 0, s.z));
	},
});

Object.assign(float3.prototype, ownProps(vops.prototype), {
	dupe(this: float3) 				{ return float3(this.x, this.y, this.z); },
	neg(this: float3) 				{ return float3(-this.x, -this.y, -this.z); },
	abs(this: float3) 				{ return float3(Math.abs(this.x), Math.abs(this.y), Math.abs(this.z)); },
	scale(this: float3, b: number) 	{ return float3(this.x * b, this.y * b, this.z * b); },
	mul(this: float3, b: float3) 	{ return float3(this.x * b.x, this.y * b.y, this.z * b.z); },
	div(this: float3, b: float3) 	{ return float3(this.x / b.x, this.y / b.y, this.z / b.z); },
	add(this: float3, b: float3) 	{ return float3(this.x + b.x, this.y + b.y, this.z + b.z); },
	sub(this: float3, b: float3) 	{ return float3(this.x - b.x, this.y - b.y, this.z - b.z); },
	min(this: float3, b: float3) 	{ return float3(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z)); },
	max(this: float3, b: float3) 	{ return float3(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z)); },
	equal(this: float3, b: float3) 	{ return this.x === b.x && this.y === b.y && this.z === b.z; },
	dot(this: float3, b: float3) 	{ return this.x * b.x + this.y * b.y + this.z * b.z; },
	perp(this: float3) 				{
		const s = this.z < 0 ? -1 : 1;
		const a = -this.y / (s + this.z);
		return float3(this.x * a, s + this.y * a, -this.y);
	},
	toString(this: float3) 			{ return `(${this.x}, ${this.y}, ${this.z})`; },
	[Symbol.for("debug.description")](this: float3) { return this.toString(); }
});
add_swizzles3(float3.prototype, float2, float3);

export class extent3 extends extent<float3> {
	constructor(
		min = float3(Infinity, Infinity, Infinity),
		max = float3(-Infinity, -Infinity, -Infinity),
	) {
		super(min, max);
	}
}

export type float3x3 = matOps<float3, E3> & vec<float3, E3>;
export const float3x3 = Object.assign(
	function(x: float3, y: float3, z: float3) {
		return new (matClass<float3, E3>())({x, y, z}) as float3x3;
	}, {
	// statics
	identity() {
		return float3x3(float3(1,0,0), float3(0,1,0), float3(0,0,1));
	},
	basis(dir: float3): float3x3 {
		const z = normalise(dir);
		const s = z.z < 0 ? -1 : 1;
		const a = -1 / (s + z.z);
		const b = z.x * z.y * a;

		return float3x3(
			float3(1 + s * a * z.x * z.x, s * b, -s * z.x),
			float3(b, s + a * z.y * z.y, -z.y),
			z
		);
	}
});


export type float3x4 = matOps<float3, E4> & vec<float3, E4> & {
	mulPos(v: float3): float3;
	mulAffine(this: float3x4, b: float3x4|float3x3): float3x4;
}
export const float3x4 = Object.assign(
	function(x: float3, y: float3, z: float3, w: float3): float3x4 {
		return new (matClass<float3, E4>())({x, y, z, w}) as float3x4;
	}, {
	// statics
	identity() {
		return float3x4(float3(1,0,0), float3(0,1,0), float3(0,0,1), float3(0,0,0));
	},
});
//extra instance methods
Object.assign(float3x4.prototype, {
	mulPos(this: float3x4,v: float3) { return this.mul({...v, w: 1}); },
	mulAffine(this: float3x4, b: float3x4|float3x3) { return mulAffine3x4(this, b); }
});

function mulAffine3x4(a: float3x4, b: float3x4|float3x3): float3x4 {
	return float3x4(
		a.x.scale(b.x.x).add(a.y.scale(b.x.y)).add(a.z.scale(b.x.z)),
		a.x.scale(b.y.x).add(a.y.scale(b.y.y)).add(a.z.scale(b.y.z)),
		a.x.scale(b.z.x).add(a.y.scale(b.z.y)).add(a.z.scale(b.z.z)),
		'w' in b
			? a.x.scale(b.w.x).add(a.y.scale(b.w.y)).add(a.z.scale(b.w.z)).add(a.w)
			: a.w
	);
}
//-----------------------------------------------------------------------------
// 4D
//-----------------------------------------------------------------------------

export interface float4 extends vec<number, E4>, swiz4<float4, float3, float2, E4>, vops<float4> {}

export const float4 = Object.assign(
	function(this: float4, x: number, y: number, z: number, w: number) {
		if (!this)
			return new float4(x, y, z, w);
		this.x = x;
		this.y = y;
		this.z = z;
		this.w = w;
	} as {
		(x: number, y: number, z: number, w: number): float4;	  // Callable signature
		new (x: number, y: number, z: number, w: number): float4; // Constructor signature
	}, {
	// statics
	zero() {
		return float4(0, 0, 0, 0);
	},
	scale(s: vec<number, E4>|number) {
		if (typeof s === 'number')
			s = float4(s, s, s, s);
		return float4x4(float4(s.x, 0, 0, 0), float4(0, s.y, 0, 0), float4(0, 0, s.z, 0), float4(0, 0, 0, s.w));
	},
});

Object.assign(float4.prototype, ownProps(vops.prototype), {
	dupe(this: float4) 				{ return float4(this.x, this.y, this.z, this.w); },
	neg(this: float4) 				{ return float4(-this.x, -this.y, -this.z, -this.w); },
	abs(this: float4) 				{ return float4(Math.abs(this.x), Math.abs(this.y), Math.abs(this.z), Math.abs(this.w)); },
	scale(this: float4, b: number) 	{ return float4(this.x * b, this.y * b, this.z * b, this.w * b); },
	mul(this: float4, b: float4) 	{ return float4(this.x * b.x, this.y * b.y, this.z * b.z, this.w * b.w); },
	div(this: float4, b: float4) 	{ return float4(this.x / b.x, this.y / b.y, this.z / b.z, this.w / b.w); },
	add(this: float4, b: float4) 	{ return float4(this.x + b.x, this.y + b.y, this.z + b.z, this.w + b.w); },
	sub(this: float4, b: float4) 	{ return float4(this.x - b.x, this.y - b.y, this.z - b.z, this.w - b.w); },
	min(this: float4, b: float4) 	{ return float4(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z), Math.min(this.w, b.w)); },
	max(this: float4, b: float4) 	{ return float4(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z), Math.max(this.w, b.w)); },
	equal(this: float4, b: float4) 	{ return this.x === b.x && this.y === b.y && this.z === b.z && this.w === b.w; },
	dot(this: float4, b: float4) 	{ return this.x * b.x + this.y * b.y + this.z * b.z + this.w * b.w; },
	perp(this: float4) 				{
		const comps = [this.x, this.y, this.z, this.w];
		let i = 0;
		for (let k = 1; k < 4; ++k)
			if (Math.abs(comps[k]) > Math.abs(comps[i]))
				i = k;

		let j = (i === 0 ? 1 : 0);
		for (let k = 0; k < 4; ++k)
			if (k != i && Math.abs(comps[k]) > Math.abs(comps[j]))
				j = k;

		const out = [0, 0, 0, 0];
		out[i] = -comps[j];
		out[j] =  comps[i];
		return float4(out[0], out[1], out[2], out[3]);
	},
	toString(this: float4) 			{ return `(${this.x}, ${this.y}, ${this.z}, ${this.w})`; },
	[Symbol.for("debug.description")](this: float4) { return this.toString(); }
});
add_swizzles4(float4.prototype, float2, float3, float4);

export type float4x4 = matOps<float4, E4> & vec<float4, E4>;
export const float4x4 = Object.assign(
	function(x: float4, y: float4, z: float4, w: float4) {
		return new (matClass<float4, E4>())({x, y, z, w}) as float4x4;
	}, {
	// statics
	identity() {
		return float4x4(float4(1,0,0,0), float4(0,1,0,0), float4(0,0,1,0), float4(0,0,0,1));
	},
});


//matmul checks
/*
const m2x2 = float2x2(float2(1,2), float2(3,4));
const m2x3 = float2x3(float2(1,2), float2(3,4), float2(5,6));
const _m0 = m2x2.matmul(m2x3);
//const _m1 = m2x3.matmul(m2x2);
const _m2 = matmul(m2x2, m2x3);
//const _m3 = matmul(m2x3, m2x2);
*/