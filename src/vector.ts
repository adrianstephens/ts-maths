/* eslint-disable no-restricted-syntax */
import {ops} from './core';
import complex from './complex';
import { polynomial, polynomialN } from './polynomial';

// Core axis types
export type E2 = 'x'|'y';
export type E3 = E2|'z';
export type E4 = E3|'w';

export const E5	= ['x','y','z','w','c4'] as const;
export type E5	= typeof E5[number];

export const E6	= ['x','y','z','w','c4','c5'] as const;
export type E6	= typeof E6[number];

// Vector and matrix types
export type vec<T, E extends string> = {
	[K in E]: T
};
export function vec<T, E extends string>(e: readonly E[], ...v: T[]) {
 	return Object.fromEntries(e.map((k, i) => [k, (v[i] ?? (0 as unknown as T))])) as vec<T, E>;
}

// Abstract vector operations
export abstract class vops<C extends vops<C>> implements ops<C> {
	create(...args: number[]): C {
		const ctor = this.constructor as new (...args: number[]) => C;
		return new ctor(...args);
	}
//	[K: number]: number;
//	[Symbol.iterator](): Iterator<number> {
//		return Object.values(this)[Symbol.iterator]();
//	}
	abstract dup():				C;
	abstract neg(): 			C;
	abstract scale(b: number):	C;
	abstract mul(b: C):			C;
	abstract div(b: C):			C;
	abstract add(b: C): 		C;
	abstract sub(b: C): 		C;
	abstract abs():				C;
	abstract min(b: C):			C;
	abstract max(b: C):			C;
	abstract equal(b: C):		boolean;
	abstract dot(b: C):			number;
	abstract perp():			C;
	lensq() 				{ return this.dot(this as unknown as C); }
	len() 					{ return Math.sqrt(this.lensq()); }
	mag()					{ return this.len(); }
	selfScale(b: number) 	{ Object.assign(this, this.scale(b)); }
	selfMul(b: C) 			{ Object.assign(this, this.mul(b)); }
	selfAdd(b: C) 			{ Object.assign(this, this.add(b)); }
	selfSub(b: C) 			{ Object.assign(this, this.sub(b)); }
	clamp(min: C, max: C) 	{ return this.max(min).min(max); }
}

// general vector type

export type vector<E extends string> = vops<vector<E>> & vec<number, E>;
export function vector<E extends string>(e: readonly E[], ...v: number[]) {
	return (new vecImp<E>(vec(e, ...v))) as vector<E>;
}

export class vecImp<E extends string> extends vops<vector<E>> {
	constructor(v: vec<number, E>) {
		super();
		Object.assign(this, v);
	}
	private entries() {
		return Object.entries(this) as [E, number][];
	}
	create(...args: number[]): vector<E> {
		const ctor = this.constructor as new (v: vec<number, E>) => vector<E>;
  		return new ctor(Object.fromEntries(Object.keys(this).map((k, i) => [k, args[i]])) as vec<number, E>);
	}

	dup() 				{ return this.create(...Object.values(this)); }
	neg() 				{ return this.create(...Object.values(this).map(x => -x)); }
	abs() 				{ return this.create(...Object.values(this).map(x => Math.abs(x))); }
	scale(b: number) 	{ return this.create(...Object.values(this).map(x => x * b)); }
	mul(b: vector<E>) 	{ return this.create(...this.entries().map(([k, v]) => v * b[k])); }
	div(b: vector<E>) 	{ return this.create(...this.entries().map(([k, v]) => v / b[k])); }
	add(b: vector<E>) 	{ return this.create(...this.entries().map(([k, v]) => v + b[k])); }
	sub(b: vector<E>) 	{ return this.create(...this.entries().map(([k, v]) => v - b[k])); }
	min(b: vector<E>) 	{ return this.create(...this.entries().map(([k, v]) => Math.min(v, b[k]))); }
	max(b: vector<E>) 	{ return this.create(...this.entries().map(([k, v]) => Math.max(v, b[k]))); }
	equal(b: vector<E>) { return this.entries().every(([k, v]) => v === b[k]); }
	dot(b: vector<E>) 	{ return this.entries().reduce((acc, [k, v]) => acc + v * b[k], 0); }
	perp() 				{ return this as vector<E>; }
}

type ColumnKeys<T, C> = { [K in keyof T]: T[K] extends C ? K : never }[keyof T] & string;
type ColumnType<E extends string> = vops<ColumnType<E>> & vec<number, E>;


// Matrix interface and implementation
export interface matOps<C extends vops<C>, R extends string> {
	create(...cols: C[]):			this;
	columns():						C[];
	inverse():						this;
	det():							number;
	mul(v: vec<number, R>):			C;
	mul0(v: vec<number, string>, col: string):	C;
	matmul<M2 extends vec<ColumnType<R>, any>>(m: M2): mat<C, ColumnKeys<M2, C>>;
	trace():						number;
	characteristic():				polynomialN;
	eigenvalues():					complex[];
}

export type mat<C extends vops<C>, R extends string> = matOps<C, R> & vec<C, R>;

class matImp<C extends vops<C>, R extends string> implements matOps<C, R> {
	x!: C;
	constructor(cols: vec<C, R>) {
		Object.assign(this, cols);
	}
	create(...cols: C[]): this {
 		const ctor = this.constructor as new (cols: vec<C, R>) => any;
  		return new ctor(Object.fromEntries(Object.keys(this).map((k, i) => [k, cols[i]])) as vec<C, R>) as this;
	}
	columns(): C[] {
		return Object.values(this) as C[];
	}

	add(b: mat<C, R>): mat<C, R> {
		const m1	= this as {[K in R]: C};
		const out: vec<C, string> = {};
		for (const k in m1)
			out[k] = m1[k].add(b[k]);
		return new matImp<C, R>(out) as mat<C, R>;
	}

	mul(v: vec<number, R>): C {
		const m1	= this as {[K in R]: C};
		const r		= this.x.scale(0);
		for (const k in m1)
			r.selfAdd(m1[k].scale(v[k]));
		return r;
	}
	mul0(v: vec<number, string>, col: string): C {
		const m1	= this as {[K in R]: C};
		const r		= this.x.scale(0);
		for (const k in m1)
			r.selfAdd(m1[k].scale(v[k] ?? (k == col ? 1 : 0)));
		return r;
	}
	matmul<M2 extends vec<ColumnType<R>, any>>(m: M2): mat<C, ColumnKeys<M2, C>> {
		const out: vec<C, string> = {};
		for (const k in m)
			out[k] = this.mul(m[k] as vec<number, R>);
		return new matImp<C, string>(out) as mat<C, ColumnKeys<M2, C>>;
	}

	inverse(): this {
		const keys 	= Object.keys(this.x) as ColumnKeys<C, number>[];
		const mat	= this.columns().map(row => row.dup());
		const n		= mat.length;
		const inv	= Array.from({length: n}, (_, i) => this.x.create(...Array.from({length: n}, (_, j) => i === j ? 1 : 0)));
		for (let i = 0; i < n; ++i) {
			const k = keys[i];
			const pivot = mat[i][k] as unknown as number;
			if (pivot === 0)
				throw new Error('Matrix is singular');
			mat[i].selfScale(1 / pivot);
			inv[i].selfScale(1 / pivot);
			for (let j = 0; j < n; ++j) {
				if (j !== i) {
					const factor = mat[j][k] as unknown as number;
					mat[j].selfSub(mat[i].scale(factor));
					inv[j].selfSub(inv[i].scale(factor));
				}
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
	trace(): number {
		let trace = 0;
		for (const k of Object.keys(this))
			trace += (this as any)[k][k] as number;
		return trace;
	}
	characteristic(): polynomialN {
		const keys = Object.keys(this) as R[];
		const n = keys.length;

		function trace() {
			let trace = 0;
			for (let i = 0; i < n; ++i)
				trace += (B_cols[i] as any)[keys[i]] as number;
			return trace;
		}

		const I_cols = Array.from({length: n}, (_, i) => this.x.create(...Array.from({length: n}, (_, j) => i === j ? 1 : 0)));
		const B_cols = this.columns().map(row => row.dup());

		// Start
		const coeffs: number[] = [];

		for (let k = 1; k < n; ++k) {
			const ck	= -trace() / k;
			coeffs.push(ck);

			for (let i = 0; i < n; i++)
				B_cols[i] = this.mul(B_cols[i].add(I_cols[i].scale(ck)) as vec<number, R>);
		}
		coeffs.push(-trace() / n);
		coeffs.reverse();
		return new polynomialN(coeffs);
	}

	// Basic QR-based eigensolver (returns array of complex eigenvalues).
	eigenvalues(): complex[] {
		const keys = Object.keys(this) as R[];
		const n = keys.length;

		// For small matrices (<= 5) prefer the polynomial solver (uses closed-form/Aberth)
		if (n <= 5) {
			const roots = this.characteristic().allRoots();
			return roots.map(r => typeof r === 'number' ? complex(r, 0) : complex(r.r, r.i));
		}

		const	tol = 1e-12;
		const	maxIter = Math.max(1000, 100 * n);
		const	eigs: complex[] = [];
		const	cols = this.columns().map(c => c.dup() as C & vec<number, string>);
		let		m = n;
		for (let iter = 0; m > 0 && iter < maxIter; iter++) {
			if (m === 1) {
				eigs.push(complex((cols[0])[keys[0]], 0));
				m = 0; // mark fully extracted to avoid duplicate push in final extraction
				break;
			}
			const a = cols[m - 2][keys[m - 2]];
			const b = cols[m - 1][keys[m - 2]];
			const c = cols[m - 2][keys[m - 1]];
			const d = cols[m - 1][keys[m - 1]];

			// Check deflation by looking at entry (m-1, m-2)
			if (Math.abs(c || 0) <= tol * (Math.abs(a) + Math.abs(d))) {
				eigs.push(complex(d, 0));
				m -= 1;
				continue;
			}

			// Wilkinson shift from bottom 2x2 (extracting using the truncated top-m entries)
			const tr	= a + d;
			const det	= a * d - b * c;
			const disc	= tr * tr - 4 * det;
			let mu: number;
			if (disc >= 0) {
				const s = Math.sqrt(disc);
				const mu1 = 0.5 * (tr + s);
				const mu2 = 0.5 * (tr - s);
				mu = Math.abs(mu1 - d) < Math.abs(mu2 - d) ? mu1 : mu2;
			} else {
				mu = tr * 0.5;
			}

			// Build truncated columns for top m x m block and apply shift
			const ScolsTrunc = cols.slice(0, m).map(c => new floatN(...Object.values(c).slice(0, m)));
			for (let j = 0; j < m; ++j)
				ScolsTrunc[j].values[j] -= mu;

			// QR decompose truncated S
			const { Q, R } = qrDecomposeCols(ScolsTrunc);
			const RQcols = matmulN(R, Q);

			// A_next (top m block) = R * Q + mu * I; write back into fullCols top m entries
			for (let j = 0; j < m; ++j) {
				for (let i = 0; i < m; ++i)
					(cols[j] as vec<number, string>)[keys[i]] = RQcols[j].values[i] + (i === j ? mu : 0);
			}
		}

		// If not fully converged, extract remaining eigenvalues from trailing blocks
		while (m > 0) {
			if (m === 1) {
				eigs.push(complex(cols[0][keys[0]], 0));
				break;
			}

			const a = cols[m - 2][keys[m - 2]];
			const b = cols[m - 1][keys[m - 2]];
			const c = cols[m - 2][keys[m - 1]];
			const d = cols[m - 1][keys[m - 1]];

			if (Math.abs(c || 0) <= tol * (Math.abs(a) + Math.abs(d))) {
				eigs.push(complex(cols[m - 1][keys[m - 1]], 0));
				m -= 1;
				continue;
			}
			// take 2x2 block - delegate to polynomial solver for correctness
			const roots = new polynomialN([a * d - b * c, -(a + d)]).allRoots();
			for (const r of roots)
				eigs.push(typeof r === 'number' ? complex(r, 0) : complex(r.r, r.i));
			m -= 2;
		}

		// eigenvalues collected bottom-up; reverse to have original order
		return eigs.reverse();
	}
}

// Modified Gram-Schmidt on truncated column vectors. Returns Q (array of floatNs) and R (numeric m x m upper-triangular).
function qrDecomposeCols(cols: floatN[]) {
	const n = cols.length;
	const Q: floatN[] = [];
	const R = cols.map((v, j) => {
		const Rj: number[] = [];
		for (let i = 0; i < j; ++i) {
			const rij = Q[i].dot(v);
			Rj[i] = rij;
			v.selfSub(Q[i].scale(rij));
		}
		const norm = v.len();
		Rj[j] = norm;
		Q[j] = norm ? v.scale(1 / norm) : floatN.zeros(n);
		return floatN.fromArray(Rj);
	});
	return { Q, R };
}

export function matClass<C extends vops<C>, R extends string>() {
	return matImp as new (cols: vec<C, R>) => mat<C, R>;
}

// Generic matrix multiply
export function matmul<C extends vops<C>, R extends string, M2 extends vec<ColumnType<R>, any>>(a: matOps<C, R>, b: M2) {
	return a.matmul(b);
}

// Generic matrix multiply with default for missing columns/rows
export function matmul0<C1 extends vops<C1>, R1 extends string, R2 extends string>(a: matOps<C1, R1>, b: vec<any, R2>): mat<C1, R1|R2> {
	const out: vec<C1, string> = {};
	const m1	= a as {[K in R2]: C1};
	const m2	= b as {[K in R2]: vec<number, string>};

	for (const k in m1)
		out[k] = k in m2 ? a.mul0(m2[k], k) : m1[k];

	for (const k in m1) {
		if (!(k in out))
			out[k] = m1[k];
	}

	return new matImp<C1, string>(out) as mat<C1, R1|R2>;
}

export function mat<C extends vops<C>, R extends string>(m: Record<R, C>) {
	return (new matImp<C, R>(m)) as mat<C, R>;
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
	Object.defineProperty(obj, (x as string) + (y as string) + (z as string) + (w as string), {
		get(this: O) { return make(this[x], this[y], this[z], this[w]); },
		...(set ? { set(this: O, v: vec<any, E4>) { this[x] = v.x; this[y] = v.y; this[z] = v.z; this[w] = v.w; } } : {})
	});
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

export function safeNormalise<C extends vops<C>>(a: C): C | undefined {
	const d = a.len();
	return d ? a.scale(1 / d) : undefined;
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
	dup(this: float2) 				{ return float2(this.x, this.y); },
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


// float2x2
export type float2x2 = mat<float2, E2> & {
	mulPos(v: float2): float2;
};
class _float2x2 extends matClass<float2, E2>() {
	mulPos(v: float2)	{ return this.mul(v); }
	det()				{ return this.x.cross(this.y); }
	inverse(): this		{ const r = 1 / this.det(); return this.create(float2(this.y.y * r, -this.x.y * r), float2(-this.y.x * r, this.x.x * r)); }

	// analytic eigenvalues for 2x2 matrix
	eigenvalues(): complex[] {
		const roots = new polynomial([this.det(), -this.trace(), 1]).allRoots();
		return roots.map(r => typeof r === 'number' ? complex(r, 0) : complex(r.r, r.i));
	}
}
export const float2x2 = Object.assign(
	function(x: float2, y: float2): float2x2 {
		return new _float2x2({x, y});
	}, {
	// statics
	identity() {
		return float2x2(float2(1,0), float2(0,1));
	},
});
float2x2.prototype = _float2x2.prototype;

// float2x3
export type float2x3 = mat<float2, E3> & {
	mulPos(v: float2): float2;
	mulAffine(this: float2x3, b: float2x3|float2x2): float2x3;
};

class _float2x3 extends matClass<float2, E3>() {
	mulPos(v: float2) { return this.mul({...v, z: 1}); }
	mulAffine(b: float2x3|float2x2): float2x3 { return mulAffine2x3(this, b); }
	inverse(): this {
		const i = float2x2(this.x, this.y);
		return this.create(i.x, i.y, i.mul(this.z));
	}
}
export const float2x3 = Object.assign(
	function(x: float2, y: float2, z: float2): float2x3 {
		return new _float2x3({ x, y, z });
	}, {
	// statics
	identity() { return float2x3(float2(1,0), float2(0,1), float2(0,0)); }
});
float2x3.prototype = _float2x3.prototype;

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
	// returns a perpendicular assuming `this` is already unit-length (fast)
	perpUnit(): float3;
}

export const float3 = Object.assign(
	function(this: float3, x: number, y: number, z: number) {
		if (!this)
			return new float3(x, y, z);
		this.x = x;
		this.y = y;
		this.z = z;
	} as {
		(x: number, y: number, z: number): float3;       // Callable signature
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
	dup(this: float3) 				{ return float3(this.x, this.y, this.z); },
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
	perp(this: float3) 				{ return normalise(this).perpUnit(); },
	cross(this: float3, b: float3) 	{ return this.yzx.mul(b.zxy).sub(this.zxy.mul(b.yzx)); },
	perpUnit(this: float3) 			{
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

// float3x3
export type float3x3 = mat<float3, E3>;
export const float3x3 = Object.assign(
	function(x: float3, y: float3, z: float3) {
		return new matImp<float3, E3>({x, y, z}) as unknown as float3x3;
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
//float3x3.prototype.det = function(this: float3x3) {
//	return this.x.cross(this.y);
//};

// float3x4
export type float3x4 = mat<float3, E4> & {
	mulPos(v: float3): float3;
	mulAffine(this: float3x4, b: float3x4|float3x3): float3x4;
}
class _float3x4 extends matClass<float3, E4>() {
	mulPos(this: float3x4,v: float3) { return this.mul({...v, w: 1}); }
	mulAffine(b: float3x4|float3x3): float3x4 { return mulAffine3x4(this, b); }
}
export const float3x4 = Object.assign(
	function(x: float3, y: float3, z: float3, w: float3): float3x4 {
		return new _float3x4({x, y, z, w});
	}, {
	// statics
	identity() {
		return float3x4(float3(1,0,0), float3(0,1,0), float3(0,0,1), float3(0,0,0));
	},
});
float3x4.prototype = _float3x4.prototype;

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
	dup(this: float4) 				{ return float4(this.x, this.y, this.z, this.w); },
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
		for (let k = 1; k < 4; ++k) {
			if (Math.abs(comps[k]) > Math.abs(comps[i]))
				i = k;
		}
		let j = i === 0 ? 1 : 0;
		for (let k = 0; k < 4; ++k) {
			if (k != i && Math.abs(comps[k]) > Math.abs(comps[j]))
				j = k;
		}
		const out = [0, 0, 0, 0];
		out[i] = -comps[j];
		out[j] =  comps[i];
		return float4(out[0], out[1], out[2], out[3]);
	},
	toString(this: float4) 			{ return `(${this.x}, ${this.y}, ${this.z}, ${this.w})`; },
	[Symbol.for("debug.description")](this: float4) { return this.toString(); }
});
add_swizzles4(float4.prototype, float2, float3, float4);

export type float4x4 = mat<float4, E4>;
export const float4x4 = Object.assign(
	function(x: float4, y: float4, z: float4, w: float4) {
		return new (matClass<float4, E4>())({x, y, z, w}) as float4x4;
	}, {
	// statics
	identity() {
		return float4x4(float4(1,0,0,0), float4(0,1,0,0), float4(0,0,1,0), float4(0,0,0,1));
	},
	basis(v: float4): float4x4 {
		const nv = safeNormalise(v);
		if (!nv)
			return float4x4.identity();

		// Householder vector
		const usex	= Math.abs(nv.x) < Math.SQRT1_2;
		const uu	= nv.sub(usex ? float4(1,0,0,0) : float4(0,1,0,0));
		const denom = uu.dot(uu);

		// Build H = I - 2 * (uu uu^T) / denom
		function applyH(p: float4) {
			return p.sub(uu.scale(2 * uu.dot(p) / denom));
		}

		// Apply H to standard basis vectors e1,e2,e3 to get orthonormal complement
		return usex ? float4x4(
			applyH(float4(0,1,0,0)),
			applyH(float4(0,0,1,0)),
			applyH(float4(0,0,0,1)),
			nv
		) :	float4x4(
			applyH(float4(1,0,0,0)),
			applyH(float4(0,0,1,0)),
			applyH(float4(0,0,0,1)),
			nv
		);
	}
});

//-----------------------------------------------------------------------------
// ND
//-----------------------------------------------------------------------------

export class floatN extends vops<floatN> {
	values: number[];

	constructor(...v: number[]) {
		super();
		this.values = v;
	}

	static zeros(n: number) {
		return new floatN(...Array(n).fill(0));
	}
	static fromArray(v: number[]) {
		return new floatN(...v);
	}
	static fromVec(v: vec<number, string>) {
		return this.fromArray(Object.values(v));
	}

	dup() 				{ return new floatN(...this.values); }
	neg() 				{ return new floatN(...this.values.map(x => -x)); }
	abs() 				{ return new floatN(...this.values.map(x => Math.abs(x))); }
	scale(b: number) 	{ return new floatN(...this.values.map(x => x * b)); }
	mul(b: floatN) 		{ return new floatN(...this.values.map((x, i) => x * b.values[i])); }
	div(b: floatN) 		{ return new floatN(...this.values.map((x, i) => x / b.values[i])); }
	add(b: floatN) 		{ return new floatN(...this.values.map((x, i) => x + b.values[i])); }
	sub(b: floatN) 		{ return new floatN(...this.values.map((x, i) => x - b.values[i])); }
	min(b: floatN) 		{ return new floatN(...this.values.map((x, i) => Math.min(x, b.values[i]))); }
	max(b: floatN) 		{ return new floatN(...this.values.map((x, i) => Math.max(x, b.values[i]))); }
	equal(b: floatN) 	{ return this.values.length === b.values.length && this.values.every((v, i) => v === b.values[i]); }
	dot(b: floatN) 		{ return this.values.reduce((acc, v, i) => acc + v * b.values[i], 0); }
	perp() 				{ return new floatN(...this.values); }
}

/*
const m6x6 = mat<vector<E6>, E6>(vec(E6,
	vector(E6, 1, 2, 3, 4, 5, 6),
	vector(E6, 7, 8, 9, 10, 11, 12),
	vector(E6, 13, 14, 15, 16, 17, 18),
	vector(E6, 19, 20, 21, 22, 23, 24),
	vector(E6, 25, 26, 27, 28, 29, 30),
	vector(E6, 31, 32, 33, 34, 35, 36)
));

const v6 = vector(E6, 1, 2, 3, 4, 5, 6);

const _mm = m6x6.mul(v6);
const _eig = m6x6.eigenvalues();
*/

function mulN(A: floatN[], b: floatN): floatN {
	const r	= A[0].scale(b.values[0]);
	for (let i = 1; i < A.length; i++) {
		const a = A[i].scale(b.values[i]);
		if (a.values.length > r.values.length)
			r.values = r.values.concat(Array(a.values.length - r.values.length).fill(0));
		r.selfAdd(a);
	}
	return r;
}

function matmulN(A: floatN[], B: floatN[]): floatN[] {
	return B.map(b => mulN(A, b));
}

/*
const m = mat<float4, E4>({
	x: float4(1, 2, 3, 4),
	y: float4(5, 6, 7, 8),
	z: float4(9, 10, 11, 12),
	w: float4(13, 14, 15, 16)
});

const mn = mat<floatN, E4>({
	x: new floatN(1, 2, 3, 4, 1, 2, 3, 4),
	y: new floatN(5, 6, 7, 8, 5, 6, 7, 8),
	z: new floatN(9, 10, 11, 12, 9, 10, 11, 12),
	w: new floatN(13, 14, 15, 16, 13, 14, 15, 16)
});

const _v0 = mn.characteristic();
const _v1 = mn.mul(float4(1, 2, 3, 4));
*/