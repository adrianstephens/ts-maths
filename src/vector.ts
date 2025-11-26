/* eslint-disable no-restricted-syntax */
import {compareT, maxT, minT, ops, scalar, scalarExt, has} from './core';
import complex from './complex';
import { Polynomial, PolynomialN } from './polynomial';
import { floatN, characteristic, eigenvalues, LUSolveBareissMulti, LUSolveBareissMultiT, LUDecomposeBareiss, LUDecomposeBareissT } from './vector2';
import { Blade, BladeT } from './kvector';
import { verticalArray, verticalStyles } from './string';
export { floatN	} from './vector2';

// Core axis types
export const E2	= ['x','y'] as const;
export type E2 = typeof E2[number];

export const E3	= ['x','y','z'] as const;
export type E3 = typeof E3[number];

export const E4	= ['x','y','z','w'] as const;
export type E4 = typeof E4[number];

export const E5	= ['x','y','z','w','c5'] as const;
export type E5	= typeof E5[number];

export const E6	= ['x','y','z','w','c5','c6'] as const;
export type E6	= typeof E6[number];

export type vec<T, E extends string> = {
	[K in E]: T
};
export function vec<T, E extends string>(e: readonly E[], ...v: T[]) {
 	return Object.fromEntries(e.map((k, i) => [k, (v[i] ?? (0 as unknown as T))])) as vec<T, E>;
}

type vecKeys<V, T> = { [K in keyof V]: V[K] extends T ? K : never }[keyof V] & string;

//-----------------------------------------------------------------------------
// swizzles
//-----------------------------------------------------------------------------

type swiz2<T2, E extends string> = { [K in`${E}${E}`]: T2; };
type swiz3<T3, T2, E extends string> = swiz2<T2, E> & { [K in`${E}${E}${E}`]: T3; };
type swiz4<T4, T3, T2, E extends string> = swiz3<T3, T2, E> & { [K in`${E}${E}${E}${E}`]: T4; }

function make_swizzle2<O>(x: keyof O, y: keyof O, set: boolean, make: (x: any, y: any) => any): [string, PropertyDescriptor] {
	return [(x as string) + (y as string), {
		get(this: O) { return make(this[x], this[y]); },
		...(set ? { set(this: O, v: vec<any, E2>) { this[x] = v.x; this[y] = v.y; } } : {})
	}];
}
function make_swizzles2<T>(fields: readonly string[], make2: (x: T, y: T) => any) {
	const props: [string, PropertyDescriptor][] = [];

	for (const x of fields) {
		for (const y of fields)
			props.push(make_swizzle2(x, y, x !== y, make2));
	}
	return props;
}

function make_swizzle3<O>(x: keyof O, y: keyof O, z: keyof O, set: boolean, make: (x: any, y: any, z: any) => any): [string, PropertyDescriptor] {
	return [(x as string) + (y as string) + (z as string), {
		get(this: O) { return make(this[x], this[y], this[z]); },
		...(set ? { set(this: O, v: vec<any, E3>) { this[x] = v.x; this[y] = v.y; this[z] = v.z; } } : {})
	}];
}
function make_swizzles3<T>(fields: readonly string[], make2: (x: T, y: T) => any, make3: (x: T, y: T, z: T) => any) {
	const props: [string, PropertyDescriptor][] = [];

	for (const x of fields) {
		for (const y of fields) {
			const set2 = x !== y;
			props.push(make_swizzle2(x, y, set2, make2));
			for (const z of fields)
				props.push(make_swizzle3(x, y, z, set2 && x !== z && y !== z, make3));
		}
	}
	return props;
}

function make_swizzle4<O>(x: keyof O, y: keyof O, z: keyof O, w: keyof O, set: boolean, make: (x: any, y: any, z: any, w: any) => any): [string, PropertyDescriptor] {
	return [(x as string) + (y as string) + (z as string) + (w as string), {
		get(this: O) { return make(this[x], this[y], this[z], this[w]); },
		...(set ? { set(this: O, v: vec<any, E4>) { this[x] = v.x; this[y] = v.y; this[z] = v.z; this[w] = v.w; } } : {})
	}];
}
function make_swizzles4<T, T2, T3, T4>(fields: readonly string[], make2: (x: T, y: T) => T2, make3: (x: T, y: T, z: T) => T3, make4: (x: T, y: T, z: T, w: T) => T4) {
	const props: [string, PropertyDescriptor][] = [];

	for (const x of fields) {
		for (const y of fields) {
			const set2 = x !== y;
			props.push(make_swizzle2(x, y, set2, make2));
			for (const z of fields) {
				const set3 = set2 && x !== z && y !== z;
				props.push(make_swizzle3(x, y, z, set3, make3));
				for (const w of fields)
					props.push(make_swizzle4(x, y, z, w, set3 && x !== w && y !== w && z !== w, make4));
			}
		}
	}
	return props;
}

//-----------------------------------------------------------------------------
// vector operations
//-----------------------------------------------------------------------------

export interface vops<C extends vops<C, S>, S = any> extends ops<C, S> {
	create(...args: S[]): C;
	_values:			S[];

	dup():				C;
	abs():				C;
	min(b: C):			C;
	max(b: C):			C;
	eq(b: C):			boolean;
	dot(b: C):			S;
	perp():				C;

	lensq(): 			S;
	len(): 				S;
	mag(): 				number | scalarExt<any>;
	selfScale(b: S): 	void;
	selfMul(b: C): 		void;
	selfAdd(b: C): 		void;
	selfSub(b: C): 		void;
	clamp(min: C, max: C):	C;
}

type scalarOf<C> = C extends vops<any, infer S> ? S : number;
export type vector<E extends string, S = number> = vec<S, E> & vops<vector<E, S>, S>;

//-----------------------------------------------------------------------------
// general vector type over numbers
//-----------------------------------------------------------------------------

export function vector<E extends string>(e: readonly E[], ...v: number[]) {
	return (new vecImp<E>(vec(e, ...v))) as vector<E>;
}
function _cross2(a: number[], b: number[]): number {
	return a[0] * b[1] - a[1] * b[0];
}
function _cross3(a: number[], b: number[]): number[] {
	return [
		a[1] * b[2] - a[2] * b[1],
		a[2] * b[0] - a[0] * b[2],
		a[0] * b[1] - a[1] * b[0],
	];
}

export class vecImp<E extends string> implements vops<vector<E>, number> {
	constructor(v: vec<number, E>) {
		Object.assign(this, v);
	}
	private keys()		{ return Object.keys(this) as E[]; }
	private entries()	{ return Object.entries(this) as [E, number][]; }
	private asVec()		{ return this as vec<number, E>; }

	create(...args: number[]): vector<E> {
		const ctor = this.constructor as new (v: vec<number, E>) => vector<E>;
  		return new ctor(Object.fromEntries(Object.keys(this).map((k, i) => [k, args[i]])) as vec<number, E>);
	}
	get _values(): number[]	{ return Object.values(this) as number[]; }

	dup() 					{ return this.create(...this._values); }
	neg() 					{ return this.create(...this._values.map(x => -x)); }
	abs() 					{ return this.create(...this._values.map(x => Math.abs(x))); }
	scale(b: number) 		{ return this.create(...this._values.map(x => x * b)); }
	mul(b: vector<E>) 		{ return this.create(...this.entries().map(([k, v]) => v * b[k])); }
	div(b: vector<E>) 		{ return this.create(...this.entries().map(([k, v]) => v / b[k])); }
	add(b: vector<E>) 		{ return this.create(...this.entries().map(([k, v]) => v + b[k])); }
	sub(b: vector<E>) 		{ return this.create(...this.entries().map(([k, v]) => v - b[k])); }
	min(b: vector<E>) 		{ return this.create(...this.entries().map(([k, v]) => Math.min(v, b[k]))); }
	max(b: vector<E>) 		{ return this.create(...this.entries().map(([k, v]) => Math.max(v, b[k]))); }
	eq(b: vector<E>) 		{ return this.entries().every(([k, v]) => v === b[k]); }
	dot(b: vector<E>) 		{ return this.entries().reduce((acc, [k, v]) => acc + v * b[k], 0); }
	perp() 					{
		const comps = this._values;
		const i = comps.reduce((minI, c, k) => Math.abs(c) < Math.abs(comps[minI]) ? k : minI, 0);
		const x = comps[i] / this.lensq();
		return this.create(...comps.map((c, j) => (j === i ? 1 : 0) - x * c));
	}

	lensq() 				{ return this.dot(this as vector<E>); }
	len() 					{ return Math.sqrt(this.lensq()); }
	mag()					{ return this.len(); }
	selfScale(b: number) 	{ const v = this.asVec(); for (const k of this.keys()) v[k] *= b; }
	selfMul(b: vector<E>) 	{ const v = this.asVec(); for (const k of this.keys()) v[k] *= b[k]; }
	selfAdd(b: vector<E>) 	{ const v = this.asVec(); for (const k of this.keys()) v[k] += b[k]; }
	selfSub(b: vector<E>) 	{ const v = this.asVec(); for (const k of this.keys()) v[k] -= b[k]; }
	clamp(min: vector<E>, max: vector<E>) 	{ return this.max(min).min(max); }
	cross(b: vector<E>) {
		switch (this.keys().length) {
			case 2:	return _cross2(this._values, b._values);
			case 3:	return this.create(..._cross3(this._values, b._values));
			default: return Blade.from(this._values, b._values);
		}
	}
	toString() 				{ return '('+this._values.join(', ')+')'; }
	[Symbol.for("debug.description")]() { return this.toString(); }
}

export function vecClass<E extends string, S>() {
	return vecImp as unknown as new (v: vec<number, E>) => S;
}

//-----------------------------------------------------------------------------
// general vector type over T
//-----------------------------------------------------------------------------

export interface vscalar<C extends vscalar<C, S>, S=number> extends scalar<C, S> {
	sqrt(): 	C;
	recip():	C;
}

export function vectorT<T extends vscalar<T>, E extends string>(e: readonly E[], ...v: T[]) {
	return (new vecImpT<T, E>(vec(e, ...v))) as vector<E, T>;
}

function _cross2T<T extends vscalar<T>>(a: T[], b: T[]): T {
	return a[0].mul(b[1]).sub(a[1].mul(b[0]));
}
function _cross3T<T extends vscalar<T>>(a: T[], b: T[]): T[] {
	return [
		a[1].mul(b[2]).sub(a[2].mul(b[1])),
		a[2].mul(b[0]).sub(a[0].mul(b[2])),
		a[0].mul(b[1]).sub(a[1].mul(b[0])),
	];
}

export class vecImpT<T extends vscalar<T>, E extends string> implements vops<vector<E, T>, T> {
	constructor(v: vec<T, E>) {
		Object.assign(this, v);
	}
	private keys()		{ return Object.keys(this) as E[]; }
	private entries()	{ return Object.entries(this) as [E, T][]; }
	private asVec()		{ return this as vec<T, E>; }

	create(...args: T[]): vector<E, T> {
		const ctor = this.constructor as new (v: vec<T, E>) => vector<E, T>;
  		return new ctor(Object.fromEntries(Object.keys(this).map((k, i) => [k, args[i]])) as vec<T, E>);
	}

	get _values() : T[]		{ return Object.values(this) as T[]; }
	dup() 					{ return this.create(...this._values); }
	neg() 					{ return this.create(...this._values.map(x => x.neg())); }
	abs() 					{ return this.create(...this._values.map(x => x.abs())); }
	scale(b: T) 			{ return this.create(...this._values.map(x => x.mul(b))); }
	mul(b: vector<E, T>) 	{ return this.create(...this.entries().map(([k, v]) => v.mul(b[k]))); }
	div(b: vector<E, T>) 	{ return this.create(...this.entries().map(([k, v]) => v.div(b[k]))); }
	add(b: vector<E, T>) 	{ return this.create(...this.entries().map(([k, v]) => v.add(b[k]))); }
	sub(b: vector<E, T>) 	{ return this.create(...this.entries().map(([k, v]) => v.sub(b[k]))); }
	min(b: vector<E, T>) 	{ return this.create(...this.entries().map(([k, v]) => minT(v, b[k]))); }
	max(b: vector<E, T>) 	{ return this.create(...this.entries().map(([k, v]) => maxT(v, b[k]))); }
	eq(b: vector<E, T>) 	{ return this.entries().every(([k, v]) => compareT(v, b[k]) === 0); }
	dot(b: vector<E, T>) 	{ return this.entries().reduce((acc, [k, v]) => acc.add(v.mul(b[k])), this._values[0].from(0)); }
	perp() 					{
		const comps = this._values;
		const i = comps.reduce((minI, c, k) => c.abs().lt(comps[minI].abs()) ? k : minI, 0);
		const x = comps[i].div(this.lensq());
		const zero = x.from(0), one = x.from(1);
		return this.create(...comps.map((c, j) => (j === i ? one : zero).sub(x.mul(c))));
	}

	lensq() 					{ return this.dot(this as vector<E, T>); }
	len() 						{ return this.lensq().sqrt(); }
	mag()						{ return this.len().mag(); }
	selfScale(b: T) 			{ const v = this.asVec(); for (const k of this.keys()) v[k] = v[k].mul(b); }
	selfMul(b: vector<E, T>) 	{ const v = this.asVec(); for (const k of this.keys()) v[k] = v[k].mul(b[k]); }
	selfAdd(b: vector<E, T>) 	{ const v = this.asVec(); for (const k of this.keys()) v[k] = v[k].add(b[k]); }
	selfSub(b: vector<E, T>) 	{ const v = this.asVec(); for (const k of this.keys()) v[k] = v[k].sub(b[k]); }
	clamp(min: vector<E, T>, max: vector<E, T>) 	{ return this.max(min).min(max); }

	cross(b: vector<E, T>) {
		switch (this.keys().length) {
			case 2:	return _cross2T(this._values, b._values);
			case 3: return this.create(..._cross3T(this._values, b._values));
			default: return BladeT.from(this._values, b._values);
		}
	}

	toString() 				{ return '('+this._values.join(', ')+')'; }
	[Symbol.for("debug.description")]() { return this.toString(); }
}

export function vecClassT<T extends vscalar<T>, E extends string, S>() {
	return vecImpT<T, E> as unknown as new (v: vec<T, E>) => S;
}

//-----------------------------------------------------------------------------
// Matrix operations
//-----------------------------------------------------------------------------

export type ColumnType<E extends string, S = any> = vops<ColumnType<E, S>, S> & vec<S, E>;

export interface matOps<C extends vops<C, any>, R extends string, S = scalarOf<C>> {
	create(...cols: C[]):			this;
	columns():						C[];
	inverse():						this;
	det():							S;
	scale(s: S):					mat<C, R>;
	add(b: mat<C, R>):				mat<C, R>;
	mul(v: vec<S, R>):				C;
	mul0(v: vec<S, string>, col: string):	C;
	matmul<M2 extends vec<ColumnType<R>, any>>(m: M2): mat<C, vecKeys<M2, C>>;
	trace():						S;
	characteristic():				PolynomialN<number>;
	eigenvalues():					complex[];
}

export type mat<C extends vops<C, any>, R extends string> = matOps<C, R> & vec<C, R>;

class matImp<C extends vops<C>, R extends string> {
	x!: C;
	constructor(cols: vec<C, R>) {
		Object.assign(this, cols);
	}
	create(...cols: C[]): this {
 		const ctor = this.constructor as new (cols: vec<C, string>) => any;
  		return new ctor(Object.fromEntries(Object.keys(this).map((k, i) => [k, cols[i]])));
	}
	create2(cols: vec<C, string>): mat<C, string> {
 		const ctor = this.constructor as new (cols: vec<C, string>) => any;
  		return new ctor(cols);
	}
	columns(): C[] {
		return Object.values(this) as C[];
	}
	scale(s: scalarOf<C>): mat<C, R> {
		const m1	= this as {[K in R]: C};
		const out: vec<C, string> = {};
		for (const k in m1)
			out[k] = m1[k].scale(s);
		return this.create2(out);
	}
	add(b: mat<C, R>): mat<C, R> {
		const m1	= this as {[K in R]: C};
		const out: vec<C, string> = {};
		for (const k in m1)
			out[k] = m1[k].add(b[k]);
		return this.create2(out);
	}
	mul(v: vec<scalarOf<C>, R>): C {
		const m1	= this as {[K in R]: C};
		const r		= this.x.scale(0);
		for (const k in m1)
			r.selfAdd(m1[k].scale(v[k]));
		return r;
	}
	mul0(v: vec<scalarOf<C>, string>, col: string): C {
		const m1	= this as {[K in R]: C};
		const r		= this.x.scale(0);
		for (const k in m1)
			r.selfAdd(m1[k].scale(v[k] ?? (k == col ? 1 : 0)));
		return r;
	}
	matmul<M2 extends vec<ColumnType<R>, any>>(m: M2): mat<C, vecKeys<M2, C>> {
		const out: vec<C, string> = {};
		for (const k in m)
			out[k] = this.mul(m[k] as vec<scalarOf<C>, R>);
		return this.create2(out);
	}
	toString(multiline = true)			{
		if (multiline) {
			const all = this.columns().map(c => {
				const col = c._values.map(String);
				const width = Math.max(...col.map(s => s.length));
				for (let i = 0; i < col.length; i++)
					col[i] = col[i].padStart(width / 2, ' ').padEnd(width, ' ');
				return col;
			});
			return verticalArray(Array.from({ length: all[0].length }, (_, r) => all.map(col => col[r]).join('  ')), verticalStyles.bigBraces);
		} 
		return '('+this.columns().join(', ')+')';
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
}

class matImpN<C extends vops<C, number>, R extends string> extends matImp<C,R> implements matOps<C, R> {
	inverse(): this {
		const A		= this.columns().map(col => col._values);
		const n		= A.length;
	
		const { perm, swaps } = LUDecomposeBareiss(A, true);
		const B		= Array.from({ length: n }, (_, j) => Array.from({ length: n }, (_, i) => i === j ? 1 : 0));
		const inv	= LUSolveBareissMulti(A, B, swaps ? perm : undefined);
		return this.create(...inv.map(c => this.x.create(...c)));
	}
	det(): scalarOf<C> {
		const A = this.columns().map(col => col._values);
		const n = A.length;
		const { swaps } = LUDecomposeBareiss(A, true);
		return (swaps & 1 ? -A[n - 1][n - 1] : A[n - 1][n - 1]) as scalarOf<C>;
	}
	trace(): scalarOf<C> {
		let trace = 0;
		for (const k of Object.keys(this))
			trace += (this as any)[k][k];
		return trace as scalarOf<C>;
	}
	characteristic(): PolynomialN<number> {
		return characteristic(this.columns().map(col => floatN.fromArray(col._values)));
	}

	eigenvalues(): complex[] {
		return eigenvalues(this.columns().map(col => floatN.fromArray(col._values)));
	}
}

class matImpT<C extends vops<C, T>, R extends string, T extends vscalar<T> = any> extends matImp<C,R> implements matOps<C, R> {
	inverse(): this {
		const A		= this.columns().map(col => col._values);
		const n		= A.length;
		const zero	= A[0][0].from(0), one = A[0][0].from(1);

		const { perm, swaps } = LUDecomposeBareissT(A, true);
		const B		= Array.from({ length: n }, (_, j) => Array.from({ length: n }, (_, i) => i === j ? one : zero));
		const inv	= LUSolveBareissMultiT(A, B, swaps ? perm : undefined);
		return this.create(...inv.map(c => this.x.create(...c)));
	}
	det(): scalarOf<C> {
		const A = this.columns().map(col => col._values);
		const n = A.length;
		const { swaps } = LUDecomposeBareissT(A, true);
		return (swaps & 1 ? A[n - 1][n - 1].neg() : A[n - 1][n - 1]) as scalarOf<C>;
	}
	trace(): scalarOf<C> {
		const a = Object.values(this.x)[0];
		let trace = a.from(0);
		for (const k of Object.keys(this))
			trace = trace.add((this as any)[k][k]);
		return trace as scalarOf<C>;
	}
	characteristic(): PolynomialN<number> {
		return PolynomialN([0]);//new polynomialN([]);
	}
	eigenvalues(): complex[] {
		return [];
	}
}

//-----------------------------------------------------------------------------
// Generic matrix functions
//-----------------------------------------------------------------------------

export function matClass<C extends vops<C>, R extends string>() {
	return matImpN as new (cols: vec<C, R>) => mat<C, R>;
}

// create a matrix
export function mat<C extends vops<C>, R extends string>(m: vec<C, R>) {
	const v = (Object.values(m)[0] as C)._values[0];
	if (typeof v === 'number')
		return (new matImpN<C, R>(m)) as mat<C, R>;
	else
		return (new matImpT<C, R>(m)) as mat<C, R>;
}

// Generic matrix multiply
export function matmul<C extends vops<C>, R extends string, M2 extends vec<ColumnType<R>, any>>(a: matOps<C, R>, b: M2) {
	return a.matmul(b);
}

// Generic matrix multiply with default for missing columns/rows
export function matmulExt<C1 extends vops<C1>, R1 extends string, R2 extends string>(a: matOps<C1, R1>, b: vec<any, R2>): mat<C1, R1|R2> {
	const out: vec<C1, string> = {};
	const m1	= a as {[K in R2]: C1};
	const m2	= b as {[K in R2]: vec<scalarOf<C1>, string>};

	for (const k in m1)
		out[k] = k in m2 ? a.mul0(m2[k], k) : m1[k];

	for (const k in m1) {
		if (!(k in out))
			out[k] = m1[k];
	}

	return new matImpN<C1, string>(out) as mat<C1, R1|R2>;
}

export function mulExt<C extends vops<C>, R extends string>(a: matOps<C, R>, v: vec<any, string>): C {
	const m1 = a as {[K in R]: C};
	let r: C | undefined;
	let k: R | undefined;
	for (const k1 in m1) {
		if (k) {
			if (k in v) {
				const r0 = m1[k].scale(v[k]);
				if (r)
					r.selfAdd(r0);
				else
					r = r0;
			}
		}
		k = k1;
	}
	const r0 = (k!) in v ? m1[k!].scale(v[k!]) : m1[k!];
	if (r)
		r.selfAdd(r0);
	else
		r = r0;
	return r;
}

//-----------------------------------------------------------------------------
// general vector extent
//-----------------------------------------------------------------------------

class extentV<C extends vops<C>> {
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
	combine(b: extentV<C>) {
		this.min = this.min.min(b.min);
		this.max = this.max.max(b.max);
	}
	encompasses(b: extentV<C>) {
		return this.min.min(b.min).eq(this.min) && this.max.max(b.max).eq(this.max);
	}
	overlaps(b: extentV<C>) {
		return this.min.max(b.min).eq(this.min) && this.max.min(b.max).eq(this.max);
	}
	contains(p: C) {
		return this.min.max(p).eq(this.min) && this.max.min(p).eq(this.max);
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

export function normalise<T extends has<'recip'> | number, C extends vops<C, T>>(a: C) {
	const len = a.len();
	return a.scale(typeof len === 'number' ? 1 / len : len.recip());
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
	return t.max(d).eq(t);
}

export function safeNormalise<C extends vops<C>>(a: C): C | undefined {
	const d = a.len();
	if (typeof d === 'number')
		return d ? a.scale(1 / d) : undefined;
	return d.sign() ? a.scale(d.recip()) : undefined;
}

//-----------------------------------------------------------------------------
// 2D
//-----------------------------------------------------------------------------

export interface float2 extends vec<number, E2>, swiz2<float2, E2>, vops<float2> {
	cross(b: float2): number;
	atan2(): number;
}

class _float2 extends vecClass<E2, float2>() {
	dup() 				{ return float2(this.x, this.y); }
	neg() 				{ return float2(-this.x, -this.y); }
	abs() 				{ return float2(Math.abs(this.x), Math.abs(this.y)); }
	scale(b: number) 	{ return float2(this.x * b, this.y * b); }
	mul(b: float2) 		{ return float2(this.x * b.x, this.y * b.y); }
	div(b: float2) 		{ return float2(this.x / b.x, this.y / b.y); }
	add(b: float2) 		{ return float2(this.x + b.x, this.y + b.y); }
	sub(b: float2) 		{ return float2(this.x - b.x, this.y - b.y); }
	min(b: float2) 		{ return float2(Math.min(this.x, b.x), Math.min(this.y, b.y)); }
	max(b: float2) 		{ return float2(Math.max(this.x, b.x), Math.max(this.y, b.y)); }
	eq(b: float2) 		{ return this.x === b.x && this.y === b.y; }
	dot(b: float2) 		{ return this.x * b.x + this.y * b.y; }
	perp() 				{ return float2(-this.y, this.x); }
	cross(b: float2) 	{ return this.x * b.y - this.y * b.x; }
	atan2() 			{ return Math.atan2(this.y, this.x); }
}

export const float2 = Object.assign(
	function(x: number, y: number) {
		return new _float2({x, y}) as unknown as float2;
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
Object.defineProperties(_float2.prototype, Object.fromEntries(make_swizzles2(E2, float2)));
float2.prototype = _float2.prototype;

export class extent2 extends extentV<float2> {
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
	overlaps(b: extentV<float2>) {
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

	eigenvalues(): complex[] {
		//return new polynomial([this.det(), -this.trace(), 1]).allRoots();
		return Polynomial([this.det(), -this.trace(), 1]).allRoots!();
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
	perpUnit(): float3;
}

class _float3 extends vecClass<E3, float3>() {
	dup() 				{ return float3(this.x, this.y, this.z); }
	neg() 				{ return float3(-this.x, -this.y, -this.z); }
	abs() 				{ return float3(Math.abs(this.x), Math.abs(this.y), Math.abs(this.z)); }
	scale(b: number) 	{ return float3(this.x * b, this.y * b, this.z * b); }
	mul(b: float3) 		{ return float3(this.x * b.x, this.y * b.y, this.z * b.z); }
	div(b: float3) 		{ return float3(this.x / b.x, this.y / b.y, this.z / b.z); }
	add(b: float3) 		{ return float3(this.x + b.x, this.y + b.y, this.z + b.z); }
	sub(b: float3) 		{ return float3(this.x - b.x, this.y - b.y, this.z - b.z); }
	min(b: float3) 		{ return float3(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z)); }
	max(b: float3) 		{ return float3(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z)); }
	eq(b: float3) 		{ return this.x === b.x && this.y === b.y && this.z === b.z; }
	dot(b: float3) 		{ return this.x * b.x + this.y * b.y + this.z * b.z; }
	perp() 				{ return normalise(this as float3).perpUnit(); }
	cross(b: float3) 	{ return this.yzx.mul(b.zxy).sub(this.zxy.mul(b.yzx)); }
	perpUnit() 			{
		const s = this.z < 0 ? -1 : 1;
		const a = -this.y / (s + this.z);
		return float3(this.x * a, s + this.y * a, -this.y);
	}
}
export const float3 = Object.assign(
	function(x: number, y: number, z: number) {
		return new _float3({x, y, z}) as unknown as float3;
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
Object.defineProperties(_float3.prototype, Object.fromEntries(make_swizzles3(E3, float2, float3)));
float3.prototype = _float3.prototype;

export class extent3 extends extentV<float3> {
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
		return new matImpN<float3, E3>({x, y, z}) as unknown as float3x3;
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

class _float4 extends vecClass<E4, float4>() {
	dup() 				{ return float4(this.x, this.y, this.z, this.w); }
	neg() 				{ return float4(-this.x, -this.y, -this.z, -this.w); }
	abs() 				{ return float4(Math.abs(this.x), Math.abs(this.y), Math.abs(this.z), Math.abs(this.w)); }
	scale(b: number) 	{ return float4(this.x * b, this.y * b, this.z * b, this.w * b); }
	mul(b: float4) 		{ return float4(this.x * b.x, this.y * b.y, this.z * b.z, this.w * b.w); }
	div(b: float4) 		{ return float4(this.x / b.x, this.y / b.y, this.z / b.z, this.w / b.w); }
	add(b: float4) 		{ return float4(this.x + b.x, this.y + b.y, this.z + b.z, this.w + b.w); }
	sub(b: float4) 		{ return float4(this.x - b.x, this.y - b.y, this.z - b.z, this.w - b.w); }
	min(b: float4) 		{ return float4(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z), Math.min(this.w, b.w)); }
	max(b: float4) 		{ return float4(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z), Math.max(this.w, b.w)); }
	eq(b: float4) 		{ return this.x === b.x && this.y === b.y && this.z === b.z && this.w === b.w; }
	dot(b: float4) 		{ return this.x * b.x + this.y * b.y + this.z * b.z + this.w * b.w; }
}

export const float4 = Object.assign(
	function(x: number, y: number, z: number, w: number) {
		return new _float4({x, y, z, w}) as unknown as float4;
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
Object.defineProperties(_float4.prototype, Object.fromEntries(make_swizzles4(E4, float2, float3, float4)));
float4.prototype = _float4.prototype;

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
