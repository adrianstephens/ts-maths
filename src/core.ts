import { randomBytes } from 'crypto';

export function isInstance<T>(x: any, i: new (...args: any[]) => T): x is T {
	return x instanceof i;
}

export interface Operators<T> {
	from(n: number): T;
	func(name: string, args: T[]): T | undefined;
	variable(name: string): T | undefined;
	dup(a: T): T;
	neg(a: T): T;
	pow(a: T, b: T): T;
	mul(a: T, b: T): T;
	div(a: T, b: T): T;
	add(a: T, b: T): T;
	sub(a: T, b: T): T;
	eq(a: T, b: T): boolean;
	lt(a: T, b: T): boolean;
}

export abstract class OperatorsBase<T extends ops<T>> implements Operators<T> {
	abstract from(n: number): T;
	abstract func(name: string, args: T[]): T | undefined;
	abstract variable(name: string): T | undefined;
	abstract pow(a: T, b: T): T;
	dup(a: T): T		{ return a.dup(); }
	neg(a: T): T		{ return a.neg(); }
	mul(a: T, b: T): T	{ return a.mul(b); }
	div(a: T, b: T): T	{ return a.div(b); }
	add(a: T, b: T): T	{ return a.add(b); }
	sub(a: T, b: T): T	{ return a.sub(b); }
	eq(_a: T, _b: T): boolean { throw new Error("Method not implemented."); }
	lt(_a: T, _b: T): boolean { throw new Error("Method not implemented."); }
}

export interface ops<C extends ops<C, S>, S=number> {
	dup():				C;
	neg(): 				C;
	scale(b: S):		C;
	mul(b: C):			C;
	div(b: C):			C;
	add(b: C): 			C;
	sub(b: C): 			C;
	mag():				number | scalarExt<any>;
}

const ops = {
	neg<T extends ops<T>>(a: T): T			{ return a.neg(); },
	scale<T extends ops<T, S>, S>(a: T, b: S): T	{ return a.scale(b); },
	mul<T extends ops<T>>(a: T, b: T): T	{ return a.mul(b); },
	div<T extends ops<T>>(a: T, b: T): T	{ return a.div(b); },
	add<T extends ops<T>>(a: T, b: T): T	{ return a.add(b); },
	sub<T extends ops<T>>(a: T, b: T): T	{ return a.sub(b); },
};

export interface scalar<C extends scalar<C, S>, S=number> extends ops<C, S> {
	from(n: number | bigint):	C;
	abs():				C;
	sign():				number;
	lt(b: C):			boolean;
	valueOf():			number | bigint;
}

export interface scalarPow<C extends scalarPow<C>> extends scalar<C> {
	sqrt(): 			C;
	recip():			C;
	rpow(n: number, d: number):	C;
	npow(n: number):	C;
}

export interface scalarExt<C extends scalarExt<C>> extends scalarPow<C> {
	divmod(b: C):		number | bigint;
}

export function compare<T extends number|bigint|string>(a: T, b: T): number {
	return a < b ? -1 : a > b ? 1 : 0;
}

export function* lazySlice<T>(arr: T[], start?: number, end?: number): Generator<T> {
	const len = arr.length;
	if (start === undefined)
		start = 0;
	else if (start < 0)
		start = Math.max(len + start, 0);
	else
		start = Math.min(start, len);

	if (end === undefined)
		end = len;
	else if (end < 0)
		end = Math.max(len + start, 0);
	else
		end = Math.min(end, len);

	for (let i = start; i < end; i++)
		yield arr[i];
}

//-----------------------------------------------------------------------------
// number
//-----------------------------------------------------------------------------

export type integer = number & { __brand: 'Integer' };

export function isInteger(n: number): integer {
    if (!Number.isInteger(n))
        throw new Error('Not an integer');
    return n as integer;
}

export function isAlmostInteger(x: number, epsilon = Number.EPSILON) {
	return Math.abs(x - Math.round(x)) < epsilon;
}

export const numberOperators = {
	dup(n: number): number		{ return n; },
	from(n: number): number		{ return n; },
	func(name: string, args: number[]) {
		const fn = (Math as unknown as Record<string, (...args: number[]) => number>)[name];
		return fn ? fn(...args) : undefined;
	},
	variable(name: string) {
		switch (name) {
			case 'pi':			return Math.PI;
			case 'e':			return Math.E;
			case 'i':			return NaN;
			case 'infinity':	return Infinity;
			default:			return undefined;
		}
	},
	neg(a: number): number				{ return -a; },
	pow(a: number, b: number): number	{ return Math.pow(a, b); },
	mul(a: number, b: number): number	{ return a * b; },
	div(a: number, b: number): number	{ return a / b; },
	add(a: number, b: number): number	{ return a + b; },
	sub(a: number, b: number): number	{ return a - b; },
	eq(a: number, b: number): boolean	{ return a === b; },
	lt(a: number, b: number): boolean	{ return a < b; }
};

export function copySign(a: number, b: number) {
	return b < 0 ? -Math.abs(a) : Math.abs(a);
}

export function sincos(angle: number) {
	return {s: Math.sin(angle), c: Math.cos(angle)};
}
export function sin2cos(x: number) {
	 return Math.sqrt((1 - x) * (1 + x));
}

export function sign(a: number) {
	return a > 0 ? 1 : a < 0 ? -1 : 0;
}

export function gcd(...values: number[]): number {
	let a = 0;
	for (let b of values) {
		if (a) {
			while (b)
				[a, b] = [b, a % b];
		} else {
			a = b;
		}
	}
	return a;
}

export function lcm(...x: number[]) {
	return x.reduce((a, b) => (a / gcd(a, b)) * b, 1);
}

export function denominator(x: number, maxDen: number, eps = Number.EPSILON): number {
	x = Math.abs(x) % 1;
	if (x <= eps)
		return 1;

	let k1 = 1, k2 = 0;
	do {
		x = 1 / x;
		const f = Math.floor(x);
		x -= f;
		[k2, k1] = [k1, f * k1 + k2];
	} while (x > eps && k1 < maxDen);
	return k1;
}

export function commonDenominator(numbers: number[], maxDen = 1000, eps = Number.EPSILON) {
	let scale = 1;
	for (const n of numbers) {
		scale *= denominator(n * scale, maxDen, eps);
		if (scale > maxDen)
			return 0;
	}
	return scale;
}

export function rationalApprox(x: number, maxDen: number, eps = Number.EPSILON): [number, number] {
	// binary check for exact representation
	const bits	= 1 << 20;
	const m		= x * bits;
	if (Number.isInteger(m)) {
		const d = bits / (m & -m);
		if (d < maxDen)
			return [x * d, d];
	}
	
	let h1 = Math.floor(x), h2 = 1;
	let k1 = 1, k2 = 0;
	let b = x - h1;

	while (b > eps && k1 < maxDen) {
		b = 1 / b;
		const f = Math.floor(b);
		[h2, h1, k2, k1] = [h1, f * h1 + h2, k1, f * k1 + k2];
		b -= f;
	}
	return [h1, k1];
}

export function modPow(base: number, exp: number, mod: number): number {
	base %= mod;
	let result = 1;
	while (exp > 0) {
		if ((exp & 1) === 1)
			result = (result * base) % mod;
		base = (base * base) % mod;
		exp >>= 1;
	}
	return result;
}

export class scalarN implements scalarExt<scalarN> {
	constructor(public value: number) {}
	dup(): 				scalarN		{ return new scalarN(this.value); }
	neg(): 				scalarN		{ return new scalarN(-this.value); }
	scale(b: number):	scalarN		{ return new scalarN(this.value * b); }
	mul(b: scalarN):	scalarN		{ return new scalarN(this.value * b.value); }
	div(b: scalarN):	scalarN		{ return new scalarN(this.value / b.value); }
	add(b: scalarN):	scalarN		{ return new scalarN(this.value + b.value); }
	sub(b: scalarN):	scalarN		{ return new scalarN(this.value - b.value); }
	mag():				number 		{ return Math.abs(this.value); }

	sign():				number		{ return sign(this.value); }
	abs():				scalarN		{ return new scalarN(Math.abs(this.value)); }
	recip():			scalarN		{ return new scalarN(1 / this.value); }
	divmod(b: scalarN):	number		{ const q = Math.floor(this.value / b.value); this.value -= q * b.value; return q; }
	lt(b: scalarN):		boolean		{ return this.value < b.value; }
	from(n: number | bigint)		{ return new scalarN(Number(n)); }

	sqrt(): 			scalarN		{ return new scalarN(Math.sqrt(this.value)); }
	npow(n: number):	scalarN		{ return new scalarN(this.value ** n); }
	rpow(n: number, d:number)		{ return new scalarN(this.value ** (n / d)); }
//	pow(n: number):		scalarN		{ return new scalarN(this.value ** n); }

	valueOf():			number		{ return this.value; }
	toString()						{ return this.value.toString(); }
}

export function asScalar(x: number|scalar<any>): scalar<any> {
	if (typeof x === 'number')
		return new scalarN(x);
	return x;
}
export function asScalarExt(x: number|scalarExt<any>): scalarExt<any> {
	if (typeof x === 'number')
		return new scalarN(x);
	return x;
}
export function asScalarT<T extends scalarExt<T>>(from: T, x: number|T): T {
	if (typeof x === 'number')
		return from.from(x);
	if (x instanceof from.constructor)
		return x;
	return from.from(+x);
}

export class extent {
	static fromCentreExtent(centre: number, size: number) {
		const half = size * 0.5;
		return new extent(centre - half, centre + half);
	}
	static from<U extends Iterable<number>>(items: U) {
		const ext = new extent;
		for (const i of items)
			ext.add(i);
		return ext;
	}
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
	combine(b: extent) {
		this.min = Math.min(this.min, b.min);
		this.max = Math.max(this.max, b.max);
	}
	encompasses(b: extent) {
		return this.min <= b.min && this.max >= b.max;
	}
	overlaps(b: extent) {
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
// bigint
//-----------------------------------------------------------------------------

export function signB(a: bigint): number {
	return a === 0n ? 0 : a < 0n ? -1 : 1;
}
export function absB(a: bigint): bigint {
	return a < 0n ? -a : a;
}
export function maxB(a: bigint, b: bigint): bigint {
	return a < b ? b : a;
}
export function minB(a: bigint, b: bigint): bigint {
	return a < b ? a : b;
}

export function gcdB(...values: bigint[]): bigint {
	let a = 0n;
	for (let b of values) {
		if (a) {
			while (b)
				[a, b] = [b, a % b];
		} else {
			a = b;
		}
	}
	return a;
}

export function lcmB(...x: bigint[]) {
	return x.reduce((a, b) => (a / gcdB(a, b)) * b, 1n);
}

export function modPowB(base: bigint, exp: bigint, mod: bigint): bigint {
	base %= mod;
	let result = 1n;
	while (exp > 0n) {
		if ((exp & 1n) === 1n)
			result = (result * base) % mod;
		base = (base * base) % mod;
		exp >>= 1n;
	}
	return result;
}

export function randomB(bits: number): bigint {
	const buf = randomBytes(Math.ceil(bits / 8));
	let r = 0n;
	for (const i of buf)
		r = (r << 8n) | BigInt(i);
	return r;
}

export class scalarB implements scalarExt<scalarB> {
	constructor(public value: bigint) {}
	dup(): 				scalarB		{ return new scalarB(this.value); }
	neg(): 				scalarB		{ return new scalarB(-this.value); }
	sqrt():				scalarB		{ return new scalarB(rootB(this.value, 2)); }
	scale(b: number):	scalarB		{ return new scalarB(this.value * BigInt(b)); }
	npow(n: number):	scalarB		{ return new scalarB(this.value ** BigInt(n)); }
	rpow(n: number, d:number):	scalarB		{ return new scalarB(this.value ** BigInt(n / d)); }
	//pow(n: number):		scalarB 	{ return new scalarB(this.value ** BigInt(n)); }
	mul(b: scalarB):	scalarB		{ return new scalarB(this.value * b.value); }
	div(b: scalarB):	scalarB		{ return new scalarB(this.value / b.value); }
	add(b: scalarB):	scalarB		{ return new scalarB(this.value + b.value); }
	sub(b: scalarB):	scalarB		{ return new scalarB(this.value - b.value); }
	mag():				number 		{ return Number(absB(this.value)); }

	sign():				number		{ return signB(this.value); }
	abs():				scalarB		{ return new scalarB(absB(this.value)); }
	recip():			scalarB		{ return new scalarB(1n / this.value); }
	divmod(b: scalarB):	number		{ const q = this.value / b.value; this.value -= q * b.value; return Number(q); }
	lt(b: scalarB):		boolean		{ return this.value < b.value; }
	from(n: number | bigint)		{ return new scalarB(BigInt(n)); }

	valueOf():			number		{ return Number(this.value); }
	toString()						{ return this.value.toString(); }
}

export class extentB {
	static fromCentreExtent(centre: bigint, size: bigint) {
		const half = size / 2n;
		return new extentB(centre - half, centre + half);
	}
	static from<U extends Iterable<bigint>>(items: U) {
		let ext;// = new extentT<T>;
		for (const i of items) {
			if (!ext)
				ext = new extentB(i, i);
			else
				ext.add(i);
		}
		return ext;
	}
	constructor(
		public min: bigint,
		public max: bigint
	) {}
	extent() {
		return this.max - this.min;
	}
	centre() {
		return (this.min + this.max) / 2n;
	}
	add(p: bigint) {
		this.min = minB(this.min, p);
		this.max = maxB(this.max, p);
	}
	combine(b: extentB) {
		this.min = minB(this.min, b.min);
		this.max = maxB(this.max, b.max);
	}
	encompasses(b: extentB) {
		return this.min <= b.min && this.max >= b.max;
	}
	overlaps(b: extentB) {
		return this.min <= b.max && this.max >= b.min;
	}
	contains(p: bigint) {
		return this.min <= p && this.max >= p;
	}
	clamp(p: bigint) {
		return minB(maxB(p, this.min), this.max);
	}
}

function highestSetB(x: bigint): number {
	let s = 0;
	let k = 0;

	for (let t = x >> 1024n; t; t >>= BigInt(s)) {
		s = 1024 << k++;
		x = t;
	}

	if (k) {
		while (--k) {
			const b = x >> BigInt(512 << k);
			if (b) {
				s += 512 << k;
				x = b;
			}
		}
	}

	const b = Math.floor(Math.log2(Number(x)));
	return 1n << BigInt(b) <= x ? s + b + 1 : s + b;
}

export function divB(a: bigint, b: bigint): number {
	if (b === 0n)
		return Infinity;
	if (a === 0n)
		return 0;

	const aa = a < 0n ? -a : a;
	const bb = b < 0n ? -b : b;

	// If both fit in safe integer range, do the fast exact conversion
	const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
	if (aa <= MAX_SAFE && bb <= MAX_SAFE)
		return Number(a) / Number(b);

	const sign = (a < 0n) !== (b < 0n) ? -1 : 1;
	const shiftA = Math.max(0, highestSetB(aa) - 53);
	const shiftB = Math.max(0, highestSetB(bb) - 53);

	return sign * Number(aa >> BigInt(shiftA)) / Number(bb >> BigInt(shiftB)) * Math.pow(2, shiftA - shiftB);
}

// Progressive n-th root for bigints
export function rootB(x: bigint, b: number): bigint {
	if (x === 0n || b < 1)
		return 0n;
	if ((b & 1) === 0 && x < 0n)
		return 0n;

	const resultBits = Math.floor(highestSetB(x) / b);

	let k = 16;
	let y = BigInt(Math.floor(Math.pow(Number(x >> BigInt((resultBits - k) * b)), 1 / b)));

	const b1 = BigInt(b - 1);
	const bb = BigInt(b);

	while (k * 2 < resultBits) {
		const xk = x >> BigInt((resultBits - k) * b - k);
		y = (((b1 * y) << BigInt(k)) + xk / (y ** b1)) / bb;
		k <<= 1;
	}

	// Final refinement at full precision
	y = y << BigInt(resultBits - k);
	y = (b1 * y + x / (y ** b1)) / bb;

	while ((y ** bb) > x)
		--y;
	return x < 0n ? -y : y;
}

//-----------------------------------------------------------------------------
// Generics
//-----------------------------------------------------------------------------

export type has<K extends keyof scalarExt<any>> = ops<any> & Pick<scalarExt<any>, K>;
export function has<K extends keyof scalarExt<any>>(f: K) {
	return (x: ops<any>): x is ops<any> & Pick<scalarExt<any>, K> => f in x;
}

export function hasStatic(x: any, f: string) {
	const c = x.constructor;
	if (f in c)
		return c[f] as (...args: any[]) => any;
	return undefined;
}

export function isScalar(x: ops<any>): x is scalar<any> {
	return 'lt' in x;
}
export function isScalarExt(x: ops<any>): x is scalarExt<any> {
	return 'rpow' in x && 'divmod' in x;
}

export function copySignT<T extends scalar<T>>(a: T, b: T) {
	return b.sign() < 0 ? a.abs().neg() : a.abs();
}

export function maxT<T extends has<'lt'>>(a: T, b: T) {
	return a.lt(b) ? b : a;
}
export function minT<T extends has<'lt'>>(a: T, b: T) {
	return a.lt(b) ? a : b;
}
export function maxT2<T extends has<'lt'>>(...b: T[]) {
	return b.slice(1).reduce((max, x) => max.lt(x) ? x : max, b[0]);
}
export function minT2<T extends has<'lt'>>(...b: T[]) {
	return b.slice(1).reduce((min, x) => min.lt(x) ? min : x, b[0]);
}

export function compareT<T extends has<'lt'>>(a: T, b: T): number {
	return a.lt(b) ? -1 : b.lt(a) ? 1 : 0;
}

export interface gcd {
	divmod(b: this):	number | bigint | this;
	sign():				number;
	abs():				this;
	scale(n: number):	this;
}

export function gcdT<T extends gcd>(...values: T[]): T {
	let a: T | undefined;
	for (let b of values) {
		b = b.abs();
		if (a && a.sign()) {
			while (b.sign()) {
				a.divmod(b);
				[a, b] = [b, a];
			}
		} else {
			a = b;
		}
	}
	return a!;
}

export function lcmT<T extends gcd>(...x: T[]) {
	return x.slice(1).reduce((a, b) => b.scale(Number(a.divmod(gcdT(a, b)))), x[0]);
}

type rationalApprox = has<'from'> & has<'divmod'> & has<'recip'> & has<'lt'>;
/*
export function rationalApproxT<T extends rationalApprox>(x: T, maxDen: T, eps?: T): [T, T] {
	const zero	= x.from(0);
	const one	= x.from(1);
	let b	= x.dup();
	let a	= b.divmod(one);
	if (b.sign() < 0) {
		--a;
		b = b.add(one);
	}
	let h1	= x.from(a), h2 = one;
	let k1	= one, k2 = zero;

	while (b.sign() !== 0 && k1.lt(maxDen) && (eps === undefined || eps.lt(b.abs()))) {
		b = b.recip();
		const f = Number(b.divmod(one));
		[h2, h1, k2, k1] = [h1, h1.scale(f).add(h2), k1, k1.scale(f).add(k2)];
	}
	return [h1, k1];
}
*/
export function rationalApproxB<T extends rationalApprox>(x: T, maxDen: bigint, eps?: T): [bigint, bigint] {
	const one	= x.from(1);
	let b	= x.dup();
	let h1	= BigInt(b.divmod(one)), h2 = 1n;
	let k1	= 1n, k2 = 0n;

	if (b.sign() < 0) {
		--h1;
		b = b.add(one);
	}

	while (b.sign() !== 0 && k1 < maxDen && (eps === undefined || eps.lt(b.abs()))) {
		b = b.recip();
		const f = BigInt(b.divmod(one));
		[h2, h1, k2, k1] = [h1, h1 * f + h2, k1, k1 * f + k2];
	}
	return [h1, k1];
}

export class extentT<T extends scalar<T>> {
	static fromCentreExtent<T extends scalar<T>>(centre: T, size: T) {
		const half = size.scale(0.5);
		return new extentT(centre.sub(half), centre.add(half));
	}
	static from<T extends scalar<T>, U extends Iterable<T>>(items: U) {
		let ext;// = new extentT<T>;
		for (const i of items) {
			if (!ext)
				ext = new extentT(i, i);
			else
				ext.add(i);
		}
		return ext;
	}
	constructor(
		public min:T,
		public max:T
	) {}
	extent() {
		return this.max.sub(this.min);
	}
	centre() {
		return this.min.add(this.max).scale(0.5);
	}
	add(p: T) {
		this.min = minT(this.min, p);
		this.max = maxT(this.max, p);
	}
	combine(b: extentT<T>) {
		this.min = minT(this.min, b.min);
		this.max = maxT(this.max, b.max);
	}
	encompasses(b: extentT<T>) {
		return !b.min.lt(this.min) && !this.max.lt(b.max);
	}
	overlaps(b: extentT<T>) {
		return !b.max.lt(this.min) && !this.max.lt(b.min);
	}
	contains(p: T) {
		return !p.lt(this.min) && !this.max.lt(p);
	}
	clamp(p: T) {
		return minT(maxT(p, this.min), this.max);
	}
}
