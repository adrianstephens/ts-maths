import { randomBytes } from 'crypto';

export interface Operators<T> {
	from(n: number): T;
	func(name: string, args: T[]): T | undefined;
	variable(name: string): T | undefined;
	neg(a: T): T;
	pow(a: T, b: T): T;
	mul(a: T, b: T): T;
	div(a: T, b: T): T;
	add(a: T, b: T): T;
	sub(a: T, b: T): T;
}

export abstract class OperatorsBase<T extends ops<T>> implements Operators<T> {
	abstract from(n: number): T;
	abstract func(name: string, args: T[]): T | undefined;
	abstract variable(name: string): T | undefined;
	abstract pow(a: T, b: T): T;
	neg(a: T): T		{ return a.neg(); }
	mul(a: T, b: T): T	{ return a.mul(b); }
	div(a: T, b: T): T	{ return a.div(b); }
	add(a: T, b: T): T	{ return a.add(b); }
	sub(a: T, b: T): T	{ return a.sub(b); }
}

export interface ops<C extends ops<C>> {
	neg(): 				C;
	scale(b: number):	C;
	mul(b: C):			C;
	div(b: C):			C;
	add(b: C): 			C;
	sub(b: C): 			C;
	mag():				number | scalar2<any>;
}

export interface scalar<C extends scalar<C>> extends ops<C> {
	dup():				C;
	sign():				number;
	abs():				C;
	recip():			C;
	divmod(b: C):		number | bigint;
	lt(b: C):			boolean;
	from(n: number | bigint):	C;
	valueOf():			number|bigint;
}

export interface scalar2<C extends scalar2<C>> extends scalar<C> {
	sqrt(): 			C;
	pow(n: number):		C;
}

export function compare<T extends number|bigint|string>(a: T, b: T): number {
	return a < b ? -1 : a > b ? 1 : 0;
}

export function isAlmostInteger(x: number, epsilon = Number.EPSILON) {
	return Math.abs(x - Math.round(x)) < epsilon;
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

export const numberOperators = {
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
};

export function sincos(angle: number) {
	return {s: Math.sin(angle), c: Math.cos(angle)};
}
export function sin2cos(x: number) {
	 return Math.sqrt((1 - x) * (1 + x));
}

export function sign(a: number) {
	return a > 0 ? 1 : a < 0 ? -1 : 0;
}

export function gcd(a: number, b: number): number {
	while (b)
		[a, b] = [b, a % b];
	return a;
}

export function lcm(...x: number[]) {
	return x.reduce((a, b) => {
		const g = gcd(a, b);
		return (a / g) * b;
	}, 1);
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

export class scalarN implements scalar2<scalarN> {
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
	pow(n: number):		scalarN		{ return new scalarN(this.value ** n); }

	valueOf():			number		{ return this.value; }
	toString()						{ return this.value.toString(); }
}

export function isInstance<T>(x: any, i: new (...args: any[]) => T): x is T {
	return x instanceof i;
}
export function isScalar(x: ops<any>): x is scalar<any> {
	return 'sign' in x;
}
export function asScalar(x: number|scalar<any>): scalar<any> {
	if (typeof x === 'number')
		return new scalarN(x);
	return x;
}
export function asScalar2(x: number|scalar2<any>): scalar2<any> {
	if (typeof x === 'number')
		return new scalarN(x);
	return x;
}
export function asScalarT<T extends scalar<T>>(from: T, x: number|T): T {
	if (typeof x === 'number')
		return from.from(x);
	if (x instanceof from.constructor)
		return x;
	return from.from(+x);
}


export class extent1 {
	static fromCentreExtent(centre: number, size: number) {
		const half = size * 0.5;
		return new extent1(centre - half, centre + half);
	}
	static from<U extends Iterable<number>>(items: U) {
		const ext = new extent1;
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

export function gcdB(a: bigint, b: bigint): bigint {
	while (b)
		[a, b] = [b, a % b];
	return a;
}
export function lcmB(...x: bigint[]) {
	return x.reduce((a, b) => {
		const g = gcdB(a, b);
		return (a / g) * b;
	}, 1n);
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

export class scalarB implements scalar<scalarB> {
	constructor(public value: bigint) {}
	dup(): 				scalarB		{ return new scalarB(this.value); }
	neg(): 				scalarB		{ return new scalarB(-this.value); }
	scale(b: number):	scalarB		{ return new scalarB(this.value * BigInt(b)); }
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

export function bigIntDivideToNumber(a: bigint, b: bigint): number {
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

export function maxT<T extends scalar<T>>(a: T, b: T) {
	return a.lt(b) ? b : a;
}
export function minT<T extends scalar<T>>(a: T, b: T) {
	return a.lt(b) ? a : b;
}

export function compareT<T extends scalar<T>>(a: T, b: T): number {
	return a.lt(b) ? -1 : b.lt(a) ? 1 : 0;
}

export function gcdT<T extends scalar<T>>(a: T, b: T): T {
	a = a.abs();
	b = b.abs();

	while (b) {
		a.divmod(b);
		[a, b] = [b, a];
	}
	return a;
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
