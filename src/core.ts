
export type hasProperty<K extends string, S> = { [P in K]: S };


export function isInstance<T>(x: any, i: (new (...args: any[]) => T) | ((...args: any[]) => T)): x is T {
	return x instanceof i || x.prototype === i.prototype;
}

export function hasStatic(x: any, f: string) {
	const c = x.constructor;
	if (f in c)
		return c[f] as (...args: any[]) => any;
	return undefined;
}
export function arrayOf<U>(arr: any[], g: (x: any) => x is U): arr is U[] {
	return arr.length ? g(arr[0]) : false;
}

// Transform method type to free function type
export type AsFreeFunction<T, K extends keyof T> = T[K] extends (this: infer This, ...args: infer Args) => infer R ? (self: This, ...args: Args) => R : never;

export interface Operators<T> {
	from(n: number): T;
	func(name: string, args: T[]): T | undefined;
	variable(name: string): T | undefined;

	dup(a: T): T;
	neg(a: T): T;
	add(a: T, b: T): T;
	sub(a: T, b: T): T;
	mul(a: T, b: T): T;
	div(a: T, b: T): T;

	ipow(a: T, n: number): T;
	rpow(a: T, n: number, d: number): T;
	pow(a: T, b: T): T;
	eq(a: T, b: T): any;
	lt(a: T, b: T): any;

//	optional
//	scale(b: S):		C;
//	npow(a: T, n: number): T;
//	rpow(a: T, n: number, d: number): T;
}

export function OperatorsBase<T extends ops<T>>(_con: new (...args: any[]) => T) {
	const con		= _con as any;
	const proto		= con.prototype as Record<string, any>;
	
	const r: Partial<Operators<T>> = {
		func: (name: string, args: T[]) => {
			const staticFn = con[name];
			if (typeof staticFn === 'function')
				return staticFn.apply(con, args as any);

			const protoFn = proto[name];
			if (typeof protoFn === 'function')
				return protoFn.apply(args[0], (args as any).slice(1));

			return undefined;
		}
	};

	// guaranteed by the ops constraint
	r.dup = (a: T) => a.dup();
	r.neg = (a: T) => a.neg();
	r.add = (a: T, b: T) =>	a.add(b);
	r.sub = (a: T, b: T) => a.sub(b);
	r.mul = (a: T, b: T) =>	a.mul(b);
	r.div = (a: T, b: T) =>	a.div(b);

	// static or prototype 'from'
	if (typeof con.from === 'function')
		r.from = (n: number) => con.from(n);
	else if (typeof proto.from === 'function')
		r.from = (n: number) => proto.from(n);

	// include prototype-backed binary ops only when present
	if ('eq' in proto)
		r.eq = (a: T, b: T) => (a as any).eq(b);
	if ('lt' in proto)
		r.lt = (a: T, b: T) => (a as any).lt(b);
	if ('pow' in proto)
		r.pow = (a: T, b: T) =>	(a as any).pow(b);

	if ('rpow' in proto)
		r.rpow = (a: T, n: number, d: number) => (a as any).rpow(n, d);

	r.ipow = 'ipow' in proto
		? (a: T, b: number) => (a as any).ipow(b)
		: (a: T, b: number) => Gen.ipow(a, b);


	return r as unknown as Pick<Operators<T>, (keyof Operators<T> & keyof T) | 'func' | 'ipow'>;
}

export function Type<T>(operators: Operators<T>) {
	return class Ops implements ops<Ops> {
		constructor(public value: T) {}
		from(n: number): Ops { return new Ops(operators.from(n)); }
		dup(): Ops			{ return new Ops(operators.dup(this.value)); }
		neg(): Ops			{ return new Ops(operators.neg(this.value)); }
		scale(b: number): Ops { return new Ops(operators.mul(this.value, operators.from(b))); }
		add(b: Ops): Ops	{ return new Ops(operators.add(this.value, b.value)); }
		sub(b: Ops): Ops	{ return new Ops(operators.sub(this.value, b.value)); }
		mul(b: Ops): Ops	{ return new Ops(operators.mul(this.value, b.value)); }
		div(b: Ops): Ops 	{ return new Ops(operators.div(this.value, b.value)); }
		mag():		number	{ return typeof this.value === 'number' ? Math.abs(this.value) : 0; }

		// scalar
		abs():		Ops		{ if (hasFree('abs')(operators)) return new Ops(operators.abs(this.value)); return this; };
		sign():		number	{ const z = operators.from(0); return operators.eq(this.value, z) ? 0 : operators.lt(this.value, z) ? -1 : 1; }
		eq(b: Ops):	boolean	{ return operators.eq(this.value, b.value); }
		lt(b: Ops):	boolean	{ return operators.lt(this.value, b.value); }

		//power
		sqrt(): 			Ops	{ return new Ops(operators.rpow(this.value, 1, 2)); }
		recip():			Ops	{ return new Ops(operators.ipow(this.value, -1)); }
		ipow(n: number):	Ops	{ return new Ops(operators.ipow(this.value, n)); }
		rpow(n: number, d: number):	Ops	{ return new Ops(operators.rpow(this.value, n, d)); }
		npow(n: number):	Ops	{ return new Ops(operators.pow(this.value, operators.from(n))); }

		//ext
//		divmod(b: Ops):		number | bigint;

		toString(): string { return String(this.value); }
		valueOf(): number { return Number(this.value); }	
	};
}


export interface ops<C extends ops<C, S>, S=number> {
	dup():				C;
	neg(): 				C;
	scale(b: S):		C;
	add(b: C): 			C;
	sub(b: C): 			C;
	mul(b: C):			C;
	div(b: C):			C;
	mag():				number | scalarExt<any>;
}

export interface scalar<C extends scalar<C, S>, S=number> extends ops<C, S> {
	from(n: number | bigint):	C;
	abs():				C;
	sign():				number;
	eq(b: C):			boolean;
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
	divmod(this: C, b: C):	number | bigint;
}

export type scalarRational<T extends scalar<T>> = scalar<T> & has<'divmod'> & has<'recip'>;
export type Immutable<T> = Omit<T, 'divmod'>;

export function compare<T extends number|bigint|string>(a: T, b: T): number {
	return a < b ? -1 : a > b ? 1 : 0;
}

export function* lazySlice<T>(arr: T[], start?: number, end?: number): Generator<T> {
	const len = arr.length;
	start = start === undefined	? 0
		:	start < 0			? Math.max(len + start, 0)
		:	Math.min(start, len);

	end =	end === undefined	? len
		:	end < 0				? Math.max(len + start, 0)
		:	Math.min(end, len);

	for (let i = start; i < end; i++)
		yield arr[i];
}

//-----------------------------------------------------------------------------
// integer
//-----------------------------------------------------------------------------

export type integer = number & { __brand: 'Integer' };

export const integer = {
	div(a: integer, b: integer): integer {
		return Math.trunc(a / b) as integer;
	},

	sqrt(n: integer): integer {
		if (n < 0)
			throw new Error("Negative number");
		let x: number = n;
		for (let y = (x + 1) >> 1; y < x; )
			[x, y] = [y, (x + n / x) >> 1];
		return x as integer;
	},

	root(n: integer, r: integer): integer {
		if (n < 0 && (r & 1) === 0)
			throw new Error("Negative number");
		let x: number = n;
		const r1 = r - 1;
		for (let y = (r1 * x + n / Math.pow(x, r1)) / r; y < x; )
			[x, y] = [y, (r1 * x + n / Math.pow(x, r1)) / r];
		return Math.round(x) as integer;
	}
};

export function isInteger(n: number): n is integer {
	return Number.isInteger(n);
}

export function isAlmostInteger(x: number, epsilon = Number.EPSILON) {
	return approx(x, Math.round(x), epsilon);
}

//-----------------------------------------------------------------------------
// number
//-----------------------------------------------------------------------------

export function isNumber(x: any): x is number {
	return typeof x === 'number';
}

export function approx(x: number, y: number, epsilon = Number.EPSILON) {
	return Math.abs(x - y) < epsilon;
}

export const Num: Operators<number> & {
	copySign(a: number, b: number): number;
	gcd(...values: number[]): number;
	lcm(...x: number[]): number;
	denominator(x: number, maxDen: number, eps?: number): number;
	commonDenominator(numbers: number[], maxDen?: number, eps?: number): number;
	rationalApprox(x: number, maxDen: number, eps?: number): [number, number];
	modPow(base: number, exp: number, mod: number): number;
} = {
	from(n) { return n; },
	func(name, args) {
		const fn = (Math as unknown as Record<string, (...args: number[]) => number>)[name];
		return fn ? fn(...args) : undefined;
	},
	variable(name) {
		switch (name) {
			case 'pi':			return Math.PI;
			case 'e':			return Math.E;
			case 'i':			return NaN;
			case 'infinity':	return Infinity;
			default:			return undefined;
		}
	},
	dup(n) 		{ return n; },
	neg(a) 		{ return -a; },
	add(a, b) 	{ return a + b; },
	sub(a, b) 	{ return a - b; },
	mul(a, b) 	{ return a * b; },
	div(a, b) 	{ return a / b; },

	ipow(a, b) 	{ return a ** b; },
	rpow(a, n, d) { return a ** (n / d); },
	pow(a, b) 	{ return Math.pow(a, b); },
	eq(a, b) 	{ return a === b; },
	lt(a, b) 	{ return a < b; },

	copySign(a, b) {
		return b < 0 ? -Math.abs(a) : Math.abs(a);
	},

	gcd(...values) {
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
	},

	lcm(...x) {
		return x.reduce((a, b) => (a / Num.gcd(a, b)) * b, 1);
	},

	denominator(x, maxDen, eps = Number.EPSILON) {
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
	},

	commonDenominator(numbers, maxDen = 1000, eps = Number.EPSILON) {
		let scale = 1;
		for (const n of numbers) {
			scale *= Num.denominator(n * scale, maxDen, eps);
			if (scale > maxDen)
				return 0;
		}
		return scale;
	},

	rationalApprox(x, maxDen, eps = Number.EPSILON) {
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
	},

	modPow(base, exp, mod) {
		let result = 1;
		while (exp) {
			base %= mod;
			if (exp & 1)
				result = (result * base) % mod;
			base *= base;
			exp >>= 1;
		}
		return result;
	}
};

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

	sign():				number		{ return Math.sign(this.value); }
	abs():				scalarN		{ return new scalarN(Math.abs(this.value)); }
	recip():			scalarN		{ return new scalarN(1 / this.value); }
	divmod(b: scalarN):	number		{ const q = Math.floor(this.value / b.value); this.value -= q * b.value; return q; }
	lt(b: scalarN):		boolean		{ return this.value < b.value; }
	eq(b: scalarN):		boolean		{ return this.value === b.value; }
	from(n: number | bigint)		{ return new scalarN(Number(n)); }

	sqrt(): 			scalarN		{ return new scalarN(Math.sqrt(this.value)); }
	npow(n: number):	scalarN		{ return new scalarN(this.value ** n); }
	rpow(n: number, d:number)		{ return new scalarN(this.value ** (n / d)); }
//	pow(n: number):		scalarN		{ return new scalarN(this.value ** n); }

	valueOf():			number		{ return this.value; }
	toString()						{ return this.value.toString(); }
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

export function isBigInt(x: any): x is bigint {
	return typeof x === 'bigint';
}

export const Big: Operators<bigint> & {
	shift(x: bigint, n: bigint): bigint;
	sign(a: bigint): number;
	abs(a: bigint): bigint;
	max(a: bigint, b: bigint): bigint;
	min(a: bigint, b: bigint): bigint;
	divToN(a: bigint, b: bigint): number;
	gcd(...values: bigint[]): bigint;
	lcm(...x: bigint[]): bigint;
	modPow(base: bigint, exp: bigint, mod: bigint): bigint;
	npow(x: bigint, n: number): bigint;
	random(bits: number): bigint;
	root(x: bigint, b: number): bigint;
	log2(n: bigint, bits?: bigint): bigint;
} = {
	func(_name, _args) 	{ return undefined; },
	variable(_name) 	{ return undefined; },
	dup(a) 				{ return a; },
	ipow(a, b) 			{ return a ** BigInt(b); },
	pow(a, b) 			{ return a ** b; },
	from(n) 			{ return BigInt(n); },
	neg(a) 				{ return -a; },
	mul(a, b) 			{ return a * b; },
	div(a, b) 			{ return a / b; },
	add(a, b) 			{ return a + b; },
	sub(a, b) 			{ return a - b; },
	eq(a, b) 			{ return a === b; },
	lt(a, b) 			{ return a < b; },
	
	shift(x, n) 		{ return n > 0 ? x << n : x >> -n; },
	sign(a) 			{ return a === 0n ? 0 : a < 0n ? -1 : 1; },
	abs(a) 				{ return a < 0n ? -a : a; },
	max(a, b) 			{ return a < b ? b : a; },
	min(a, b) 			{ return a < b ? a : b; },

	divToN(a, b) {
		if (b === 0n)
			return Infinity;
		if (a === 0n)
			return 0;

		const aa = Big.abs(a);
		const bb = Big.abs(b);

		// If both fit in safe integer range, do the fast exact conversion
		const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
		if (aa <= MAX_SAFE && bb <= MAX_SAFE)
			return Number(a) / Number(b);

		const sign = (a < 0n) !== (b < 0n) ? -1 : 1;
		const shiftA = Math.max(0, highestSetB(aa) - 52);
		const shiftB = Math.max(0, highestSetB(bb) - 52);

		return sign * Number(aa >> BigInt(shiftA)) / Number(bb >> BigInt(shiftB)) * Math.pow(2, shiftA - shiftB);
	},

	gcd(...values) {
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
	},

	lcm(...x: bigint[]) {
		return x.reduce((a, b) => (a / Big.gcd(a, b)) * b, 1n);
	},

	modPow(base, exp, mod) {
		let result = 1n;
		while (exp) {
			base %= mod;
			if (exp & 1n)
				result = (result * base) % mod;
			base *= base;
			exp >>= 1n;
		}
		return result;
	},

	rpow(x, n, d) {
		return Big.root(x ** BigInt(n), d);
	},

	npow(x, n) {
		const d = Num.denominator(n, 100);
		if (d === 0)
			throw new Error('scalarB.npow: exponent not representable as rational');
		return this.rpow(x, n * d, d);
	},

	random(bits) {
		let seed = (((Date.now() & 0xffffffff) ^ 0x9e3779b9) >>> 0) || 1;

		const rng32 = () => {
			let x = seed >>> 0;
			x ^= (x << 13) >>> 0;
			x ^= x >>> 17;
			x ^= (x << 5) >>> 0;
			return seed = x >>> 0;
		};

		let r = 0n;
		for (let i = bits >> 5; i--; )
			r = (r << 32n) | BigInt(rng32());

		const rem = bits & 31;
		if (rem)
			r = (r << BigInt(rem)) | BigInt(rng32() & ((1 << rem) - 1));
		return r;
	},

	root(x, b) {
		if (b === 1)
			return x;
		if (x === 0n || b < 1)
			return 0n;
		if ((b & 1) === 0 && x < 0n)
			throw new Error('Big.root: negative numbers not allowed with even denominator');

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
	},

	log2(n, bits = 32n) {
		if (n <= 0n)
			throw new Error("n must be positive");

		let log = BigInt(highestSetB(n));
		let x	= this.shift(n, bits - log);  // normalize to fixed-point [1, 2)

		for (let i = 0; i < bits; i++) {
			log <<= 1n;
			x = (x * x) >> bits;
			if (x >= (2n << BigInt(bits))) {
				++log;
				x >>= 1n;
			}
		}

		return log;
	}

};

export class scalarB implements scalarExt<scalarB> {
	constructor(public value: bigint) {}
	dup(): 				scalarB			{ return new scalarB(this.value); }
	neg(): 				scalarB			{ return new scalarB(-this.value); }
	sqrt():				scalarB			{ return new scalarB(Big.root(this.value, 2)); }
	scale(b: number):	scalarB			{ return new scalarB(this.value * BigInt(b)); }
	ipow(n: number):	scalarB			{ return new scalarB(this.value ** BigInt(n)); }
	rpow(n: number, d:number):	scalarB	{ return new scalarB(Big.rpow(this.value, n, d)); }
	npow(n: number):	scalarB			{ return new scalarB(Big.npow(this.value, n)); }
	mul(b: scalarB):	scalarB			{ return new scalarB(this.value * b.value); }
	div(b: scalarB):	scalarB			{ return new scalarB(this.value / b.value); }
	add(b: scalarB):	scalarB			{ return new scalarB(this.value + b.value); }
	sub(b: scalarB):	scalarB			{ return new scalarB(this.value - b.value); }
	mag():				number 			{ return Number(Big.abs(this.value)); }

	sign():				number			{ return Big.sign(this.value); }
	abs():				scalarB			{ return new scalarB(Big.abs(this.value)); }
	recip():			scalarB			{ return new scalarB(1n / this.value); }
	divmod(b: scalarB):	number			{ const q = this.value / b.value; this.value -= q * b.value; return Number(q); }
	lt(b: scalarB):		boolean			{ return this.value < b.value; }
	eq(b: scalarB):		boolean			{ return this.value === b.value; }
	from(n: number | bigint)			{ return new scalarB(BigInt(n)); }

	valueOf():			number			{ return Number(this.value); }
	toString()							{ return this.value.toString(); }
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
	return 1n << BigInt(b) <= x ? s + b : s + b - 1;
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
		this.min = Big.min(this.min, p);
		this.max = Big.max(this.max, p);
	}
	combine(b: extentB) {
		this.min = Big.min(this.min, b.min);
		this.max = Big.max(this.max, b.max);
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
		return Big.min(Big.max(p, this.min), this.max);
	}
}

//-----------------------------------------------------------------------------
// Generics
//-----------------------------------------------------------------------------

export type has0<K extends keyof scalarExt<any>> = Pick<scalarExt<any>, K>;
export function has0<K extends keyof scalarExt<any>>(f: K) {
	return (x: object): x is has0<K> => f in x;
}

export type has<K extends keyof scalarExt<any>> = ops<any> & Pick<scalarExt<any>, K>;
export function has<K extends keyof scalarExt<any>>(f: K) {
	return (x: ops<any>): x is has<K> => f in x;
}

export type hasFree<K extends keyof scalarExt<any>> = {[P in K]: AsFreeFunction<scalarExt<any>, K> };
export function hasFree<K extends keyof scalarExt<any>>(f: K) {
	return (x: object): x is hasFree<K> => f in x;
}

export function isScalar(x: ops<any>): x is scalar<any> {
	return 'lt' in x;
}
export function isScalarRational(x: ops<any>): x is scalarRational<any> {
	return 'divmod' in x;
}
export function isScalarExt(x: ops<any>): x is scalarExt<any> {
	return 'rpow' in x && 'divmod' in x;
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

export type canDenominator = Pick<scalarExt<any>, 'from'|'divmod'|'recip'|'lt'>;

export const Gen = {
	dup<T extends ops<T>>(a: T) 		{ return a.dup(); },
	neg<T extends ops<T>>(a: T) 		{ return a.neg(); },
	add<T extends ops<T>>(a: T, b: T)	{ return a.add(b); },
	sub<T extends ops<T>>(a: T, b: T)	{ return a.sub(b); },
	mul<T extends ops<T>>(a: T, b: T)	{ return a.mul(b); },
	div<T extends ops<T>>(a: T, b: T)	{ return a.div(b); },

	ipow<T extends ops<T>>(base: T, exp: number): T {
		let result = exp & 1 ? base : undefined;
		for (exp >>= 1; exp; exp >>= 1) {
			base = base.mul(base);
			if (exp & 1)
				result = result ? result.mul(base) : base;
		}
		return result!;
	},

	eq<T extends has0<'eq'>>(a: T, b: T) { return a.eq(b); },
	lt<T extends has0<'lt'>>(a: T, b: T) { return a.lt(b); },

	copySign<T extends scalar<T>>(a: T, b: T) {
		return b.sign() < 0 ? a.abs().neg() : a.abs();
	},

	max<T extends has0<'lt'>>(a: T, b: T) {
		return a.lt(b) ? b : a;
	},

	min<T extends has0<'lt'>>(a: T, b: T) {
		return a.lt(b) ? a : b;
	},

	compare<T extends has0<'lt'>>(a: T, b: T): number {
		return a.lt(b) ? -1 : b.lt(a) ? 1 : 0;
	},

	gcd<T extends Pick<scalarExt<any>, 'sign'|'abs'> & hasProperty<'divmod', (b: T)=>any>>(...values: T[]): T {
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
	},

	lcm<T extends Pick<scalarExt<any>, 'divmod'|'sign'|'abs'|'scale'>>(...values: T[]) {
		let a: T | undefined;
		for (const b of values)
			a = a ? b.scale(Number(a.divmod(Gen.gcd(a, b)))) : b;
		return a!;
	},

	denominator<T extends canDenominator>(x: T, maxDen: bigint, eps?: T): bigint {
		const one = x.from(1);
		x.divmod(one);
		if (eps && x.lt(eps))
			return 1n;

		let k1 = 1n, k2 = 0n;
		do {
			x = x.recip();
			const f = BigInt(x.divmod(one));
			[k2, k1] = [k1, f * k1 + k2];
		} while ((!eps || eps.lt(x)) && k1 < maxDen);
		return k1;
	},

	commonDenominator<T extends canDenominator & has0<'scale'>>(numbers: T[], maxDen = 1000n, eps?: T) {
		let scale = 1n;
		for (const n of numbers) {
			scale *= Gen.denominator(n.scale(Number(scale)), maxDen, eps);
			if (scale > maxDen)
				return 0;
		}
		return scale;
	},

	rationalApprox<T extends scalar<T> & canDenominator>(x: Immutable<T>, maxDen: bigint, eps?: T): [bigint, bigint] {
		const den = this.denominator(x.dup(), maxDen, eps);
		return [BigInt(x.dup().divmod(x.from(den))), den];
/*
		const one = x.from(1);
		let b = x.dup();
		let h1 = BigInt(b.divmod(one)), h2 = 1n;
		let k1 = 1n, k2 = 0n;

		if (b.sign() < 0) {
			--h1;
			b = b.add(one);
		}

		while (b.sign() !== 0 && k1 < maxDen && (!eps || eps.lt(b.abs()))) {
			b = b.recip();
			const f = BigInt(b.divmod(one));
			[h2, h1, k2, k1] = [h1, h1 * f + h2, k1, k1 * f + k2];
		}
		return [h1, k1];
		*/
	},

	modPow<T extends has<'divmod'>>(base: T, exp: number, mod: T): T {
		let result: T | undefined;
		while (exp) {
			base.divmod(mod);
			if (exp & 1) {
				result = result ? result.mul(base) : base;
				result!.divmod(mod);
			}
			base = base.mul(base);
			exp >>= 1;
		}
		return result!;
	}
};

export function maxT2<T extends has0<'lt'>>(...b: T[]) {
	return b.slice(1).reduce((max, x) => max.lt(x) ? x : max, b[0]);
}
export function minT2<T extends has0<'lt'>>(...b: T[]) {
	return b.slice(1).reduce((min, x) => min.lt(x) ? min : x, b[0]);
}

export function ipowT2<T extends ops<T>>(base: T, exp: number): T {
	return has('npow')(base) ? base.npow(exp) : Gen.ipow(base, exp);
}

export class extentT<T extends has<'lt'>> {
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
		this.min = Gen.min(this.min, p);
		this.max = Gen.max(this.max, p);
	}
	combine(b: extentT<T>) {
		this.min = Gen.min(this.min, b.min);
		this.max = Gen.max(this.max, b.max);
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
		return Gen.min(Gen.max(p, this.min), this.max);
	}
}
