import {Operators, scalar, scalarExt} from "./core";

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
	return Num.approx(x, Math.round(x), epsilon);
}

//-----------------------------------------------------------------------------
// number
//-----------------------------------------------------------------------------

export function isNumber(x: any): x is number {
	return typeof x === 'number';
}

const NumOps: Operators<number> = {
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
	from(n) { return n; },
	dup(n) 		{ return n; },
	neg(a) 		{ return -a; },
	scale(a, b)	{ return a * b; },
	add(a, b) 	{ return a + b; },
	sub(a, b) 	{ return a - b; },
	mul(a, b) 	{ return a * b; },
	div(a, b) 	{ return a / b; },
	ipow(a, b) 	{ return a ** b; },
	rpow(a, n, d) { return a ** (n / d); },
	pow(a, b) 	{ return Math.pow(a, b); },
	eq(a, b) 	{ return a === b; },
	lt(a, b) 	{ return a < b; },
};

class _Num implements scalarExt<_Num> {
	constructor(public value: number) {}
	dup(): 				Num		{ return Num(this.value); }
	neg(): 				Num		{ return Num(-this.value); }
	scale(b: number):	Num		{ return Num(this.value * b); }
	mul(b: Num):		Num		{ return Num(this.value * b.value); }
	div(b: Num):		Num		{ return Num(this.value / b.value); }
	add(b: Num):		Num		{ return Num(this.value + b.value); }
	sub(b: Num):		Num		{ return Num(this.value - b.value); }
	mag():				number 	{ return Math.abs(this.value); }

	sign():				number	{ return Math.sign(this.value); }
	abs():				Num		{ return Num(Math.abs(this.value)); }
	recip():			Num		{ return Num(1 / this.value); }
	divmod(b: Num):	number		{ const q = Math.floor(this.value / b.value); this.value -= q * b.value; return q; }
	lt(b: Num):			boolean	{ return this.value < b.value; }
	eq(b: Num):			boolean	{ return this.value === b.value; }
	from(n: number | bigint)	{ return Num(Number(n)); }

	sqrt(): 			Num		{ return Num(Math.sqrt(this.value)); }
	ipow(n: number):	Num		{ return Num(this.value ** n); }
	npow(n: number):	Num		{ return Num(this.value ** n); }
	rpow(n: number, d:number)	{ return Num(this.value ** (n / d)); }

	valueOf():			number	{ return this.value; }
	toString()					{ return this.value.toString(); }
}

export const Num = Object.assign(
	function (value: number) {
		return new _Num(value);
	},
	NumOps, {

	approx(x: number, y: number, epsilon = Number.EPSILON) {
		return Math.abs(x - y) < epsilon;
	},

	copySign(a: number, b: number) {
		return b < 0 ? -Math.abs(a) : Math.abs(a);
	},

	gcd(...values: number[]) {
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

	lcm(...x: number[]) {
		return x.reduce((a, b) => (a / Num.gcd(a, b)) * b, 1);
	},

	denominator(x: number, maxDen: number, eps = Number.EPSILON) {
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

	commonDenominator(numbers: number[], maxDen = 1000, eps = Number.EPSILON) {
		let scale = 1;
		for (const n of numbers) {
			scale *= Num.denominator(n * scale, maxDen, eps);
			if (scale > maxDen)
				return 0;
		}
		return scale;
	},

	rationalApprox(x: number, maxDen: number, eps = Number.EPSILON) {
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

	continuedFraction(x: number, maxTerms = 64, eps?: number): number[] {
		const out: number[] = [];

		for (let i = 0; i < maxTerms; i++) {
			const a = Math.floor(x);
			out.push(a);
			x -= a;
			if (x === 0 || (eps !== undefined && Math.abs(x) < eps))
				break;
			x = 1 / x;
		}
		return out;
	},

	modPow(base: number, exp: number, mod: number) {
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
});

Num.prototype = _Num.prototype;
export type Num = _Num;
export default Num;


export function asScalar(x: number|scalar<any>): scalar<any> {
	if (typeof x === 'number')
		return Num(x);
	return x;
}
export function asScalarExt(x: number|scalarExt<any>): scalarExt<any> {
	if (typeof x === 'number')
		return Num(x);
	return x;
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
