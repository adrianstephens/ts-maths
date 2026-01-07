import {Operators, scalar, powerOps} from "./core";
import integer from "./integer";

//-----------------------------------------------------------------------------
// number
//-----------------------------------------------------------------------------

class _real implements scalar<_real>, powerOps<_real> {
	constructor(public value: number) {}
	dup(): 				real	{ return real(this.value); }
	neg(): 				real	{ return real(-this.value); }
	scale(b: number):	real	{ return real(this.value * b); }
	mul(b: real):		real	{ return real(this.value * b.value); }
	div(b: real):		real	{ return real(this.value / b.value); }
	add(b: real):		real	{ return real(this.value + b.value); }
	sub(b: real):		real	{ return real(this.value - b.value); }
	mag():				number 	{ return Math.abs(this.value); }

	from(n: number | bigint)	{ return real(Number(n)); }
	sign():				number	{ return Math.sign(this.value); }
	abs():				real	{ return real(Math.abs(this.value)); }
	recip():			real	{ return real(1 / this.value); }
	divmod(b: real):	number	{ const q = Math.floor(this.value / b.value); this.value -= q * b.value; return q; }
	lt(b: real):		boolean	{ return this.value < b.value; }
	eq(b: real):		boolean	{ return this.value === b.value; }

	sqrt(): 			real	{ return real(Math.sqrt(this.value)); }
	ipow(n: number):	real	{ return real(this.value ** n); }
	npow(n: number):	real	{ return real(this.value ** n); }
	rpow(n: number, d:number)	{ return real(this.value ** (n / d)); }

	valueOf():			number	{ return this.value; }
	toString()					{ return this.value.toString(); }
}

class _extent {
	static fromCentreExtent(centre: number, size: number) {
		const half = size * 0.5;
		return new _extent(centre - half, centre + half);
	}
	static from<U extends Iterable<number>>(items: U) {
		const ext = new _extent;
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
	combine(b: _extent) {
		this.min = Math.min(this.min, b.min);
		this.max = Math.max(this.max, b.max);
	}
	encompasses(b: _extent) {
		return this.min <= b.min && this.max >= b.max;
	}
	overlaps(b: _extent) {
		return this.min <= b.max && this.max >= b.min;
	}
	contains(p: number) {
		return this.min <= p && this.max >= p;
	}
	clamp(p: number) {
		return Math.min(Math.max(p, this.min), this.max);
	}
}

export const real = Object.assign(
	function (value: number) {
		return new _real(value);
	}, {
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
	from(n) 	{ return n; },
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
	} as Operators<number>, {

	is(x: any): x is number {
		return typeof x === 'number';
	},
	
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

	extendedGcd(a: number, b: number) {
		let s0 = 1, s = 0, t0 = 0, t = 1;
		while (b) {
			const q	= Math.floor(a / b);
			[a, b, s0, t0, s, t] = [b, a - b * q, s, t, s0 - s * q, t0 - t * q];
		}
		// a = gcd, and s0 * a + t0 * b = r0
		return { g: a, x: s0, y: t0 };
	},

	lcm(...x: number[]) {
		return x.reduce((a, b) => (a / real.gcd(a, b)) * b, 1);
	},

	denominator(x: number, maxDen: number, eps = Number.EPSILON): integer {
		x = Math.abs(x) % 1;
		if (x <= eps)
			return 1 as integer;

		let k1 = 1, k2 = 0;
		do {
			x = 1 / x;
			const f = Math.floor(x);
			x -= f;
			[k2, k1] = [k1, f * k1 + k2];
		} while (x > eps && k1 < maxDen);
		return k1 as integer;
	},

	commonDenominator(numbers: number[], maxDen = 1000, eps = Number.EPSILON): integer {
		let scale = 1;
		for (const n of numbers) {
			scale *= real.denominator(n * scale, maxDen, eps);
			if (scale > maxDen)
				return 0 as integer;
		}
		return scale as integer;
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
		return [h1 as integer, k1 as integer];
	},

	continuedFraction(x: number, maxTerms = 64, eps?: number): integer[] {
		const out: number[] = [];

		for (let i = 0; i < maxTerms; i++) {
			const a = Math.floor(x);
			out.push(a);
			x -= a;
			if (x === 0 || (eps !== undefined && Math.abs(x) < eps))
				break;
			x = 1 / x;
		}
		return out as integer[];
	},

	extent: _extent
});

real.prototype = _real.prototype;
export type real = _real;
export default real;
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace real {
	export type extent = _extent;
}
