import {Operators, scalar, powerOps} from "./core";
import {real} from "./real";

//-----------------------------------------------------------------------------
// bigint
//-----------------------------------------------------------------------------

export function isBigInt(x: any): x is bigint {
	return typeof x === 'bigint';
}

class _big implements scalar<_big, number>, powerOps<_big, number> {
	constructor(public value: bigint) {}
	dup(): 				big			{ return big(this.value); }
	neg(): 				big			{ return big(-this.value); }
	scale(b: number):	big			{ return big(this.value * BigInt(b)); }
	add(b: big):		big			{ return big(this.value + b.value); }
	sub(b: big):		big			{ return big(this.value - b.value); }
	mul(b: big):		big			{ return big(this.value * b.value); }
	div(b: big):		big			{ return big(this.value / b.value); }
	mag():				number 		{ return Number(big.abs(this.value)); }

	from(n: number | bigint)		{ return big(BigInt(n)); }
	ipow(n: number):	big			{ return big(this.value ** BigInt(n)); }
	rpow(n: number, d:number):	big	{ return big(big.rpow(this.value, n, d)); }
	npow(n: number):	big			{ return big(big.npow(this.value, n)); }
	sqrt():				big			{ return big(big.root(this.value, 2)); }

	abs():				big			{ return big(big.abs(this.value)); }
	sign():				number		{ return big.sign(this.value); }
	recip():			big			{ return big(1n / this.value); }
	divmod(b: big):		number		{ const q = this.value / b.value; this.value -= q * b.value; return Number(q); }
	lt(b: big):			boolean		{ return this.value < b.value; }
	eq(b: big):			boolean		{ return this.value === b.value; }

	valueOf():			number		{ return Number(this.value); }
	toString()						{ return this.value.toString(); }
}


class extentB {
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
		this.min = big.min(this.min, p);
		this.max = big.max(this.max, p);
	}
	combine(b: extentB) {
		this.min = big.min(this.min, b.min);
		this.max = big.max(this.max, b.max);
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
		return big.min(big.max(p, this.min), this.max);
	}
}

export const big = Object.assign(
	function (value: bigint) {
		return new _big(value);
	}, {
	func(_name, _args) 	{ return undefined; },
	variable(_name) 	{ return undefined; },
	from(n) 			{ return BigInt(n); },
	dup(a) 				{ return a; },
	neg(a) 				{ return -a; },
	scale(a, b)			{ return a * BigInt(b); },
	add(a, b) 			{ return a + b; },
	sub(a, b) 			{ return a - b; },
	mul(a, b) 			{ return a * b; },
	div(a, b) 			{ return a / b; },
	ipow(a, b) 			{ return a ** BigInt(b); },
	rpow(x, n, d)		{ return big.root(x ** BigInt(n), d); },
	pow(a, b) 			{ return a ** b; },
	eq(a, b) 			{ return a === b; },
	lt(a, b) 			{ return a < b; },
	} as Operators<bigint>, {

	is(x: any): x is bigint		{ return typeof x === 'bigint'; },

	shift(x: bigint, n: bigint)	{ return n > 0 ? x << n : x >> -n; },
	sign(a: bigint) 			{ return a === 0n ? 0 : a < 0n ? -1 : 1; },
	abs(a: bigint) 				{ return a < 0n ? -a : a; },
	max(...values: bigint[])	{ return values.reduce((a, b) => a < b ? b : a);},
	min(...values: bigint[])	{ return values.reduce((a, b) => a < b ? a : b);},

	gcd(...values: bigint[]) {
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
	
	extendedGcd(a: bigint, b: bigint) {
		let s0 = 1n, s = 0n, t0 = 0n, t = 1n;
		while (b) {
			const q	= a / b;
			[a, b, s0, t0, s, t] = [b, a - b * q, s, t, s0 - s * q, t0 - t * q];
		}
		// a = gcd, and s0 * a + t0 * b = r0
		return { g: a, x: s0, y: t0 };
	},

	lcm(...x: bigint[]) {
		return x.reduce((a, b) => (a / big.gcd(a, b)) * b, 1n);
	},

	divToReal(a: bigint, b: bigint) {
		if (b === 0n)
			return Infinity;
		if (a === 0n)
			return 0;

		const aa = big.abs(a);
		const bb = big.abs(b);

		// If both fit in safe integer range, do the fast exact conversion
		const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
		if (aa <= MAX_SAFE && bb <= MAX_SAFE)
			return Number(a) / Number(b);

		const shiftA = Math.max(0, highestSetB(aa) - 52);
		const shiftB = Math.max(0, highestSetB(bb) - 52);
		const result = Number(aa >> BigInt(shiftA)) / Number(bb >> BigInt(shiftB)) * Math.pow(2, shiftA - shiftB);
		return (a < 0n) !== (b < 0n) ? -result : result;
	},
	
	modPow(base: bigint, exp: bigint, mod: bigint) {
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

	npow(x: bigint, n: number) {
		const d = real.denominator(n, 100);
		if (d === 0)
			throw new Error('Big.npow: exponent not representable as rational');
		return big.root(x ** BigInt(n * d), d);
	},

	random(bits: number) {
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

	root(x: bigint, b: number) {
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
			const _xk = x >> BigInt((resultBits - k) * b - k);
			y = (((b1 * y) << BigInt(k)) + _xk / (y ** b1)) / bb;
			k <<= 1;
		}

		// Final refinement at full precision
		y = y << BigInt(resultBits - k);
		y = (b1 * y + x / (y ** b1)) / bb;

		while ((y ** bb) > x)
			--y;
		return x < 0n ? -y : y;
	},

	log2(n: bigint, bits = 32n) {
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
	},
	extent: extentB
});

big.prototype = _big.prototype;
export type big = _big;
export default big;
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace big {
	export type extent = extentB;
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

