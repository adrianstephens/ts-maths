
//-----------------------------------------------------------------------------
// integer
//-----------------------------------------------------------------------------

export type integer = number & { __brand: 'Integer' };

export const integer = {
	from(n: number): integer {
		if (!Number.isInteger(n))
			throw new Error(`Not an integer: ${n}`);
		return n as integer;
	},
	is(n: number): n is integer {
		return Number.isInteger(n);
	},
	almost(x: number, epsilon = Number.EPSILON) {
		return Math.abs(x - Math.round(x)) < epsilon;
	},
	shift(x: integer, n: integer) {
		return (n > 0 ? x << n : x >> -n) as integer;
	},

	add(a: integer, b: integer) {
		return a + b as integer;
	},
	sub(a: integer, b: integer) {
		return a - b as integer;
	},
	mul(a: integer, b: integer) {
		return a * b as integer;
	},
	div(a: integer, b: integer) {
		return Math.trunc(a / b) as integer;
	},
	neg(n: integer) {
		return -n as integer;
	},
	abs(n: integer) {
		return Math.abs(n) as integer;
	},
	sign(n: integer) {
		return Math.sign(n);
	},
	ipow(x: integer, n: integer) {
		return x ** n as integer;
	},
	rpow(x: integer, n: integer, d: integer) {
		return this.root(this.ipow(x, n), d);
	},

	sqrt(n: integer) {
		if (n < 0)
			throw new Error("Negative number");
		let x: number = n;
		for (let y = (x + 1) >> 1; y < x; )
			[x, y] = [y, (x + n / x) >> 1];
		return x as integer;
	},

	root(n: integer, r: integer) {
		if (n < 0 && (r & 1) === 0)
			throw new Error('integer.root: negative numbers not allowed with even denominator');
		let x: number = n;
		const r1 = r - 1;
		for (let y = (r1 * x + n / Math.pow(x, r1)) / r; y < x; )
			[x, y] = [y, (r1 * x + n / Math.pow(x, r1)) / r];
		return Math.round(x) as integer;
	},
	modPow(ibase: integer, iexp: integer, mod: integer) {
		let result = 1;
		for (let exp: number = iexp, base: number = ibase; exp; exp >>= 1) {
			base %= mod;
			if (exp & 1)
				result = (result * base) % mod;
			base *= base;
		}
		return result as integer;
	},
	gcd(...values: integer[]) {
		let a = 0 as integer;
		for (let b of values) {
			if (a) {
				while (b)
					[a, b] = [b, a % b as integer];
			} else {
				a = b;
			}
		}
		return a;
	},

	extendedGcd(a: integer, b: integer) {
		let s0 = 1, s = 0, t0 = 0, t = 1;
		while (b) {
			const q	= this.div(a, b);
			[a, b, s0, t0, s, t] = [b, a - b * q as integer, s, t, s0 - s * q, t0 - t * q];
		}
		// a = gcd, and s0 * a + t0 * b = r0
		return { g: a, x: s0 as integer, y: t0 as integer };
	},

	lcm(...x: integer[]) {
		return x.reduce((a, b) => (a / integer.gcd(a, b)) * b as integer, 1 as integer);
	},
};

export default integer;
