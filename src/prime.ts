/* eslint-disable @typescript-eslint/no-this-alias */

import integer from './integer';
import big from './big';

const smallPrimes	= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] as integer[];
const MR64			= [2, 325, 9375, 28178, 450775, 9780504, 1795265022] as integer[];

export function isProbablePrime(n: integer): boolean {
	if (n < 4n)
		return n >= 2n;

	for (const p of smallPrimes) {
		if (n === p)
			return true;
		if (n % p === 0)
			return false;
	}

	return MillerRabin(n) === 0;
}

// probabilistic primality test with opportunistic factor extraction
function MillerRabin(n: integer): integer {
	// write n-1 as d * 2^s
	const t = n - 1;
	const s = 31 - Math.clz32(t & -t);
	const d = t >> s as integer;;
	
	function tryWitness(a: integer): integer {
		// quick gcd check: if a shares a factor with n, return it
		const factor = integer.gcd(a, n);
		if (factor > 1 && factor < n)
			return factor;

		let x = integer.modPow(a, d, n);
		if (x === 1 || x === n - 1)
			return 0 as integer;

		for (let r = 1; r < s; r++) {
			const x0 = x;
			x = (x * x) % n as integer;
			if (x === 1) {
				// Found nontrivial square root: x is such that x^2 ≡ 1 (mod n) but x ≠ ±1
				const factor = integer.gcd(x0 - 1 as integer, n);
				if (factor > 1 && factor < n)
					return factor;
				// try the other sign as well (rarely useful here, but harmless)
				const factor2 = integer.gcd(x0 + 1 as integer, n);
				if (factor2 > 1 && factor2 < n)
					return factor2;
			}
			if (x === n - 1)
				return 0 as integer;
		}
		return 1 as integer;	//not prime, no factor found
	}

	for (const a of MR64) {
		const f = tryWitness(a);
		if (f)
			return f as integer;
	}

	return 0 as integer;
}

// find a factor of n using Pollard-Rho with Brent's cycle detection
function pollardRhoBrent(n: integer, f: (x: number) => number, y: number): integer {
	const m		= 32;
	let  y0		= y, x = y, g = 1 as integer;

	for (let r = 1; g === 1 && r < 0x10000; r <<= 1) {
		x = y;
		for (let i = 0; i < r; i++)
			y = f(y);
		for (let k = 0; k < r && g === 1; k += m) {
			let q = 1;
			y0 = y;
			for (let i = 0; i < m && i < r - k; i++) {
				y = f(y);
				q = (q * Math.abs(x - y)) % n;
			}
			g = integer.gcd(q as integer, n);
		}
	}

	if (g === n) {
		g = 1 as integer;
		for (let attempts = 0; g === 1 && attempts < 4096; attempts++) {
			y0 = f(y0);
			g = integer.gcd(Math.abs(x - y0) as integer, n);
		}
	}
	return g === 1 ? n : g;
}

// Recursively factor n into prime factors
/*
export function factorInt(n: number): number[] {
	n		= Math.abs(n);
	const out: number[] = [];

	for (const p of smallPrimes) {
		let count = 0;
		while (n % p === 0) {
			n /= p;
			count++;
		}
		if (count)
			out[p] = count;
	}

	if (n === 1)
		return out;

	const stack: number[] = [n];
	while (stack.length) {
		const m = stack.pop()!;
		let factor = MillerRabin(m);
		if (factor) {
			if (factor === 1) {
				for (let attempt = 0; attempt < 10; attempt++) {
					const c	= Math.random() * (m - 1) + 1;
					// f(x) = x^2 + c mod n
					factor	= pollardRhoBrent(m, (x: number) => (x * x + c) % m, Math.random() * m);
					if (factor < m)
						break;
				}
			}
			if (factor < m) {
				stack.push(factor, m / factor);
				continue;
			}
		}

		// failed to factor further; treat as prime (probabilistic)
		out[m] = (out[m] ?? 0) + 1;
	}

	return out;
}
*/
export class factorisation extends Array<integer> {
	constructor(n: integer | integer[]) {
		if (Array.isArray(n)) {
			super(...n);
			return;
		}
		super();
		n	= Math.abs(n) as integer;

		for (const p of smallPrimes) {
			let count = 0;
			while (n % p === 0) {
				n = n / p as integer;
				count++;
			}
			if (count)
				this[p] = count as integer;
		}

		if (n === 1)
			return;

		const stack: integer[] = [n];
		while (stack.length) {
			const m = stack.pop()!;
			let factor = MillerRabin(m);
			if (factor) {
				if (factor === 1) {
					for (let attempt = 0; attempt < 10; attempt++) {
						const c	= Math.random() * (m - 1) + 1;
						// f(x) = x^2 + c mod n
						factor	= pollardRhoBrent(m, (x: number) => (x * x + c) % m, Math.random() * m);
						if (factor < m)
							break;
					}
				}
				if (factor < m) {
					stack.push(factor, m / factor as integer);
					continue;
				}
			}

			// failed to factor further; treat as prime (probabilistic)
			this[m] = (this[m] ?? 0) + 1 as integer;
		}
	}

	dup(): factorisation {
		return new factorisation(this);
	}

	*divisors(limit?: number): Generator<integer> {
		const factors = this;
		const keys = Object.keys(this);
		function* backtrack(i: number, current: number): Generator<integer> {
			if (limit && current > limit)
				return;
			if (i === keys.length) {
				yield current as integer;
				return;
			}
			const p		= Number(keys[i]);
			const exp	= factors[p];
			for (let e = 0; e <= exp; e++, current *= p)
				yield* backtrack(i + 1, current);
		}
		if (keys.length === 0)
			yield 1 as integer;
		else
			yield *backtrack(0, 1);
	}

	selfMul(other: factorisation|integer): factorisation {
		if (typeof other === 'number') {
			for (const i in this) {
				const p = Number(i);
				while (other % p === 0) {
					++this[p];
					other = other / p as integer;
				}
			}
			other = new factorisation(other);
		}
		for (const p in other)
			this[p] = (this[p] ?? 0) + other[p] as integer;
		return this;
	}
	selfDiv(other: factorisation|integer): factorisation {
		if (typeof other === 'number') {
			for (const i in this) {
				const p = Number(i);
				while (other % p === 0) {
					--this[p];
					other = other / p as integer;
				}
			}
			other = new factorisation(other);
		}
		for (const p in other)
			this[p] = (this[p] ?? 0) - other[p] as integer;
		return this;
	}
	mul(other: factorisation|integer): factorisation {
		return this.dup().selfMul(other);
	}
	div(other: factorisation|integer): factorisation {
		return this.dup().selfDiv(other);
	}
	pow(exp: integer): factorisation {
		return new factorisation(this.map(f => f * exp as integer));
	}
}

//------------------------ BigInt version -----------------------

const MR64B = [2n, 325n, 9375n, 28178n, 450775n, 9780504n, 1795265022n];

function makeWitnesses(n: bigint, rounds = 12): Iterable<bigint> {
	const bits = n.toString(2).length;
	return (function*() {
		for (let i = 0; i < rounds; i++)
			yield big.random(bits) % (n - 3n) + 2n;
	})();
}

export function isProbablePrimeB(n: bigint, witnesses: Iterable<bigint>|number = 12): boolean {
	if (n < 4n)
		return n >= 2n;

	for (const i of smallPrimes) {
		const p = BigInt(i);
		if (n === p)
			return true;
		if (n % p === 0n)
			return false;
	}

	if (typeof witnesses === 'number')
		witnesses = n < (1n << 64n) ? MR64B : makeWitnesses(n, witnesses);

	return MillerRabinB(n, witnesses) === 0n;
}

/*
 * Miller-Rabin probabilistic primality test
 * - returns:
 *   - 0n  => probably prime
 *   - 1n  => composite (no factor discovered from MR)
 *   - >1n => composite and this returned bigint is a nontrivial factor of `n`
 */
function MillerRabinB(n: bigint, witnesses: Iterable<bigint>): bigint {
	// write n-1 as d * 2^s
	let d = n - 1n;
	let s = 0;
/*
	for (const m32 = (1n << 32n) - 1n;; d >>= 32n, s += 32) {
		const d1 = Number(d & m32);
		if (d1) {
			const s1 = 31 - Math.clz32(d1 & -d1);
			d >>= BigInt(s1);
			s += s1;
			break;
		}
	}
*/
	while ((d & 1n) === 0n) {
		d >>= 1n;
		s++;
	}

	function tryWitness(a: bigint): bigint {
		// quick gcd check: if a shares a factor with n, return it
		const factor = big.gcd(a, n);
		if (factor > 1n && factor < n)
			return factor;

		let x = big.modPow(a, d, n);
		if (x === 1n || x === n - 1n)
			return 0n;

		for (let r = 1; r < s; r++) {
			const x0 = x;
			x = (x * x) % n;
			if (x === 1n) {
				// Found nontrivial square root: x is such that x^2 ≡ 1 (mod n) but x ≠ ±1
				const factor = big.gcd(x0 - 1n, n);
				if (factor > 1n && factor < n)
					return factor;
				// try the other sign as well (rarely useful here, but harmless)
				const factor2 = big.gcd(x0 + 1n, n);
				if (factor2 > 1n && factor2 < n)
					return factor2;
			}
			if (x === n - 1n)
				return 0n;
		}
		return 1n;	//not prime, no factor found
	}

	for (const a of witnesses) {
		const f = tryWitness(a);
		if (f)
			return f;
	}

	return 0n;
}

// Pollard-Rho single-run attempt: returns a nontrivial factor or n if failed
/*
function pollardRhoOnceB(n: bigint, f: (x: bigint) => bigint): bigint {
	let x = 2n, y = 2n, d = 1n;
	while (d === 1n) {
		x = f(x);
		y = f(f(y));
		d = Big.gcd(Big.abs(x - y), n);
	}
	return d;
}
*/

// find a factor of n using Pollard-Rho with Brent's cycle detection
function pollardRhoBrentB(n: bigint, f: (x: bigint) => bigint, y: bigint): bigint {
	const m		= 32;
	let  y0		= y, x = y, g = 1n;

	for (let r = 1; g === 1n && r < 0x10000; r <<= 1) {
		x = y;
		for (let i = 0; i < r; i++)
			y = f(y);
		for (let k = 0; k < r && g === 1n; k += m) {
			y0 = y;
			let q = 1n;
			for (let i = 0; i < m && i < r - k; i++) {
				y = f(y);
				q = (q * big.abs(x - y)) % n;
			}
			g = big.gcd(q, n);
		}
	}

	if (g === n) {
		g = 1n;
		for (let attempts = 0; g === 1n && attempts < 4096; attempts++) {
			y0 = f(y0);
			g = big.gcd(big.abs(x - y0), n);
		}
	}
	return g === 1n ? n : g;
}
/*
// Recursively factor n into prime factors
export function factorBigInt(n: bigint): Map<bigint, number> {
	const out	= new Map<bigint, number>();
	n			= Big.abs(n);

	for (const i of smallPrimes) {
		const	p		= BigInt(i);
		let		count	= 0;
		while (n % p === 0n) {
			n /= p;
			count++;
		}
		if (count)
			out.set(p, count);
	}

	if (n === 1n)
		return out;

	const stack: bigint[] = [n];
	while (stack.length) {
		const m = stack.pop()!;
		let factor = MillerRabinB(m, m < (1n << 64n) ? MR64B : makeWitnesses(m));
		if (factor) {
			if (factor === 1n) {
				const bits	= m.toString(2).length;
				for (let attempt = 0; attempt < 10; attempt++) {
					const c	= Big.random(bits) % (m - 1n) + 1n;
					//factor	= pollardRhoOnce(m, c);
					// f(x) = x^2 + c mod n
					factor	= pollardRhoBrentB(m, (x: bigint) => (x * x + c) % m, Big.random(bits) % m);
					if (factor < m)
						break;
				}
			}
			if (factor < m) {
				stack.push(factor, m / factor);
				continue;
			}
		}

		// failed to factor further; treat as prime (probabilistic)
		out.set(m, (out.get(m) || 0) + 1);
	}

	return out;
}
*/
export class factorisationB extends Map<bigint, number> {
	constructor(n: bigint| Iterable<[bigint, number]>) {
		if (typeof n !== 'bigint') {
			super(n);
			return;
		}
		super();
		n	= big.abs(n);

		for (const i of smallPrimes) {
			const	p		= BigInt(i);
			let		count	= 0;
			while (n % p === 0n) {
				n /= p;
				count++;
			}
			if (count)
				this.set(p, count);
		}

		if (n === 1n)
			return;

		const stack: bigint[] = [n];
		while (stack.length) {
			const m = stack.pop()!;
			let factor = MillerRabinB(m, m < (1n << 64n) ? MR64B : makeWitnesses(m));
			if (factor) {
				if (factor === 1n) {
					const bits	= m.toString(2).length;
					for (let attempt = 0; attempt < 10; attempt++) {
						const c	= big.random(bits) % (m - 1n) + 1n;
						//factor	= pollardRhoOnce(m, c);
						// f(x) = x^2 + c mod n
						factor	= pollardRhoBrentB(m, (x: bigint) => (x * x + c) % m, big.random(bits) % m);
						if (factor < m)
							break;
					}
				}
				if (factor < m) {
					stack.push(factor, m / factor);
					continue;
				}
			}

			// failed to factor further; treat as prime (probabilistic)
			this.set(m, (this.get(m) || 0) + 1);
		}
	}
	dup(): factorisationB {
		return new factorisationB(this.entries());
	}
	*divisors(limit?: bigint): Generator<bigint> {
		const entries = Array.from(this.entries());

		function* backtrack(i: number, current: bigint): Generator<bigint> {
			if (limit && current > limit)
				return;
			if (i === entries.length) {
				yield current;
				return;
			}
			const [p, exp] = entries[i];
			for (let e = 0; e <= exp; e++, current *= p)
				yield* backtrack(i + 1, current);
		}
		if (entries.length === 0)
			yield 1n;
		else
			yield *backtrack(0, 1n);
	}
	
	selfMul(other: factorisationB|bigint): factorisationB {
		if (typeof other === 'bigint') {
			for (const p of this.keys()) {
				while (other % p === 0n) {
					this.set(p, this.get(p)! + 1);
					other /= p;
				}
			}
			other = new factorisationB(other);
		}
		for (const [p, exp] of other.entries())
			this.set(p, (this.get(p) ?? 0) + exp);
		return this;
	}
	selfDiv(other: factorisationB|bigint): factorisationB {
		if (typeof other === 'bigint') {
			for (const p of this.keys()) {
				while (other % p === 0n) {
					this.set(p, this.get(p)! - 1);
					other /= p;
				}
			}
			other = new factorisationB(other);
		}
		for (const [p, exp] of other.entries())
			this.set(p, (this.get(p) ?? 0) - exp);
		return this;
	}
	mul(other: factorisationB|bigint): factorisationB {
		return this.dup().selfMul(other);
	}
	div(other: factorisationB|bigint): factorisationB {
		return this.dup().selfDiv(other);
	}
	pow(exp: number): factorisationB {
		return new factorisationB(Array.from(this.entries(), ([p, e]) => [p, e * exp]));
	}
}