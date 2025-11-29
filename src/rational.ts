import { scalar, gcd, absB, signB, gcdB, divB, minB, lazySlice, has, rationalApprox, rationalApproxB } from "./core";

type scalar1<T extends scalar<T>> = scalar<T> & has<'divmod'> & has<'recip'>;

//-----------------------------------------------------------------------------
// continued fractions
//-----------------------------------------------------------------------------
export function continuedFraction(x: number, maxTerms = 64, eps?: number): number[] {
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
}

export function continuedFractionT<T extends scalar1<T>>(x: T, maxTerms = 64, eps?: T): (bigint|number)[] {
	const out: (bigint|number)[] = [];
	const one = x.from(1);

	x = x.dup();
	let a = x.divmod(one);
	if (x.sign() < 0) {
		--a;
		x = x.add(one);
	}
	out.push(a);

	for (let i = 1; i < maxTerms && (x.sign() !== 0 && (eps === undefined || eps.lt(x.abs()))); i++) {
		x = x.recip();
		out.push(x.divmod(one));
	}
	return out;
}
/*
function *convergents<T extends scalar<T>>(terms: (bigint|number)[]): Generator<{n: bigint, d: bigint, a: bigint}> {
	let p2 = 0n, p1 = 1n;
	let q2 = 1n, q1 = 0n;
	for (const i of terms) {
		const a = BigInt(i);
		[p2, q2, p1, q1] = [p1, q1, a * p1 + p2, a * q1 + q2];
		yield {n: p1, d: q1, a};
	}
}
*/

//-----------------------------------------------------------------------------
// number rationals
//-----------------------------------------------------------------------------

export class rational implements scalar<rational> {
	static from(n: number, maxDen?: number): rational {
		if (Number.isInteger(n))
			return new rational(n, 1);
		const [h, k] = rationalApprox(n, maxDen ?? 1e20, 1e-20);
		return new rational(h, k);
	}
	
	static fromContinuedFraction(terms: number[], maxDen?: number): rational {
		let p2 = 1, q2 = 0;
		let p1 = terms[0], q1 = 1;

		for (const a of lazySlice(terms, 1)) {
			[p2, q2, p1, q1] = [p1, q1, a * p1 + p2, a * q1 + q2];
			if (maxDen && q1 > maxDen) {
				// compute maximal t that keeps denom <= maxDen:
				const t = Math.min((maxDen - q2) / q1, a - 1);
				p1 = p2 + t * p1;
				q1 = q2 + t * q1;
				break;
			}
		}
		return new rational(p1, q1);
	}

	static simplified(num: number, den: number): rational {
		const g = gcd(num, den);
		return new rational(num / g, den / g);
	}

	constructor(public num: number, public den = 1) {
		if (this.den < 0) {
			this.den = -this.den;
			this.num = -this.num;
		}
	}
	from(n: number):	rational { return rational.from(n); }
	dup():				rational { return new rational(this.num, this.den); }

	simplify():	rational	{ return rational.simplified(this.num, this.den); }
	neg(): 		rational	{ return new rational(-this.num, this.den); }
	recip():	rational	{ return new rational(this.den, this.num); }
	abs():	 	rational	{ return new rational(Math.abs(this.num), this.den); }
	frac():		rational	{ return new rational(this.num % this.den, this.den); }
	floor():	number		{ return Math.floor(this.num / this.den); }
	sign():		number		{ return this.num === 0 ? 0 : this.num > 0 ? 1 : -1; }
	mag():	 	number		{ return Math.abs(this.num / this.den); }

	set(b: rational):	rational { this.num = b.num; this.den = b.den; return this; }
	scale(b: number):	rational { return this.mul(rational.from(b)); }
	mul(b: rational):	rational { return rational.simplified(this.num * b.num, this.den * b.den); }
	add(b: rational):	rational { return rational.simplified(this.num * b.den + b.num * this.den, this.den * b.den); }
	sub(b: rational):	rational { return rational.simplified(this.num * b.den - b.num * this.den, this.den * b.den); }
	div(b: rational):	rational { return this.mul(b.recip()); }
	mod(b: rational):	rational { return this.div(b).frac().mul(b); }

	divmod(b: rational): number	{ const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }
	lt(b: rational):	boolean { return this.compare(b) < 0; }
	eq(b: rational):	boolean { return this.compare(b) === 0; }
	compare(b: rational): number { return this.num * b.den - b.num * this.den; }

	toString()				{ return this.den === 1 ? `${this.num}` : `${this.num} / ${this.den}`; }
	valueOf():	number		{ return this.num / this.den; }
}

//-----------------------------------------------------------------------------
// bigint rationals
//-----------------------------------------------------------------------------

export class rationalB implements scalar<rationalB> {
	static from(n: number|bigint|scalar1<any>, maxDen?: bigint): rationalB {
		if (typeof n === 'bigint')
			return new rationalB(n, 1n);

		if (typeof n === 'number') {
			if (Number.isInteger(n))
				return new rationalB(BigInt(n), 1n);

			const [h, k] = rationalApprox(n, 1e20);
			return new rationalB(BigInt(h), BigInt(k));
		}
		const [h, k] = rationalApproxB(n, maxDen ?? 1n << 64n, n.from(1e-8));
		return new rationalB(BigInt(h), BigInt(k));
	}

	static fromContinuedFraction(terms: (bigint|number)[], maxDen?: bigint): rationalB {
		let p2 = 1n, q2 = 0n;
		let p1 = BigInt(terms[0]), q1 = 1n;

		for (const i of lazySlice(terms, 1)) {
			const a = BigInt(i);
			[p2, q2, p1, q1] = [p1, q1, a * p1 + p2, a * q1 + q2];
			if (maxDen && q1 > maxDen) {
				// compute maximal t that keeps denom <= maxDen:
				const t = minB((maxDen - q2) / q1, a - 1n);
				p1 = p2 + t * p1;
				q1 = q2 + t * q1;
				break;
			}
		}
		return new rationalB(p1, q1);
	}

	constructor(public num: bigint, public den = 1n) {
		if (this.den < 0n) {
			this.den = -this.den;
			this.num = -this.num;
		}
	}
	from(n: number|bigint):	rationalB	{ return rationalB.from(n); }
	dup():					rationalB	{ return new rationalB(this.num, this.den); }

	simplify(): rationalB {
		const g = gcdB(this.num, this.den);
		return new rationalB(this.num / g, this.den / g);
	}
	neg(): 		rationalB	{ return new rationalB(-this.num, this.den); }
	recip():	rationalB	{ return new rationalB(this.den, this.num); }
	abs():	 	rationalB	{ return new rationalB(absB(this.num), this.den); }
	frac():		rationalB	{ return new rationalB(this.num % this.den, this.den); }
	floor():	bigint		{ return this.num / this.den; }
	sign():		number		{ return this.num === 0n ? 0 : this.num > 0n ? 1 : -1; }
	mag():	 	number		{ return divB(absB(this.num), this.den); }

	set(b: rationalB):	rationalB	{ this.num = b.num; this.den = b.den; return this; }
	scale(b: number):	rationalB	{ return this.mul(rationalB.from(b)); }
	mul(b: rationalB):	rationalB	{ return new rationalB(this.num * b.num, this.den * b.den); }
	add(b: rationalB):	rationalB	{ return new rationalB(this.num * b.den + b.num * this.den, this.den * b.den); }
	sub(b: rationalB):	rationalB	{ return new rationalB(this.num * b.den - b.num * this.den, this.den * b.den); }
	div(b: rationalB):	rationalB	{ return this.mul(b.recip()); }
	mod(b: rationalB):	rationalB	{ return this.div(b).frac().mul(b); }

	divmod(b: rationalB):	bigint	{ const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }
	compare(b: rationalB):	number	{ return signB(this.num * b.den - b.num * this.den); }
	lt(b: rationalB):		boolean	{ return this.compare(b) < 0; }
	eq(b: rationalB):		boolean	{ return this.compare(b) === 0; }

	toString()			{ return this.den === 1n ? `${this.num}` : `${this.num} / ${this.den}`; }
	valueOf():	number	{ return divB(this.num, this.den); }
}

//-----------------------------------------------------------------------------
// generic rationals
//-----------------------------------------------------------------------------
/*
export class rationalT<T extends scalar1<T>> {
	constructor(public num: T, public den: T) {
		if (this.den.sign() < 0) {
			this.den = this.den.neg();
			this.num = this.num.neg();
		}
	}
	from(n: number|bigint|T): rationalT<T> {
		if (typeof n === 'bigint' || (typeof n === 'number' && Number.isInteger(n)))
			return new rationalT(this.num.from(n), this.num.from(1));

		if (typeof n === 'number') {
			const [h, k] = rationalApprox(n, 1e20);
			return new rationalT(this.num.from(h), this.num.from(k));
		}
		const [h, k] = rationalApproxT(n, this.num.from(1e20));
		return new rationalT(h, k);
	}

	dup():		rationalT<T>	{ return new rationalT(this.num, this.den); }
	simplify(): rationalT<T>	{
		const g = gcdT(this.num, this.den);
		return new rationalT(this.num.div(g), this.den.div(g));
	}
	neg(): 		rationalT<T>	{ return new rationalT(this.num.neg(), this.den); }
	recip():	rationalT<T>	{ return new rationalT(this.den, this.num); }
	abs():	 	rationalT<T>	{ return new rationalT(this.num.abs(), this.den); }
	frac():		rationalT<T>	{ const q = this.dup(); q.num.divmod(this.den); return q; }
	floor()						{ const q = this.dup(); return q.num.divmod(this.den); }
	sign():		number			{ return this.num.sign(); }
	mag()						{ return this.num.div(this.den).mag(); }

	set(b: rationalT<T>)				{ this.num = b.num; this.den = b.den; return this; }
	scale(b: number): rationalT<T>		{ return this.mul(this.from(b)); }
	mul(b: rationalT<T>): rationalT<T>	{ return new rationalT(this.num.mul(b.num), this.den.mul(b.den)); }
	add(b: rationalT<T>): rationalT<T>	{ return new rationalT(this.num.mul(b.den).add(b.num.mul(this.den)), this.den.mul(b.den)); }
	sub(b: rationalT<T>): rationalT<T>	{ return new rationalT(this.num.mul(b.den).sub(b.num.mul(this.den)), this.den.mul(b.den)); }
	div(b: rationalT<T>): rationalT<T>	{ return this.mul(b.recip()); }
	mod(b: rationalT<T>): rationalT<T>	{ return this.div(b).frac().mul(b); }

	divmod(b: rationalT<T>)				{
		const q = this.div(b);
		const r = q.num.divmod(q.den);
		this.set(q.mul(b));
		return r;
	}
	lt(b: rationalT<T>):		boolean	{ return this.num.mul(b.den).lt(b.num.mul(this.den)); }
	compare(b: rationalT<T>):	number	{ return compareT(this.num.mul(b.den), b.num.mul(this.den)); }

	toString()	{ return `${this.num} / ${this.den}`; }
}
*/