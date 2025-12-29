import { Operators, ops1, hasop, isInstance, lazySlice, divmodto } from "./core";
import real from './real';
import big from './big';
import gen from './gen';
import { FractionOptions, fractionString } from "./string";

export type rationalOps<T extends ops1<T>> = ops1<T> & hasop<'from'|'lt'|'sign'|'abs'|'ipow'> & divmodto<T>;
export type canMakeRationalOps<T extends ops1<T>> = ops1<T> & hasop<'from'|'divmod'|'recip'|'lt'>;
export type canMakeRational = number|bigint|canMakeRationalOps<any>;

export function canMakeRational(x: ops1<any>): x is canMakeRationalOps<any> {
	return 'divmod' in x;
}

//-----------------------------------------------------------------------------
// number rationals
//-----------------------------------------------------------------------------

class _rational {
	constructor(public num: number, public den = 1) {
		if (this.den < 0) {
			this.den = -this.den;
			this.num = -this.num;
		}
	}
	from(n: canMakeRational):	rational	{ return rational.from(n); }

	dup():		rational	{ return new _rational(this.num, this.den); }
	neg(): 		rational	{ return new _rational(-this.num, this.den); }
	recip():	rational	{ return new _rational(this.den, this.num); }
	abs():	 	rational	{ return new _rational(Math.abs(this.num), this.den); }
	frac():		rational	{ return new _rational(this.num % this.den, this.den); }
	floor():	number		{ return Math.floor(this.num / this.den); }
	sign():		number		{ return Math.sign(this.num); }

	set(b: rational):	rational { this.num = b.num; this.den = b.den; return this; }
	scale(b: number):	rational { return rational(this.num * b, this.den); }
	mul(b: rational):	rational { return rational(this.num * b.num, this.den * b.den); }
	add(b: rational):	rational { return rational(this.num * b.den + b.num * this.den, this.den * b.den); }
	sub(b: rational):	rational { return rational(this.num * b.den - b.num * this.den, this.den * b.den); }
	div(b: rational):	rational { return this.mul(b.recip()); }
	mod(b: rational):	rational { return this.div(b).frac().mul(b); }
	
	ipow(b: number):	rational {
		return b < 0
			? rational(this.den ** -b, this.num ** -b)
			: rational(this.num ** b, this.den ** b);
	}
	divmod(b: rational): number	{ const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }
	
	compare(b: rational): number { return this.num * b.den - b.num * this.den; }
	lt(b: rational):	boolean { return this.compare(b) < 0; }
	eq(b: rational):	boolean { return this.compare(b) === 0; }

	isInteger(): boolean	{ return this.den === 1; }
	is0(): boolean			{ return this.num === 0; }
	is1(): boolean			{ return this.num === this.den; }

	toString(opts?: FractionOptions): string { return fractionString(this.num, this.den, opts); }
	valueOf():	number		{ return this.num / this.den; }
}


export const rational: ((num: number, den?: number) => rational) & Omit<Operators<rational>, 'from'> & {
	zero(): rational;
	from(n: canMakeRational, maxDen?: number): rational;
	fromContinuedFraction(terms: number[], maxDen?: number): rational;
} = Object.assign(
	function(num: number, den = 1) {
		const g = real.gcd(num, den);
		return new _rational(num / g, den / g);
	},
	gen.OperatorsBase(_rational),
	{// statics
	variable(_name: string): undefined			{},
	rpow(a: rational, n: number, d: number)		{ if (d === 1) return a.ipow(n); throw new Error("invalid"); },
	pow(a: rational, b: rational)				{ return this.rpow(a, b.num, b.den); },

	zero()				{ return new _rational(0, 1); },
	from(n: canMakeRational, maxDen?: number): rational {
		if (typeof n === 'number') {
			if (Number.isInteger(n))
				return new _rational(n, 1);
			const [h, k] = real.rationalApprox(n, maxDen ?? 1e6, 1e-20);
			return new _rational(h, k);
		}
		const b = rationalB.from(n);
		return new _rational(Number(b.num), Number(b.den));
	},
	
	fromContinuedFraction(terms: number[], maxDen?: number): rational {
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
		return new _rational(p1, q1);
	}
});

rational.prototype = _rational.prototype;
export type rational = _rational;
export default rational;


//-----------------------------------------------------------------------------
// bigint rationals
//-----------------------------------------------------------------------------

class _rationalB {
	constructor(public num: bigint, public den = 1n) {
		if (this.den < 0n) {
			this.den = -this.den;
			this.num = -this.num;
		}
	}
	from(n: canMakeRational):	rationalB	{ return rationalB.from(n); }

	dup():		rationalB	{ return rationalB(this.num, this.den); }
	neg(): 		rationalB	{ return rationalB(-this.num, this.den); }
	recip():	rationalB	{ return rationalB(this.den, this.num); }
	abs():	 	rationalB	{ return rationalB(big.abs(this.num), this.den); }
	frac():		rationalB	{ return rationalB(this.num % this.den, this.den); }
	floor():	bigint		{ return this.num / this.den; }
	sign():		number		{ return this.num === 0n ? 0 : this.num > 0n ? 1 : -1; }

	set(b: rationalB):	rationalB	{ this.num = b.num; this.den = b.den; return this; }
	scale(b: number|bigint):	rationalB	{ return typeof b === 'bigint' ?  rationalB(this.num * b, this.den) : this.mul(rationalB.from(b)); }
	mul(b: rationalB):	rationalB	{ return rationalB(this.num * b.num, this.den * b.den); }
	add(b: rationalB):	rationalB	{ return rationalB(this.num * b.den + b.num * this.den, this.den * b.den); }
	sub(b: rationalB):	rationalB	{ return rationalB(this.num * b.den - b.num * this.den, this.den * b.den); }
	div(b: rationalB):	rationalB	{ return this.mul(b.recip()); }
	mod(b: rationalB):	rationalB	{ return this.div(b).frac().mul(b); }

	ipow(b: number):	rationalB	{
		return b < 0
			? rationalB(this.den ** BigInt(-b), this.num ** BigInt(-b))
			: rationalB(this.num ** BigInt(b), this.den ** BigInt(b));
	}
	divmod(b: rationalB):	bigint	{ const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }

	compare(b: rationalB):	number	{ return big.sign(this.num * b.den - b.num * this.den); }
	lt(b: rationalB):		boolean	{ return this.compare(b) < 0; }
	eq(b: rationalB):		boolean	{ return this.compare(b) === 0; }

	toString()			{ return this.den === 1n ? `${this.num}` : `${this.num}/${this.den}`; }
	valueOf():	number	{ return big.divToReal(this.num, this.den); }
}

export const rationalB: ((num: bigint, den?: bigint) => rationalB) & Omit<Operators<rationalB>, 'from'> & {
	from(n: canMakeRational, maxDen?: bigint): rationalB;
	fromContinuedFraction(terms: (bigint|number)[], maxDen?: bigint): rationalB;
} = Object.assign(
	function(num: bigint, den = 1n) {
		const g = big.gcd(num, den);
		return new _rationalB(num / g, den / g);
	},
	gen.OperatorsBase(_rationalB),
	{// statics
	variable(_name: string): undefined			{},
	rpow(a: rationalB, n: number, d: number)	{ if (d === 1) return a.ipow(n); throw new Error("invalid"); },
	pow(a: rationalB, b: rationalB)				{ return this.rpow(a, Number(b.num), Number(b.den)); },

	from(n: canMakeRational, maxDen?: bigint): rationalB {
		if (typeof n === 'bigint' || (typeof n === 'number' && Number.isInteger(n)))
			return rationalB(BigInt(n), 1n);

		if (typeof n === 'number') {
			const [h, k] = real.rationalApprox(n, 1e20);
			return rationalB(BigInt(h), BigInt(k));
		}
		const [h, k] = gen.rationalApprox(n, maxDen ?? 1n << 64n, n.from(1e-8));
		return rationalB(BigInt(h), BigInt(k));
	},

	fromContinuedFraction(terms: (bigint|number)[], maxDen?: bigint): rationalB {
		let p2 = 1n, q2 = 0n;
		let p1 = BigInt(terms[0]), q1 = 1n;

		for (const i of lazySlice(terms, 1)) {
			const a = BigInt(i);
			[p2, q2, p1, q1] = [p1, q1, a * p1 + p2, a * q1 + q2];
			if (maxDen && q1 > maxDen) {
				// compute maximal t that keeps denom <= maxDen:
				const t = big.min((maxDen - q2) / q1, a - 1n);
				p1 = p2 + t * p1;
				q1 = q2 + t * q1;
				break;
			}
		}
		return rationalB(p1, q1);
	}
});

rationalB.prototype = _rationalB.prototype;
export type rationalB = _rationalB;

//-----------------------------------------------------------------------------
// generic rationals - e.g. polynomials, symbolic.
// Use rational or rationalB for 'scalar' types
//-----------------------------------------------------------------------------

class _rationalT<T extends rationalOps<T>> {
	constructor(public num: T, public den: T) {
		if (this.den.sign() < 0) {
			this.den = this.den.neg();
			this.num = this.num.neg();
		}
	}
	from(n: number|bigint|T|rational|rationalB, maxDen?: bigint): rationalT<T> {
		if (typeof n === 'bigint' || (typeof n === 'number' && Number.isInteger(n)))
			return new _rationalT(this.num.from(n), this.num.from(1));

		if (typeof n === 'number') {
			const [h, k] = real.rationalApprox(n, 1e20);
			return new _rationalT(this.num.from(h), this.num.from(k));
		}
		if (isInstance(n, rational) || isInstance(n, rationalB))
			return new _rationalT(this.num.from(n.num), this.num.from(n.den));

		const [h, k] = gen.rationalApprox(n, maxDen ?? 1n << 64n, n.from(1e-8));
		return new _rationalT(n.from(Number(h)), n.from(Number(k)));
	}

	dup():		rationalT<T>	{ return new _rationalT(this.num, this.den); }
	neg(): 		rationalT<T>	{ return new _rationalT(this.num.neg(), this.den); }
	recip():	rationalT<T>	{ return new _rationalT(this.den, this.num); }
	abs():	 	rationalT<T>	{ return new _rationalT(this.num.abs(), this.den); }
	frac():		rationalT<T>	{ const q = this.dup(); q.num.divmod(this.den); return q; }
	floor()						{ return this.num.dup().divmod(this.den); }
	sign():		number			{ return this.num.sign(); }

	set(b: rationalT<T>)				{ this.num = b.num; this.den = b.den; return this; }
	scale(b: number): rationalT<T>		{ return this.mul(this.from(b)); }
	mul(b: rationalT<T>): rationalT<T>	{ return rationalT(this.num.mul(b.num), this.den.mul(b.den)); }
	add(b: rationalT<T>): rationalT<T>	{ return rationalT(this.num.mul(b.den).add(b.num.mul(this.den)), this.den.mul(b.den)); }
	sub(b: rationalT<T>): rationalT<T>	{ return rationalT(this.num.mul(b.den).sub(b.num.mul(this.den)), this.den.mul(b.den)); }
	div(b: rationalT<T>): rationalT<T>	{ return this.mul(b.recip()); }
	mod(b: rationalT<T>): rationalT<T>	{ return this.div(b).frac().mul(b); }

	ipow(b: number):	rationalT<T>	{
		return b < 0
			? new _rationalT(this.den.ipow(-b), this.num.ipow(-b))
			: new _rationalT(this.num.ipow(b), this.den.ipow(b));
	}
	divmod(b: rationalT<T>)				{
		const q = this.div(b);
		const _r = q.num.divmod(q.den);
		this.set(q.mul(b));
		return _r;
	}
	compare(b: rationalT<T>):	number	{ return gen.compare(this.num.mul(b.den), b.num.mul(this.den)); }
	lt(b: rationalT<T>):		boolean	{ return this.compare(b) < 0; }
	eq(b: rationalT<T>):		boolean	{ return this.compare(b) === 0; }

	toString()	{ return `${this.num} / ${this.den}`; }
	valueOf()	{ return (this.num.div(this.den)).valueOf(); }
}

export const rationalT = Object.assign(
	function<T extends rationalOps<T>>(num: T, den: T) {
		return new _rationalT(num, den);
	},
	{// statics
//	from<T extends scalarRational<T>>(n: T, maxDen?: bigint): rationalT<T> {
//		const [h, k] = gen.rationalApprox(n, maxDen ?? 1n << 64n, n.from(1e-8));
//		return rationalT(n.from(Number(h)), n.from(Number(k)));
//	},
});

rationalT.prototype = _rationalT.prototype;
export type rationalT<T extends rationalOps<T>> = _rationalT<T>;
