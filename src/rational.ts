import { Operators, scalarRational, lazySlice } from "./core";
import Num from './num';
import Big from './big';
import Gen, {OperatorsBase} from './gen';


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
	from(n: number):	rational { return rational.from(n); }
	dup():				rational { return new _rational(this.num, this.den); }

	simplify():	rational	{ return rational.simplified(this.num, this.den); }
	neg(): 		rational	{ return new _rational(-this.num, this.den); }
	recip():	rational	{ return new _rational(this.den, this.num); }
	abs():	 	rational	{ return new _rational(Math.abs(this.num), this.den); }
	frac():		rational	{ return new _rational(this.num % this.den, this.den); }
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
	ipow(b: number):	rational {
		return b < 0
			? rational(this.den ** -b, this.num ** -b)
			: rational(this.num ** b, this.den ** b);
	}
	divmod(b: rational): number	{ const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }
	lt(b: rational):	boolean { return this.compare(b) < 0; }
	eq(b: rational):	boolean { return b instanceof rational && this.compare(b) === 0; }
	compare(b: rational): number { return this.num * b.den - b.num * this.den; }

	toString()				{ return this.den === 1 ? `${this.num}` : `${this.num}/${this.den}`; }
	valueOf():	number		{ return this.num / this.den; }

	is0(): boolean			{ return this.num === 0; }
	is1(): boolean			{ return this.num === this.den; }
}


export const rational: ((num: number, den?: number) => rational) & Operators<rational> & {
	zero(): rational;
	from(n: number, maxDen?: number): rational;
	fromContinuedFraction(terms: number[], maxDen?: number): rational;
	simplified(num: number, den: number): rational;
} = Object.assign(
//export const rational = Object.assign(
	function(num: number, den = 1) {
		const g = Num.gcd(num, den);
		return new _rational(num / g, den / g);
	},
	OperatorsBase(_rational),
	{// statics
	variable(_name: string): undefined			{},
	rpow(a: rational, n: number, d: number)		{ if (d === 1) return a.ipow(n); throw new Error("invalid"); },
	pow(a: rational, b: rational)				{ return this.rpow(a, b.num, b.den); },

	zero()				{ return new _rational(0, 1); },
	from(n: number, maxDen?: number): rational {
		if (Number.isInteger(n))
			return rational(n, 1);
		const [h, k] = Num.rationalApprox(n, maxDen ?? 1e20, 1e-20);
		return new _rational(h, k);
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
	},

	simplified(num: number, den: number): rational {
		const g = Num.gcd(num, den);
		return new _rational(num / g, den / g);
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
	from(n: number|bigint):	rationalB	{ return rationalB.from(n); }
	dup():					rationalB	{ return rationalB(this.num, this.den); }

	simplify(): rationalB {
		const g = Big.gcd(this.num, this.den);
		return rationalB(this.num / g, this.den / g);
	}
	neg(): 		rationalB	{ return rationalB(-this.num, this.den); }
	recip():	rationalB	{ return rationalB(this.den, this.num); }
	abs():	 	rationalB	{ return rationalB(Big.abs(this.num), this.den); }
	frac():		rationalB	{ return rationalB(this.num % this.den, this.den); }
	floor():	bigint		{ return this.num / this.den; }
	sign():		number		{ return this.num === 0n ? 0 : this.num > 0n ? 1 : -1; }
	mag():	 	number		{ return Big.divToNum(Big.abs(this.num), this.den); }

	set(b: rationalB):	rationalB	{ this.num = b.num; this.den = b.den; return this; }
	scale(b: number):	rationalB	{ return this.mul(rationalB.from(b)); }
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
	compare(b: rationalB):	number	{ return Big.sign(this.num * b.den - b.num * this.den); }
	lt(b: rationalB):		boolean	{ return this.compare(b) < 0; }
	eq(b: rationalB):		boolean	{ return this.compare(b) === 0; }

	toString()			{ return this.den === 1n ? `${this.num}` : `${this.num}/${this.den}`; }
	valueOf():	number	{ return Big.divToNum(this.num, this.den); }
}

export const rationalB = Object.assign(
	function(num: bigint, den = 1n) {
		const g = Big.gcd(num, den);
		return new _rationalB(num / g, den / g);
	},
	{// statics
	from(n: number|bigint|scalarRational<any>, maxDen?: bigint): rationalB {
		if (typeof n === 'bigint')
			return rationalB(n, 1n);

		if (typeof n === 'number') {
			if (Number.isInteger(n))
				return rationalB(BigInt(n), 1n);

			const [h, k] = Num.rationalApprox(n, 1e20);
			return rationalB(BigInt(h), BigInt(k));
		}
		const [h, k] = Gen.rationalApprox(n, maxDen ?? 1n << 64n, n.from(1e-8));
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
				const t = Big.min((maxDen - q2) / q1, a - 1n);
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
// generic rationals - why?
//-----------------------------------------------------------------------------
/*
export class rationalT<T extends scalarRational<T>> {
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

//-----------------------------------------------------------------------------
// Numeric
//-----------------------------------------------------------------------------
/*
// Numeric abstraction: allow `number | rational | rationalB` during migration
type Num = number | rational | rationalB;

export class Numeric {
	constructor(public value: Num) {}

	dup()	{ return new Numeric(this.value); }
	sign()	{ return isNumber(this.value) ? Math.sign(this.value) : this.value.sign(); }
	neg()	{ return new Numeric(isNumber(this.value) ? -this.value : this.value.neg()); }
	abs()	{ return new Numeric(isNumber(this.value) ? Math.abs(this.value) : this.value.abs()); }
	recip()	{ return new Numeric(isNumber(this.value) ? rational(1, this.value) : this.value.recip()); }
	scale(b: number)	{ return new Numeric(isNumber(this.value) ? this.value * b : this.value.scale(b)); }

	add(other: Numeric): Numeric {
		const [a, b] = Numeric.coerce(this.value, other.value);
		return new Numeric(isNumber(a) ? a + (b as number) : a.add(b as any));
	}
	sub(other: Numeric): Numeric {
		const [a, b] = Numeric.coerce(this.value, other.value);
		return new Numeric(isNumber(a) ? a - (b as number) : a.sub(b as any));
	}
	mul(other: Numeric): Numeric {
		const [a, b] = Numeric.coerce(this.value, other.value);
		return new Numeric(isNumber(a) ? a * (b as number) : a.mul(b as any));
	}
	div(other: Numeric): Numeric {
		const [a, b] = Numeric.coerce(this.value, other.value);
		return new Numeric(isNumber(a) ? rational(a, b as number) : a.div(b as any));
	}

	ipow(n: number)		{
		return new Numeric(isNumber(this.value) ? Math.pow(this.value, n) : this.value.ipow(n));
	}
	rpow(n: number, d: number)		{
		return new Numeric(Number(this.value) ** (n / d));
	}
	npow(n: number)		{
		return new Numeric(Number(this.value) ** n);
	}
	lt(other: Numeric)	{ return this.valueOf() < other.valueOf(); }
	valueOf() {
		return isNumber(this.value) ? this.value : Number(this.value.valueOf());
	}
	is0() { return this.valueOf() === 0; }
	is1() { return this.valueOf() === 1; }

	denominator() {
		if (isNumber(this.value))
			return Num.denominator(this.value, 1000);
		return Number(this.value.den);
	}
	toString() {
		return this.value.toString();
	}

	static coerce(a: Num, b: Num): [Num, Num] {
		return isNumber(a) ? (isNumber(b) ? [a, b] : [b.from(a), b]) as any
			: isNumber(b) ? [a, a.from(b)] as any
			: a.constructor === b.constructor ? [a, b] as any
			: a instanceof rational ? [rationalB(BigInt(a.num), BigInt(a.den)), b] as any
			: [a, rationalB(BigInt(b.num), BigInt(b.den))] as any;
	}
}
*/