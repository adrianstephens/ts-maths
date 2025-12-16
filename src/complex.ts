/* eslint-disable no-restricted-syntax */

import { scalar, has } from "./core";
import real from "./real";
import gen from "./gen";

class _complex {
	constructor(public r: number, public i: number) {}

	from(b: number)			{ return complex(b, 0); }
	dup(): 		complex		{ return complex(this.r, this.i); }
	neg(): 		complex		{ return complex(-this.r, -this.i); }
	conj(): 	complex		{ return complex(this.r, -this.i); }
	recip():	complex		{ return this.conj().scale(1 / this.magSq()); }
	magSq():	number		{ return this.r * this.r + this.i * this.i; }
	mag():	 	number		{ return Math.sqrt(this.magSq()); }
	abs():	 	number		{ return this.mag(); }
	arg():		number		{ return Math.atan2(this.i, this.r); }

	scale(b: number): complex	{ return complex(this.r * b, this.i * b); }
	mul(b: complex): complex	{ return complex(this.r * b.r - this.i * b.i, this.r * b.i + this.i * b.r); }
	add(b: complex): complex	{ return complex(this.r + b.r, this.i + b.i); }
	sub(b: complex): complex	{ return complex(this.r - b.r, this.i - b.i); }
	div(b: complex): complex	{ return this.mul(b.conj()).scale(1 / b.magSq()); }
	npow(b: number): complex	{ return complex.fromPolar(Math.pow(this.mag(), b), this.arg() * b); }
	rpow(n: number, d: number): complex	{ return this.npow(n / d); }
	pow(b: complex): complex	{
		if (b.i === 0)
			return this.npow(b.r);
		const r		= this.mag();
		if (r === 0)
			return complex(0, 0);
		const ln_r	= Math.log(r);
		const theta	= this.arg();
		return complex.fromPolar(Math.exp(ln_r * b.r - theta * b.i), ln_r * b.i + theta * b.r);
	}

	eq(b: complex):	boolean		{ return this.r === b.r && this.i === b.i; }
	approx(b: complex, eps = 1e-8):	boolean		{ return real.approx(this.r, b.r, eps) && real.approx(this.i, b.i, eps); }

	sqrt()	{
		const m = this.abs();
		return complex(Math.sqrt(0.5 * (m + this.r)), Math.sqrt(0.5 * (m - this.r)));
	}
	
	toString()	{ return `${this.r} ${this.i >= 0 ? '+' : '-'} ${Math.abs(this.i)}i`; }
}

function fromPolar2(theta: number, r1: number, r2: number)	{
	return complex(Math.cos(theta) * r1, Math.sin(theta) * r2);
}

export const complex = Object.assign(
	function (r: number, i = 0) {
		return new _complex(r, i);
	},
	gen.OperatorsBase(_complex),
	{// statics
	variable(name: string): complex | undefined {
		switch (name) {
			case 'i':			return complex(0, 1);
			case 'pi':			return complex(Math.PI);
			case 'e':			return complex(Math.E);
			case 'infinity':	return complex(Infinity);
			default:			return undefined;
		}
	},
	lt: (a: complex, b: complex) => a.r < b.r || (a.r === b.r && a.i < b.i),
	fromPolar(r: number, t: number)	{ return complex(Math.cos(t) * r, Math.sin(t) * r); },

	zero()				{ return complex(0); },
	re(a: complex)		{ return complex(a.r); },
	im(a: complex)		{ return complex(a.i); },
	sqrt(a: complex|number)	{
		return typeof a === 'number' ? (a < 0 ? complex(0, Math.sqrt(-a)) : complex(Math.sqrt(a))) : a.sqrt();
	},
	ln(a: complex)		{ return complex(Math.log(a.mag()), a.arg()); },
	exp(a: complex)		{ return this.fromPolar(Math.exp(a.r), a.i); },
	sin(a: complex) {
		const e = Math.exp(a.i), re = 1 / e;
		return fromPolar2(a.r + Math.PI / 2, -(e + re) * 0.5, (e - re) * 0.5);
	},
	cos(a: complex) {
		const e = Math.exp(a.i), re = 1 / e;
		return fromPolar2(a.r, (e + re) * 0.5, (re - e) * 0.5);
	},
	tan(a: complex) {
		return complex.sin(a).div(complex.cos(a));
	},
	sinh(a: complex) {
		const e = Math.exp(a.r), re = 1 / e;
		return fromPolar2(a.i, (e - re) * 0.5, (e + re) * 0.5);
	},
	cosh(a: complex) {
		const e = Math.exp(a.r), re = 1 / e;
		return fromPolar2(a.i, (e + re) * 0.5, (e - re) * 0.5);
	},
	tanh(a: complex) {
		return complex.sinh(a).div(complex.cosh(a));
	},

	conjugatePair(c: complex) { return [c, c.conj()]; },
});

complex.prototype	= _complex.prototype;
export type complex	= _complex;
export default complex;

type scalarComplex<T extends scalar<T>> = scalar<T> & has<'sqrt'> & has<'rpow'>;

class _complexT<T extends scalarComplex<T>> {
	constructor(public r: T, public i: T) {}
	dup() 		{ return complexT(this.r.dup(), this.i.dup()); }
	neg() 		{ return complexT(this.r.neg(), this.i.neg()); }
	conj() 		{ return complexT(this.r, this.i.neg()); }
	recip()		{ return this.conj().rscale(this.magSq()); }
	magSq()		{ return this.r.mul(this.r).add(this.i.mul(this.i)); }
	mag()	 	{ return this.magSq().sqrt(); }
	abs()	 	{ return this.mag(); }

	scale(b: number)	{ return complexT(this.r.scale(b), this.i.scale(b)); }
	rscale(b: T)		{ return complexT(this.r.div(b), this.i.div(b)); }
	mul(b: complexT<T>)	{ return complexT(this.r.mul(b.r).sub(this.i.mul(b.i)), this.r.mul(b.i).add(this.i.mul(b.r))); }
	add(b: complexT<T>)	{ return complexT(this.r.add(b.r), this.i.add(b.i)); }
	sub(b: complexT<T>)	{ return complexT(this.r.sub(b.r), this.i.sub(b.i)); }
	div(b: complexT<T>) { return this.mul(b.conj()).rscale(b.magSq()); }

	arg(): number		{ return Math.atan2(Number(this.i), Number(this.r)); }

	sqrt()	{
		const m = this.mag();
		return complexT(m.add(this.r).scale(0.5).sqrt(), m.sub(this.r).scale(0.5).sqrt());
	}

	npow(n: number)		{
		return complexT.fromPolar(this.mag().npow(n), this.arg() * n);
	}
	rpow(n: number, d: number) {
		if (n === 1 && d === 2)
			return this.sqrt();

		return complexT.fromPolar(this.mag().rpow(n, d), this.arg() * n / d);
	}

	toString()	{ return `${this.r} ${this.i.sign() >= 0 ? '+' : '-'} ${this.i.abs()}i`; }
	[Symbol.for("debug.description")]() { return `${this.r} + ${this.i} i`; }
}

export const complexT = Object.assign(
	function<T extends scalarComplex<T>>(r: T, i: T) {
		return new _complexT(r, i);
	},
	{// statics
	zero()				{ return complex(0, 0); },
	fromPolar<T extends scalarComplex<T>>(r: T, t: number)		{ return complexT(r.scale(Math.cos(t)), r.scale(Math.sin(t))); },
	conjugatePair<T extends scalarComplex<T>>(c: complexT<T>)	{ return [c, c.conj()]; },
});

complexT.prototype = _complexT.prototype;
export type complexT<T extends scalarComplex<T>> = _complexT<T>;

