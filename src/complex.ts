/* eslint-disable no-restricted-syntax */

import { OperatorsBase, Operators, scalar, scalarExt, approx, has } from "./core";

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
	approx(b: complex, eps = 1e-8):	boolean		{ return approx(this.r, b.r, eps) && approx(this.i, b.i, eps); }

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
	{// statics
	zero()				{ return complex(0, 0); },
	fromPolar(r: number, t: number)	{ return complex(Math.cos(t) * r, Math.sin(t) * r); },
	sqrt(a: complex|number)	{
		return typeof a === 'number' ? (a < 0 ? complex(0, Math.sqrt(-a)) : complex(Math.sqrt(a), 0)) : a.sqrt();
	},
	ln(a: complex)		{ return complex(Math.log(a.mag()), a.arg()); },
	exp(a: complex)		{ return this.fromPolar(Math.exp(a.r), a.i); },
	//sin(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(s * (e + re) * 0.5,  c * (e - re) * 0.5); },
	//cos(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(c * (e + re) * 0.5, -s * (e - re) * 0.5); },
	//tan(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(s * (e + re),  c * (e - re)).div(complex(c * (e + re), -s * (e - re))); },
	//sinh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex((e - re) * c * 0.5, (e + re) * s * 0.5); },
	//cosh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex((e + re) * c * 0.5, (e + re) * s * 0.5); },
	//tanh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex(s * (e + re),  c * (e - re)).div(complex(c * (e + re), -s * (e - re))); },
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

complex.prototype = _complex.prototype;
export type complex = _complex;
export default complex;

type scalarComplex<T extends scalar<T>> = scalar<T> & has<'sqrt'>;

export class complexT<T extends scalarComplex<T>> {
	constructor(public r: T, public i: T) {}
	static fromPolar<T extends scalarComplex<T>>(r: T, t: number)		{ return new complexT(r.scale(Math.cos(t)), r.scale(Math.sin(t))); }
	static conjugatePair<T extends scalarComplex<T>>(c: complexT<T>	)	{ return [c, c.conj()]; }

	dup() 		{ return new complexT(this.r.dup(), this.i.dup()); }
	neg() 		{ return new complexT(this.r.neg(), this.i.neg()); }
	conj() 		{ return new complexT(this.r, this.i.neg()); }
	recip()		{ return this.conj().rscale(this.magSq()); }
	magSq()		{ return this.r.mul(this.r).add(this.i.mul(this.i)); }
	mag()	 	{ return this.magSq().sqrt(); }
	abs()	 	{ return this.mag(); }

	scale(b: number)	{ return new complexT(this.r.scale(b), this.i.scale(b)); }
	rscale(b: T)		{ return new complexT(this.r.div(b), this.i.div(b)); }
	mul(b: complexT<T>)	{ return new complexT(this.r.mul(b.r).sub(this.i.mul(b.i)), this.r.mul(b.i).add(this.i.mul(b.r))); }
	add(b: complexT<T>)	{ return new complexT(this.r.add(b.r), this.i.add(b.i)); }
	sub(b: complexT<T>)	{ return new complexT(this.r.sub(b.r), this.i.sub(b.i)); }
	div(b: complexT<T>) { return this.mul(b.conj()).rscale(b.magSq()); }

	sqrt()	{
		const m = this.mag();
		return new complexT(m.add(this.r).scale(0.5).sqrt(), m.sub(this.r).scale(0.5).sqrt());
	}

	cbrt() {
		// Algebraic cube root using Cardano-like formula
		// ∛(a+bi) = ∛(r)e^(iθ/3) where r=|a+bi|, θ=arg(a+bi)
		// In algebraic form: u+vi where u²+v² = ∛(a²+b²), u³-3uv²=a
		// Simplified: u = ∛((a+|z|)/2), v = sign(b)∛((|z|-a)/2)
		const mag = this.magSq().sqrt();
		const u = mag.add(this.r).scale(0.5).rpow(1, 3);
		const v_mag = mag.sub(this.r).scale(0.5).rpow(1, 3);
		const v = this.i.sign() < 0 ? v_mag.neg() : v_mag;
		return new complexT(u, v);
	}

	rpow(n: number, d: number) {
		if (n === 1) {
			switch (d) {
				case 2:		return this.sqrt();
				case 3:		return this.cbrt();
			}
		}
		// fallback to polar form for other cases
		const r = this.mag().rpow(n, d);
		return complexT.fromPolar(r, Math.atan2(Number(this.i), Number(this.r)) * n / d);
	}

	toString()	{ return `${this.r} ${this.i.sign() >= 0 ? '+' : '-'} ${this.i.abs()}i`; }
	[Symbol.for("debug.description")]() { return `${this.r} + ${this.i} i`; }
}


export const complexOps: Operators<complex> = {
	...OperatorsBase(_complex),
	variable(name: string): complex | undefined {
		switch (name) {
			case 'i':			return complex(0, 1);
			case 'pi':			return complex(Math.PI);
			case 'e':			return complex(Math.E);
			case 'infinity':	return complex(Infinity);
			default:			return undefined;
		}
	},
	lt: (a: complex, b: complex) => a.eq(b),
};

