
import { scalar2, sincos } from "./core";

class _complex {
	constructor(public r: number, public i: number) {}

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
	
	toString()	{ return `${this.r} ${this.i >= 0 ? '+' : '-'} ${Math.abs(this.i)}i`; }
}

export type complex = _complex;

export const complex = Object.assign(
	function (r: number, i = 0) {
		return new _complex(r, i);
	},
	{// statics
	zero()				{ return complex(0, 0); },
	fromPolar(r: number, t: number)	{ const {c, s} = sincos(t); return complex(c * r, s * r); },
	sqrt(a: complex|number)	{
		if (typeof a === 'number')
			return a < 0 ? complex(0, Math.sqrt(-a)) : complex(Math.sqrt(a), 0);
		const m = a.abs();
		return complex(Math.sqrt(0.5 * (m + a.r)), Math.sqrt(0.5 * (m - a.r)));
	},
	ln(a: complex)		{ return complex(Math.log(a.abs()), a.arg()); },
	exp(a: complex)		{ const {c, s} = sincos(a.i); const e = Math.exp(a.r); return complex(c * e, s * e); },
	sin(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(s * (e + re) * 0.5,  c * (e - re) * 0.5); },
	cos(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(c * (e + re) * 0.5, -s * (e - re) * 0.5); },
	tan(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(s * (e + re),  c * (e - re)).div(complex(c * (e + re), -s * (e - re))); },
	sinh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex((e - re) * c * 0.5, (e + re) * s * 0.5); },
	cosh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex((e + re) * c * 0.5, (e + re) * s * 0.5); },
	tanh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex(s * (e + re),  c * (e - re)).div(complex(c * (e + re), -s * (e - re))); },

	conjugatePair(c: complex) { return [c, c.conj()]; },
});

export class complexT<T extends scalar2<T>> {
	constructor(public r: T, public i: T) {}
	static fromPolar<T extends scalar2<T>>(r: T, t: number)	{ const {c, s} = sincos(t); return new complexT(r.scale(c), r.scale(s)); }

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

	//toString()	{ return `${this.r} ${this.i.gt(from(this.i)(0)) ? '+' : '-'} ${this.i.abs()}i`; }
	toString()	{ return `${this.r} + ${this.i} i`; }
}

export default complex;
