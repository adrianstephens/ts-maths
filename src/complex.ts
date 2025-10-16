
/* eslint-disable no-restricted-syntax */

import { sincos } from "./core";

class _complex {
	constructor(public r: number, public i: number) {}

	neg(): 		complex		{ return complex(-this.r, -this.i); }
	conj(): 	complex		{ return complex(this.r, -this.i); }
	abs():	 	number		{ return Math.sqrt(this.r * this.r + this.i * this.i); }
	arg():		number		{ return Math.atan2(this.i, this.r); }

	scale(b: number): complex	{ return complex(this.r * b, this.i * b); }
	mul(b: complex): complex	{ return complex(this.r * b.r - this.i * b.i, this.r * b.i + this.i * b.r); }
	add(b: complex): complex	{ return complex(this.r + b.r, this.i + b.i); }
	sub(b: complex): complex	{ return complex(this.r - b.r, this.i - b.i); }
	div(b: complex) {
		const d = b.r * b.r + b.i * b.i;
		return complex(
			(this.r * b.r + this.i * b.i) / d,
			(this.i * b.r - this.r * b.i) / d
		);
	}
	toString()	{ return `${this.r} ${this.i >= 0 ? '+' : '-'} ${Math.abs(this.i)}i`; }
}

export type complex = _complex;

export const complex = Object.assign(
	function (r: number, i = 0) {
		return new _complex(r, i);
	},
	{// statics
	zero()				{ return complex(0, 0); },
	from(r: number, t: number)	{ const {c, s} = sincos(t); return complex(c * r, s * r); },
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

export default complex;
