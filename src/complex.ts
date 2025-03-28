
export interface complex {
	r: number;
	i: number;

	neg(): 	complex;
	conj(): complex;
	abs():	number;
	arg():	number;
	add(b: complex): complex;
	sub(b: complex): complex;
	mul(b: complex): complex;
	div(b: complex): complex;
}

function sincos(angle: number) {
	return {s: Math.sin(angle), c: Math.cos(angle)};
}

export const complex = Object.assign(
	function (this: complex, r: number, i: number) {
		if (!this)
			return new complex(r, i);
		this.r = r;
		this.i = i;
	} as {
		(r: number, i: number): complex;		// Callable signature
		new (r: number, i: number): complex;	// Constructor signature
	},
	{// statics
		zero()				{ return complex(0, 0); },
		ln(a: complex)		{ return complex(Math.log(a.abs()), a.arg()); },
		exp(a: complex)		{ const {c, s} = sincos(a.i); const e = Math.exp(a.r); return complex(c * e, s * e); },
		sin(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(s * (e + re) * 0.5,  c * (e - re) * 0.5); },
		cos(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(c * (e + re) * 0.5, -s * (e - re) * 0.5); },
		tan(a: complex)		{ const {c, s} = sincos(a.r); const e = Math.exp(a.i), re = 1 / e; return complex(s * (e + re),  c * (e - re)).div(complex(c * (e + re), -s * (e - re))); },
		sinh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex((e - re) * c * 0.5, (e + re) * s * 0.5); },
		cosh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex((e + re) * c * 0.5, (e + re) * s * 0.5); },
		tanh(a: complex)	{ const {c, s} = sincos(a.i); const e = Math.exp(a.r), re = 1 / e; return complex(s * (e + re),  c * (e - re)).div(complex(c * (e + re), -s * (e - re))); },
	}
);

Object.assign(complex.prototype, {
	neg(this: complex): 		complex		{ return complex(-this.r, -this.i); },
	conj(this: complex): 		complex		{ return complex(this.r, -this.i); },
	abs(this: complex):	 		number		{ return Math.sqrt(this.r* this.r + this.i * this.i); },
	arg(this: complex):			number		{ return Math.atan2(this.i, this.r); },

	mul(this: complex, b: complex): complex	{ return complex(this.r * b.r - this.i * b.i, this.r * b.i + this.i * b.r); },
	add(this: complex, b: complex): complex	{ return complex(this.r + b.r, this.i + b.i); },
	sub(this: complex, b: complex): complex	{ return complex(this.r - b.r, this.i - b.i); },
	div(this: complex, b: complex) {
		const d = b.r * b.r + b.i * b.i;
		return complex(
			(this.r * b.r + this.i * b.i) / d,
			(this.i * b.r - this.r * b.i) / d
		);
	},
	toString(this: complex) {
		return `${this.r} ${this.i >= 0 ? '+' : '-'} ${Math.abs(this.i)}i`;
	}
});
