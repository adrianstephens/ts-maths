import { scalar, absB, gcd, gcdB, gcdT } from "./core";

//-----------------------------------------------------------------------------
// number rationals
//-----------------------------------------------------------------------------

function rationalApprox(x: number, maxDen = 1e20, tol = 1e-20): [number, number] {
    let h1 = Math.floor(x), h2 = 1;
    let k1 = 1, k2 = 0;

	for (let b = x - h1; Math.abs(x - h1 / k1) > tol; ) {
        const f = Math.floor(1 / b);
        const h = f * h1 + h2;
        const k = f * k1 + k2;
        if (k > maxDen)
			break;
		[h2, h1, k2, k1] = [h1, h, k1, k];
		b = 1 / b - f;
	}
    return [h1, k1];
}

export class rational {
	static from(n: number): rational {
		if (Number.isInteger(n))
			return new rational(n, 1);
		const [h, k] = rationalApprox(n);
		return new rational(h, k);
	}
	static simplified(num: number, den: number): rational {
		const g = gcd(num, den);
		return new rational(num / g, den / g);
	}

	constructor(public num: number, public den: number) {
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

	lt(b: rational):	boolean { return this.num * b.den < b.num * this.den; }

	toString()				{ return `${this.num} / ${this.den}`; }
	valueOf():	number		{ return this.num / this.den; }
}

//-----------------------------------------------------------------------------
// bigint rationals
//-----------------------------------------------------------------------------

export class rationalB {
	static from(n: number|bigint): rationalB {
		if (typeof n === 'bigint')
			return new rationalB(n, 1n);

		if (Number.isInteger(n))
			return new rationalB(BigInt(n), 1n);

		const [h, k] = rationalApprox(n);
		return new rationalB(BigInt(h), BigInt(k));
	}

	constructor(public num: bigint, public den: bigint) {
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
	mag():	 	number		{ return Number(absB(this.num)) / Number(this.den); }

	set(b: rationalB):	rationalB	{ this.num = b.num; this.den = b.den; return this; }
	scale(b: number):	rationalB	{ return this.mul(rationalB.from(b)); }
	mul(b: rationalB):	rationalB	{ return new rationalB(this.num * b.num, this.den * b.den); }
	add(b: rationalB):	rationalB	{ return new rationalB(this.num * b.den + b.num * this.den, this.den * b.den); }
	sub(b: rationalB):	rationalB	{ return new rationalB(this.num * b.den - b.num * this.den, this.den * b.den); }
	div(b: rationalB):	rationalB	{ return this.mul(b.recip()); }
	mod(b: rationalB):	rationalB	{ return this.div(b).frac().mul(b); }
	divmod(b: rationalB): bigint	{ const q = this.div(b); this.set(q.frac().mul(b)); return q.floor(); }

	toString()	{ return `${this.num} / ${this.den}`; }
}

//-----------------------------------------------------------------------------
// generic rationals
//-----------------------------------------------------------------------------

function rationalApproxT<T extends scalar<T>>(x: T, maxDen: T, tol: T): [T, T] {
	const zero	= x.from(0);
	const one	= x.from(1);
	let b = x;
    let h1 = x.from(b.divmod(one)), h2 = one;
    let k1 = one, k2 = zero;

	while (tol.lt(x.sub(h1.div(k1)).abs())) {
        b = one.div(b);
		const f = Number(b.divmod(one));
		const h = h1.scale(f).add(h2);
        const k = k1.scale(f).add(k2);
        if (k > maxDen)
			break;
		[h2, h1, k2, k1] = [h1, h, k1, k];
	}
    return [h1, k1];
}

export class rationalT<T extends scalar<T>> {
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
			const [h, k] = rationalApprox(n);
			return new rationalT(this.num.from(h), this.num.from(k));
		}
		const [h, k] = rationalApproxT(n, this.num.from(1e20), this.num.from(1e-20));
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

	toString()	{ return `${this.num} / ${this.den}`; }
}
