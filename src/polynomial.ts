import {ops, scalar, scalar2, sign, extent1, maxT, extentT, isScalar, asScalar2, asScalarT} from './core';
import complex, { complexT } from './complex';

const sqrt3	= Math.sqrt(3);
const defaultEpsilon = 1e-9;

//-----------------------------------------------------------------------------
//	helpers
//-----------------------------------------------------------------------------

function copySign(a: number, b: number) {
	return b < 0 ? -Math.abs(a) : Math.abs(a);
}

function insertSorted<T>(arr: T[], value: T, less: (a: T, b: T) => boolean = (a, b) => a < b): T[] {
	const i = arr.findIndex(x => less(value, x));
	if (i === -1)
		arr.push(value);
	else
   		arr.splice(i, 0, value);
	return arr;
}

function arrayN(arr: any[]): arr is number[] {
	return typeof arr[0] === 'number';
}
function arrayO<T>(arr: any[]): arr is T[] {
	return typeof arr[0] === 'object';
}

//-----------------------------------------------------------------------------
//	Polynomial with real coefficients
//-----------------------------------------------------------------------------

export class polynomial {
	constructor(public c: number[]) {}
	degree()	{ return this.c.length - 1; }
	dup()		{ return new polynomial(this.c.slice()); }

	evaluate(t: complex): complex;
	evaluate(t: number): number;
	evaluate(t: number[]): number[];
	evaluate(t: number|number[]|complex) {
		let i = this.c.length;
		if (typeof t === 'number') {
			let r = this.c[--i];
			while (i--)
				r = r * t + this.c[i];
			return r;
		}
		if (Array.isArray(t)) {
			const r = new Array<number>(t.length).fill(this.c[--i]);
			while (i--)
				r.forEach((x, j) => r[j] = x * t[j] + this.c[i]);
			return r;
		}
		let r = complex(this.c[--i]);
		while (i--)
			r = r.mul(t).add(complex(this.c[i]));
		return r;
	}
	deriv() {
		return new polynomial(this.c.slice(1).map((v, i) => v * (i + 1)));
	}
	add(b: polynomial) {
		return new polynomial(Array.from({length: Math.max(this.c.length, b.c.length)}, (_, i) => (this.c[i] || 0) + (b.c[i] || 0)));
	}
	sub(b: polynomial) {
		return new polynomial(Array.from({length: Math.max(this.c.length, b.c.length)}, (_, i) => (this.c[i] || 0) - (b.c[i] || 0)));
	}
	mul(b: number|polynomial) {
		if (typeof b === 'number')
			return new polynomial(this.c.map(a => a * b));

		const r = new Array<number>(this.c.length + b.c.length - 1).fill(0);
		for (let i = 0; i < this.c.length; i++)
			for (let j = 0; j < b.c.length; j++)
				r[i + j] += this.c[i] * b.c[j];
		return new polynomial(r);
	}
	div(b: number|polynomial) {
		if (typeof b === 'number')
			return this.mul(1 / b);
		return this.dup().divmod(b);
	}

	divmod(b: polynomial) {
		const a = this.c;
		const bdeg = b.c.length - 1;
		const alen = a.length;
		const bt = b.c[bdeg];
		const q = new Array<number>(Math.max(0, alen - bdeg));
		for (let i = q.length; i--;) {
			const d = a[i + bdeg] / bt;
			q[i] = d;
			for (let j = 0; j < bdeg; j++)
				a[i + j] -= d * b.c[j];
		}

		let rlen = bdeg;
		while (rlen > 0 && a[rlen - 1] === 0)
			rlen--;
		this.c.length = rlen;
		return new polynomial(q);
	}

	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && Math.abs(this.c[i]) < epsilon)
			i--;
		const f = 1 / this.c[i];
		return new polynomialN(this.c.slice(0, i).map(v => v * f));
	}
	realRoots(epsilon = defaultEpsilon): number[] {
		return this.normalise().realRoots(epsilon);
	}
	allRoots(): (complex|number)[] {
		return this.normalise().allRoots();
	}
	refine_roots(x: number[], count = 1) {
		const	dpoly	= this.deriv();
		const	ddpoly	= dpoly.deriv();
	
		while (count--)
			x = x.map(x => {
				const	f	= this.evaluate(x);
				const	f1	= dpoly.evaluate(x);
				const	f2	= ddpoly.evaluate(x);
				return x - (f * f1 * 2) / (f1 * f1 * 2 - f * f2);
			});
		return x;
	}
}

//-----------------------------------------------------------------------------
//	Normalised Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

export class polynomialN {
	constructor(public c: number[]) {}
	degree()	{ return this.c.length; }
	dup()		{ return new polynomialN(this.c.slice()); }

	evaluate(t: complex): complex;
	evaluate(t: number): number;
	evaluate(t: number[]): number[];
	evaluate(t: number|number[]|complex) {
		let i = this.c.length;

		if (typeof t === 'number') {
			let r = t + this.c[--i];
			while (i--)
				r = r * t + this.c[i];
			return r;
		}
		if (Array.isArray(t)) {
			const lead = this.c[--i];
			const r = t.map(x => x + lead);
			while (i--)
				r.forEach((x, j) => r[j] = x * t[j] + this.c[i]);
			return r;
		}
//		if (t instanceof complex) {
			let r = t.add(complex(this.c[--i]));
			while (i--)
				r = r.mul(t).add(complex(this.c[i]));
			return r;
//		}
	}
	deriv() {
		return new polynomial([...this.c.slice(1).map((v, i) => v * (i + 1)), this.c.length]);
	}
	normalised_deriv() {
		const f = 1 / this.c.length;
		return new polynomialN(this.c.slice(1).map((v, i) => v * (i + 1) * f));
	}
	mul(b: polynomialN) {
		const r = new Array<number>(this.c.length + b.c.length).fill(0);
		for (let i = 0; i < this.c.length; i++)
			for (let j = 0; j < b.c.length; j++)
				r[i + j] += this.c[i] * b.c[j];

		for (let i = 0; i < this.c.length; i++)
			r[i + b.c.length] += this.c[i];

		for (let i = 0; i < b.c.length; i++)
			r[i + this.c.length] += b.c[i];

		return new polynomialN(r);
	}

	divmod(b: polynomialN) {
		const a = this.c;
		const alen = a.length;
		const blen = b.c.length;
		const q = new Array<number>(Math.max(0, alen - blen + 1));
		for (let i = q.length; i--;) {
			const d = a[i + blen];
			q[i] = d;
			for (let j = 0; j < blen; j++)
				a[i + j] -= d * b.c[j];
		}

		let rlen = blen;
		while (rlen > 0 && a[rlen - 1] === 0)
			rlen--;
		this.c.length = rlen;
		return new polynomial(q);
	}
	realRoots(epsilon = defaultEpsilon): number[] {
		return normPolyRealRoots(this.c, epsilon);
	}
	allRoots(epsilon = defaultEpsilon): (complex|number)[] {
		return normPolyComplexRoots(this.c, epsilon);
	}
}

//-----------------------------------------------------------------------------
//	Polynomial with generic coefficients supporting ops interface
//-----------------------------------------------------------------------------

type inferRealRoots<T> = T extends scalar<any> ? T[] : never;
type inferComplexRoots<T> = T extends scalar2<infer R> ? complexT<R>[] : never;

function lessThan<T extends scalar<T>>(a: number | bigint | T, b: number): boolean {
	if (typeof a === 'number')
		return a < b;
	else if (typeof a === 'bigint')
		return a < BigInt(b);
	// Use the scalar's lt method to compare a < b.  Avoid calling a non-existent
	// `gt` method which may have been accepted due to loosened `any` typing.
	return a.lt(a.from(b));
}

export class polynomialT<T extends ops<T>> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length - 1; }
	dup(): polynomialT<T>		{ return new polynomialT<T>(this.c.slice()); }

	evaluate(t: number|T): T;
	evaluate(t: number[]|T[]): T[];
	evaluate(t: number|T|number[]|T[]) {
		let i = this.c.length;
		if (Array.isArray(t)) {
			const r = new Array<T>(t.length).fill(this.c[--i]);
			if (typeof t[0] === 'number') {
				while (i--)
					r.forEach((x, j) => r[j] = x.scale(t[j] as number).add(this.c[i]));
				return r;
			} else {
				while (i--)
					r.forEach((x, j) => r[j] = x.mul(t[j] as T).add(this.c[i]));
				return r;
			}
		} else {
			let r = this.c[--i];
			if (typeof t === 'number') {
				while (i--)
					r = r.scale(t).add(this.c[i]);
			} else {
				while (i--)
					r = r.mul(t).add(this.c[i]);
			}
			return r;
		}
	}
	deriv() {
		return new polynomialT(this.c.slice(1).map((v, i) => v.scale(i + 1)));
	}
	add(b: polynomialT<T>|T) {
		if (b instanceof polynomialT)
			return new polynomialT(Array.from({length: Math.max(this.c.length, b.c.length)}, (_, i) =>
				this.c[i] ? (b.c[i] ? this.c[i].add(b.c[i]) : this.c[i]) : b.c[i].neg()
			));

		return new polynomialT(this.c.map(c => c.add(b)));
	}
	sub(b: polynomialT<T>|T) {
		if (b instanceof polynomialT)
			return new polynomialT(Array.from({length: Math.max(this.c.length, b.c.length)}, (_, i) =>
				this.c[i] ? (b.c[i] ? this.c[i].sub(b.c[i]) : this.c[i]) : b.c[i].neg()
			));
		return new polynomialT(this.c.map(c => c.sub(b)));
	}
	mul(b: number|polynomialT<T>|T) {
		if (b instanceof polynomialT) {
			const zero = this.c[0].scale(0);
			return new polynomialT(Array.from({ length: this.c.length + b.c.length - 1 }, (_, k) => {
				let sum = zero;
				for (let i = Math.max(0, k - b.c.length + 1); i <= Math.min(this.c.length - 1, k); i++)
					sum = sum.add(this.c[i].mul(b.c[k - i]));
				return sum;
			}));
		}

		if (typeof b === 'number')
			return new polynomialT(this.c.map(a => a.scale(b)));

		return new polynomialT(this.c.map(a => a.mul(b)));
	}
	
	div(b: number|polynomialT<T>|T) {
		if (b instanceof polynomialT)
			return this.dup().divmod(b);

		if (typeof b === 'number')
			return this.mul(1 / b);

		return new polynomialT(this.c.map(a => a.div(b)));
	}

	divmod(b: polynomialT<T>) {
		const a = this.c;
		const bdeg = b.c.length - 1;
		const alen = a.length;
		const bt = b.c[bdeg];
		const q = new Array<T>(Math.max(0, alen - bdeg));
		for (let i = q.length; i--;) {
			const d = a[i + bdeg].div(bt);
			q[i] = d;
			for (let j = 0; j < bdeg; j++)
				a[i + j] = a[i + j].sub(d.mul(b.c[j]));
		}

		let rlen = bdeg;
		while (rlen > 0 && lessThan(a[rlen - 1].mag(), defaultEpsilon))
			rlen--;
		this.c.length = rlen;
		return new polynomialT(q);
	}
	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && lessThan(this.c[i].mag(), epsilon))
			i--;
		const f = this.c[i];
		return new polynomialNT<T>(this.c.slice(0, i).map(v => v.div(f)));
	}

	realRoots(epsilon = defaultEpsilon): inferRealRoots<T>  {
		return this.normalise().realRoots(epsilon);
	}
	allRoots(epsilon = defaultEpsilon): inferComplexRoots<T>  {
		return this.normalise().allRoots(epsilon);
	}

	map(func: (c: T, i: number) => number) {
		return new polynomial(this.c.map((c, i) => func(c, i)));
	}
}

//-----------------------------------------------------------------------------
//	Normalised General Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------


export class polynomialNT<T extends ops<T>> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length; }
	dup(): polynomialNT<T>		{ return new polynomialNT<T>(this.c.slice()); }

	evaluate(t: T): T;
	evaluate(t: T[]): T[];
	evaluate(t: T|T[]) {
		let i = this.c.length;
		if (Array.isArray(t)) {
			const lead = this.c[--i];
			const r = t.map(x => x.add(lead));
			while (i--)
				r.forEach((x, j) => r[j] = x.mul(t[j] as T).add(this.c[i]));
			return r;
		} else {
			let r = t.add(this.c[--i]);
			while (i--)
				r = r.mul(t).add(this.c[i]);
			return r;
		}
	}

	//deriv<U extends scalar<any>>(this: polynomialNT<U>) : polynomialT<U> {
	deriv() : polynomialT<T> {
		if (isScalar(this.c[0]))
			return new polynomialT([...this.c.slice(1).map((v, i) => v.scale(i + 1)), this.c[0].from(this.c.length)]);
		return undefined as never;
	}
	normalised_deriv() {
		const f = 1 / this.c.length;
		return new polynomialNT(this.c.slice(1).map((v, i) => v.scale((i + 1) * f)));
	}
	mul(b: polynomialNT<T>) {
		const zero = this.c[0].scale(0);
		return new polynomialNT(Array.from({ length: this.c.length + b.c.length }, (_, k) => {
			let sum = zero;
			for (let i = Math.max(0, k - b.c.length); i <= Math.min(this.c.length, k); i++)
				sum = sum.add(this.c[i].mul(b.c[k - i]));
			return sum;
		}));
	}
	divmod(b: polynomialNT<T>) {
		const blen	= b.c.length;
		const dc = new Array<T>(this.c.length - blen);

		for (let i = dc.length; --i;) {
			const d = this.c[i + blen];
			for (let j = 0; j < b.c.length; j++)
				this.c[i - j] = this.c[i-j].sub(d.mul(b.c[j]));
			dc[i] = d;
		}
		this.c.length = b.c.length - 1;
		return new polynomialT(dc);
	}

	realRoots(epsilon = defaultEpsilon): inferRealRoots<T> {
		if (isScalar(this.c[0]))
			return sturmRealRootsT(this, this.c[0].from(epsilon)) as inferRealRoots<T>;
		return undefined as never;
	}

	allRoots(epsilon = defaultEpsilon): inferComplexRoots<T> {
		if (isScalar(this.c[0]))
			return aberthT(this, this.c[0].from(epsilon)) as inferComplexRoots<T>;
		return undefined as never;
	}
}

//-----------------------------------------------------------------------------
//	lagrange root bounds
//-----------------------------------------------------------------------------

function sumTop2(a: number[]) {
	if (a.length < 2)
		return a.length === 1 ? a[0] : 0;
	let max1 = -Infinity, max2 = -Infinity;
	for (const v of a) {
		if (v > max1) {
			max2 = max1;
			max1 = v;
		} else if (v > max2) {
			max2 = v;
		}
	}
	return max1 + max2;
}

function lagrangeImproved(k: polynomialN|polynomialNT<complex>): number {
	const N = k.c.length;
	if (k instanceof polynomialN)
		return sumTop2(k.c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i))));
	return sumTop2(k.c.map((c, i) => Math.pow(c.abs(), 1 / (N - i))));
}

function realBound(k: polynomialN): extent1 {
	const N = k.c.length;
	const terms = k.c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i)));

	return new extent1(
		-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (k.c[i] > 0))),
		sumTop2(terms.filter((x, i) => k.c[i] < 0))
	);
}

function sumTop2T<T extends scalar<T>>(zero: T, a: T[]) {
	if (a.length < 2)
		return a.length === 1 ? a[0] : zero;
	let max1 = a[0], max2 = a[0];
	for (const v of a) {
		if (max1.lt(v)) {
			max2 = max1;
			max1 = v;
		} else if (max2.lt(v)) {
			max2 = v;
		}
	}
	return max1.add(max2);
}

function lagrangeImprovedT<T extends scalar2<T>>(k: polynomialNT<T>|polynomialNT<complexT<T>>) {
	const N = k.c.length;
	const zero = k.c[0] instanceof complexT ? k.c[0].r.from(0) : k.c[0].from(0);
	return sumTop2T(zero, k.c.map((c, i) => c.abs().pow(1 / (N - i))));
}

function realBoundT<T extends scalar<T>>(k: polynomialNT<T>) {
	const N = k.c.length;
	const terms = k.c.map((c, i) => asScalar2(c.mag()).pow(1 / (N - i)));

	if (arrayN(terms)) {
		return new extent1(
			-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))),
			sumTop2(terms.filter((x, i) => k.c[i].sign() < 0))
		);
	} else if (arrayO<scalar<any>>(terms)) {
		const zero = terms[0].from(0);

		return new extentT(
			sumTop2T(zero, terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))).neg(),
			sumTop2T(zero, terms.filter((x, i) => k.c[i].sign() < 0))
		);
	} else {
		throw new Error("realBoundT: unsupported coefficient type");
	}
}

//-----------------------------------------------------------------------------
//	root refinement
//-----------------------------------------------------------------------------

function bisectRoot(p: polynomial|polynomialN, min: number, max: number, tol: number, maxIter = 10) {
	let fmin = p.evaluate(min);

	for (let i = 0; i < maxIter && max - min > tol; i++) {
		const mid	= 0.5 * (min + max);
		const fmid	= p.evaluate(mid);
		if (Math.abs(fmid) < tol)
			break;

		// choose side where sign changes
		if (fmin * fmid <= 0) {
			max		= mid;
		} else {
			min		= mid;
			fmin	= fmid;
		}
	}
	return 0.5 * (min + max);
}

function halley(p: polynomial|polynomialN, x: number, maxIter = 10) {
	const d1 = p.deriv();
	const d2 = d1.deriv();
	for (let i = 0; i < maxIter; i++) {
		const f = p.evaluate(x);
		const f1 = d1.evaluate(x);
		const f2 = d2.evaluate(x);
		const denom = 2 * f1 * f1 - f * f2;
		if (denom === 0)
			break;
		const dx = (2 * f * f1) / denom;
		x -= dx;
		if (Math.abs(dx) < 1e-14 * Math.max(1, Math.abs(x)))
			break;
	}
	return x;
}

function _halley(poly: polynomial|polynomialN, dpoly: polynomial, ddpoly: polynomial, x: number[]) {
	const	f	= poly.evaluate(x);
	const	f1	= dpoly.evaluate(x);
	const	f2	= ddpoly.evaluate(x);
	return x.map((x, i) => x - (f[i] * f1[i] * 2) / (f1[i] * f1[i] * 2 - f[i] * f2[i]));
}

function _norm_halley(poly: polynomialN, dpoly: polynomialN, ddpoly: polynomialN, x: number[]) {
	const 	N	= poly.c.length;
	const	f	= poly.evaluate(x);
	const	f1	= dpoly.evaluate(x);// * N;
	const	f2	= ddpoly.evaluate(x);// * N * (N - 1);
	return x.map((x, i) => x - (f[i] * f1[i] * 2) / (f1[i] * f1[i] * N * 2 - f[i] * f2[i] * (N - 1)));
}

//	adjust intervals to guarantee convergence of halley's method by using roots of further derivatives
function adjust_roots(poly: polynomial|polynomialN, dpoly: polynomial|polynomialN, extents: extent1[]) {
	const	rootsd	= dpoly.realRoots();
	for (const ext of extents) {
		for (const r of rootsd) {
			if (ext.contains(r)) {
				if (dpoly.evaluate(r) > 0)
					ext.max = r;
				else
					ext.min = r;
			}
		}
	}
	return rootsd.length;
}

function bisectRootT<T extends scalar<T>>(p: polynomialT<T>|polynomialNT<T>, min: T, max: T, tol: T, maxIter = 10) {
	let fmin = p.evaluate(min);

	for (let i = 0; i < maxIter && tol.lt(max.sub(min)); i++) {
		const mid	= min.add(max).scale(0.5);
		const fmid	= p.evaluate(mid);
		if (fmid.abs().lt(tol))
			break;

		// choose side where sign changes
		if (fmin.sign() != fmid.sign()) {
			max		= mid;
		} else {
			min		= mid;
			fmin	= fmid;
		}
	}
	return min.add(max).scale(0.5);
}

function halleyT<T extends scalar<T>>(p: polynomialT<T>|polynomialNT<T>, x: T, maxIter = 10) {
	const d1 = p.deriv();
	const d2 = d1.deriv();
	for (let i = 0; i < maxIter; i++) {
		const f = p.evaluate(x);
		const f1 = d1.evaluate(x);
		const f2 = d2.evaluate(x);
		const denom = f1.mul(f1).scale(2).sub(f.mul(f2));
		if (denom.sign() === 0)
			break;
		const dx = f.mul(f1).scale(2).div(denom);
		x = x.sub(dx);
		if (dx.abs().lt(maxT(x.abs().scale(1e-14), x.from(1))))
			break;
	}
	return x;
}

//-----------------------------------------------------------------------------
//	get real roots
//-----------------------------------------------------------------------------

function normPolyRealRoots(k: number[], epsilon: number): number[] {
	let zeros = 0;
	while (k[zeros] === 0)
		++zeros;

	return zeros > 0
		? insertSorted(checkEven(k.slice(zeros)), 0)
		: checkEven(k);

	function checkEven(k: number[]) {
		if (k[1] === 0 && k.length % 2 == 0) {
			let odds = false;
			for (let j = 3; j < k.length; j += 2) {
				odds = k[j] !== 0;
				if (odds)
					break;
			}
			if (!odds) {
				const r = normal(k.filter((v, i) => i % 2 == 0)).filter(r => r > 0).map(r => Math.sqrt(r));
				return [...r.map(r => -r).reverse(), ...r];
			}
		}
		return normal(k);
	}

	function normal(k: number[]) {
		switch (k.length) {
			case 1:
				return [-k[0]];

			case 2: {
				const e = k[1] * 0.5;
				const d = e * e - k[0];
				if (d > 0) {
					const r = Math.sqrt(d);
					return [-r - e, r - e];
				} else if (d == 0) {
					return [-e];
				} else {
					return [];//[-e, Math.sqrt(-d)];
				}
			}
			case 3: {
				const	e	= k[2] / 3;
				const	f	= e * e - k[1] / 3;
				const	g	= (e * k[1] - k[0]) / 2 - e * e * e;
				const	h	= g * g - f * f * f;
			
				if (h < 0) {
					//3 real roots
					const	angle	= Math.atan2(copySign(Math.sqrt(-h), g), g) / 3;
					const	c		= Math.cos(angle), s = Math.sin(angle);
					const	rf		= Math.sqrt(f);
					return	[
						2 * c * rf - e,
						(-c - sqrt3 * s) * rf - e,
						(-c + sqrt3 * s) * rf - e
					];
			
				} else if (h > 0) {
					//1 real root, 2 imaginary (y + iz) & (y - iz)
					const	rh = Math.sqrt(h);
					const	x = Math.pow(g + rh, 1 / 3);
					const	y = Math.pow(g - rh, 1 / 3);
					return [x + y - e, /*-0.5 * (x + y) - e,	sqrt3 / 2 * (x - y)*/];
				} else {
					//3 real and equal
					return [Math.pow(-k[0], 1 / 3)];
				}
			}
			case 4: {
				const a		= k[3];
				const b		= k[2];
				const c		= k[1];
				const d		= k[0];

				//  substitute x = y - A/4 to eliminate cubic term:
				//	x^4 + px^2 + qx + r = 0

				const a2	= a * a;
				const p		= b - 3 * a2 / 8;
				const q		= a2 * a / 8 - a * b / 2 + c;
				const t		= -3 * a2 * a2 / 256 + a2 * b / 16 - a * c / 4 + d;

				if (Math.abs(t) < epsilon) {
					// no absolute term: y(y^3 + py + q) = 0
					return [0, ...normPolyRealRoots([q, p, 0], epsilon).map(r => r - a / 4)];
				}

				// solve the resolvent cubic ...
				const r = normPolyRealRoots([-q * q / 8, p * p / 4 - t, p], epsilon);
				const m = Math.max(...r);
				const v = Math.sqrt(m * 2);
				const u = q / Math.sqrt(m * 8);

				return [...normPolyRealRoots([m + p / 2 - u, v], epsilon), ...normPolyRealRoots([m + p / 2 + u, -v], epsilon)].map(i => i - a / 4);
				//if (ra.length === 0 && rb.length === 0)
				//	return [];
				//return halley(new polynomialN(k), [...ra, ...rb].map(i => i - a / 4));
			}

			case 5: {
				const poly		= new polynomialN(k);
				const dpoly		= poly.normalised_deriv();
				const d2poly	= dpoly.normalised_deriv();
				const r1		= dpoly.realRoots();
				const bounds	= realBound(poly);

				let r: number[];

				if (r1.length === 4) {
					r = [];
					const r2	= [bounds.min, ...r1, bounds.max];
					const vals 	= poly.evaluate(r2);
					for (let i = 1; i < 6; i++) {
						if (vals[i] * vals[i - 1] <= 0)
							r.push((r2[i] + r2[i - 1]) / 2);
					}


				} else {
					const ranges = [
						bounds,
					];
					const vals 	= poly.evaluate(r1);
					if (r1.length === 2) {
						//roots1.xy	= sort(get(roots1.xy));
						if (vals[0] < 0) {
							ranges[0].min = r1[1];
						} else if (vals[1] > 0) {
							ranges[0].max = r1[0];
						} else {
							ranges[0].max = r1[0];
							ranges.push(new extent1(r1[1], bounds.max));
						}
					}

					//further subdivide intervals to guarantee convergence of halley's method by using roots of further derivatives
					const num_roots = adjust_roots(poly, d2poly, ranges);
					if (num_roots != 3)
						adjust_roots(poly, d2poly.normalised_deriv(), ranges);

					r = ranges.map(i => i.centre());
				}

				//8 halley iterations
				for (let i = 0; i < 8; i++)
					r = _norm_halley(poly, dpoly, d2poly, r);

				return r;
			}

			default:
				// Use a real-root isolator (Sturm sequences + bisection) to find real roots
				// This avoids relying on complex-arithmetic fallbacks and gives stricter
				// guarantees about real roots and multiplicities.
				return sturmRealRoots(new polynomialN(k), epsilon);
		}
	}
}

//-----------------------------------------------------------------------------
//  Real-root isolation using Sturm sequences + bisection + polishing
//  Returns sorted real roots (with multiplicity collapsed as repeated entries)
//-----------------------------------------------------------------------------

function sturmRealRoots(pN: polynomialN, epsilon: number) {
	const n = pN.degree();
	if (n === 0)
		return [];

	// Build Sturm sequence
	const seq: polynomial[] = [];
	let p0 = new polynomial([...pN.c, 1]);
	let p1 = pN.deriv();
	seq.push(p0.dup(), p1.dup());

	while (p1.degree() >= 0) {
		p0.divmod(p1);
		if (p0.degree() < 0)
			break;

		// push -r
		for (let i = 0; i < p0.c.length; i++)
			p0.c[i] = -p0.c[i];

		seq.push(p0.dup());
		[p0, p1] = [p1, p0];
	}

	function SignChanges(x: number) {
		let prev = 0;
		let changes = 0;
		for (const p of seq) {
			let s = sign(p.evaluate(x));
			if (s === 0) {
				// if polynomial evaluates to zero, use sign of nearby point
				const eps = Math.max(1e-12, Math.abs(x) * 1e-12);
				s = sign(p.evaluate(x + eps));
				if (s === 0)
					continue;
			}
			if (prev !== 0 && s !== prev)
				changes++;
			prev = s;
		}
		return changes;
	}

	function countRootsInInterval(a: number, b: number) {
		const va = SignChanges(a);
		const vb = SignChanges(b);
		return Math.abs(va - vb);
	}

//	const pN = p.normalise();
	const bounds = realBound(pN);
	let lower = bounds.min;
	let upper = bounds.max;
	let count = countRootsInInterval(lower, upper);

	const stack: [number, number, number][] = [];
	const roots: number[] = [];

	for (;;) {
		while (count > 1) {
			// split interval
			const mid = 0.5 * (lower + upper);
			const lowerCount = countRootsInInterval(lower, mid);
			if (lowerCount < count)
				stack.push([mid, upper, count - lowerCount]);
			upper = mid;
			count = lowerCount;
		}

		if (count === 1)
			roots.push(halley(pN, bisectRoot(pN, lower, upper, epsilon)));
				
		if (stack.length == 0)
			break;
		[lower, upper, count] = stack.pop()!;
	}

	return roots;
}


function sturmRealRootsT<T extends scalar<T>>(pN: polynomialNT<T>, epsilon: T) {
	const n = pN.degree();
	if (n === 0)
		return [];

	// Build Sturm sequence
	const seq: polynomialT<T>[] = [];
	let p0 = new polynomialT<T>([...pN.c, pN.c[0].from(1)]);
	let p1 = pN.deriv();
	seq.push(p0.dup(), p1.dup());

	while (p1.degree() >= 0) {
		p0.divmod(p1);
		if (p0.degree() < 0)
			break;

		// push -r
		for (let i = 0; i < p0.c.length; i++)
			p0.c[i] = p0.c[i].neg();

		seq.push(p0.dup());
		[p0, p1] = [p1, p0];
	}

	function SignChanges(x: T) {
		let prev = 0;
		let changes = 0;
		for (const p of seq) {
			let s = p.evaluate(x).sign();
			if (s === 0) {
				// if polynomial evaluates to zero, use sign of nearby point
				const eps = x.abs().scale(1e-12);//Math.max(1e-12, Math.abs(x) * 1e-12);
				s = p.evaluate(x.add(eps)).sign();
				if (s === 0)
					continue;
			}
			if (prev !== 0 && s !== prev)
				changes++;
			prev = s;
		}
		return changes;
	}

	function countRootsInInterval(a: T, b: T) {
		const va = SignChanges(a);
		const vb = SignChanges(b);
		return Math.abs(va - vb);
	}

	//const pN = p.normalise();
	const bounds = realBoundT(pN);
	let lower = asScalarT(epsilon, bounds.min);
	let upper = asScalarT(epsilon, bounds.max);
	let count = countRootsInInterval(lower, upper);

	const stack: [scalar<any>, scalar<any>, number][] = [];
	const roots: T[] = [];

	for (;;) {
		while (count > 1) {
			// split interval
			const mid = lower.add(upper).scale(0.5);
			const lowerCount = countRootsInInterval(lower, mid);
			if (lowerCount < count)
				stack.push([mid, upper, count - lowerCount]);
			upper = mid;
			count = lowerCount;
		}

		if (count === 1)
			roots.push(halleyT(pN, bisectRootT(pN, lower as T, upper as T, epsilon)));
				
		if (stack.length == 0)
			break;
		[lower, upper, count] = stack.pop()!;
	}

	return roots;
}


//-----------------------------------------------------------------------------
//	get real and complex roots
//-----------------------------------------------------------------------------

function normPolyComplexRoots(k: number[], epsilon: number): (complex|number)[] {
	let zeros = 0;
	while (k[zeros] === 0)
		++zeros;

	return zeros > 0
		? insertSorted(checkEven(k.slice(zeros)), 0)
		: checkEven(k);

	function checkEven(k: number[]) {
		if (k[1] === 0 && k.length % 2 == 0) {
			let odds = false;
			for (let j = 3; j < k.length; j += 2) {
				odds = k[j] !== 0;
				if (odds)
					break;
			}
			if (!odds) {
				const r = normal(k.filter((_, i) => i % 2 == 0)).map(r => complex.sqrt(r));
				return [...r.map(r => r.neg()).reverse(), ...r];
			}
		}
		return normal(k);
	}

	function normal(k: number[]) {
		switch (k.length) {
			case 1:
				return [-k[0]];

			case 2: {
				const e = k[1] * 0.5;
				const d = e * e - k[0];
				if (d > 0) {
					const r = Math.sqrt(d);
					return [-r - e, r - e];
				}
				return d == 0 ? [-e] : complex.conjugatePair(complex(-e, Math.sqrt(-d)));
			}
			case 3: {
				const	e	= k[2] / 3;
				const	f	= e * e - k[1] / 3;
				const	g	= (e * k[1] - k[0]) / 2 - e * e * e;
				const	h	= g * g - f * f * f;
			
				if (h < 0) {
					//3 real roots
					const	angle	= Math.atan2(copySign(Math.sqrt(-h), g), g) / 3;
					const	c		= Math.cos(angle), s = Math.sin(angle);
					const	rf		= Math.sqrt(f);
					return	[
						2 * c * rf - e,
						(-c - sqrt3 * s) * rf - e,
						(-c + sqrt3 * s) * rf - e
					];
			
				} else if (h > 0) {
					//1 real root, 2 imaginary (y + iz) & (y - iz)
					const	rh = Math.sqrt(h);
					const	x = Math.pow(g + rh, 1 / 3);
					const	y = Math.pow(g - rh, 1 / 3);
					return [x + y - e, ...complex.conjugatePair(complex(-0.5 * (x + y) - e,	sqrt3 / 2 * (x - y)))];
				} else {
					//3 real and equal
					return [Math.pow(-k[0], 1 / 3)];
				}
			}
			case 4: {
				const a		= k[3];
				const b		= k[2];
				const c		= k[1];
				const d		= k[0];

				//  substitute x = y - A/4 to eliminate cubic term:
				//	x^4 + px^2 + qx + r = 0

				const a2	= a * a;
				const p		= b - 3 * a2 / 8;
				const q		= a2 * a / 8 - a * b / 2 + c;
				const t		= -3 * a2 * a2 / 256 + a2 * b / 16 - a * c / 4 + d;

				let roots3;
				if (Math.abs(t) < epsilon) {
					// no absolute term: y(y^3 + py + q) = 0
					roots3 = [0, ...normPolyComplexRoots([q, p, 0], epsilon)];
				} else {
						// solve the resolvent cubic ...
						const r = normPolyRealRoots([-q * q / 8, p * p / 4 - t, p], epsilon);
						const m = Math.max(...r);
						const v = Math.sqrt(m * 2);
						const u = q / Math.sqrt(m * 8);
						roots3 = [...normPolyComplexRoots([m + p / 2 - u, v], epsilon), ...normPolyComplexRoots([m + p / 2 + u, -v], epsilon)];
				}
				return roots3.map(r => typeof r === 'number' ? r - a / 4 : complex(r.r - a / 4, r.i));
			}
			
			default:
				return aberth(new polynomialN(k));
		}
	}	
}

//-----------------------------------------------------------------------------
//	Aberth method to find all roots of polynomial simultaneously
//-----------------------------------------------------------------------------

export function aberth(poly: polynomialN|polynomialNT<complex>, tolerance = 1e-6, maxIterations = 100) {
	const radius = lagrangeImproved(poly);
	const n		= poly.degree();
	const roots	= Array.from({length: n}, (_, i) => complex.fromPolar(radius, 2 * Math.PI * i / n));

	const dpoly = poly.deriv();

	for (let iter = 0; iter < maxIterations; iter++) {
		let maxCorrection = 0;
		for (let i = 0; i < roots.length; i++) {
			const zi	= roots[i];
			const p_zi	= poly.evaluate(zi);
			const dp_zi	= dpoly.evaluate(zi);
			
			let sum		= complex.zero();
			for (let j = 0; j < roots.length; j++) {
				if (i !== j)
					sum = sum.add(complex(1).div(zi.sub(roots[j])));
			}
			const correction = p_zi.div((dp_zi.sub(p_zi.mul(sum))));
			roots[i] = roots[i].sub(correction);
			maxCorrection = Math.max(maxCorrection, Math.abs(correction.r) + Math.abs(correction.i));
		}
		if (maxCorrection < tolerance)
			break;
	}
	return roots;
}

function isComplexPolyN<T extends scalar2<T>>(poly: polynomialNT<T>|polynomialNT<complexT<T>>): poly is polynomialNT<complexT<T>> {
	return poly.c[0] instanceof complexT;
}

export function aberthT<T extends scalar2<T>>(poly: polynomialNT<T>|polynomialNT<complexT<T>>, tolerance?: T, maxIterations = 100) {
	let from: (v: number) => T;
	let evaluate: (p: any, t: complexT<T>) => complexT<T>;

	if (isComplexPolyN(poly)) {
		from = poly.c[0].r.from;
		evaluate = (p, t) => p.evaluate(t);
	} else {
		from = poly.c[0].from;
		evaluate = (p, t) => {
			let i = p.c.length - 1;
			let r = t.add(new complexT<T>(p.c[i], p.c[i].scale(0)));
			while (i--)
				r = r.mul(t).add(new complexT<T>(p.c[i], p.c[i].scale(0)));
			return r;
		};
	}

	const n			= poly.degree();
	const zero		= from(0), one = from(1);
	const czero		= new complexT<T>(zero, zero), cone = new complexT<T>(one, zero);
	const dpoly 	= poly.normalised_deriv();

	const radius	= lagrangeImprovedT<T>(poly);
	const roots		= Array.from({length: n}, (_, i) => complexT.fromPolar(radius, 2 * Math.PI * i / n));

	tolerance ??= from(1e-6);

	for (let iter = 0; iter < maxIterations; iter++) {
		let maxCorrection = zero;
		for (let i = 0; i < n; i++) {
			const zi	= roots[i];
			const p_zi	= evaluate(poly, zi);
			const dp_zi	= evaluate(dpoly, zi).scale(n);

			let sum		= czero;
			for (let j = 0; j < roots.length; j++) {
				if (i !== j)
					sum = sum.add(cone.div(zi.sub(roots[j])));
			}
			const correction = p_zi.div((dp_zi.sub(p_zi.mul(sum))));
			roots[i] = roots[i].sub(correction);
			maxCorrection = maxT(maxCorrection, correction.abs());
		}
		if (maxCorrection.lt(tolerance))
			break;
	}
	return roots;

}

//-----------------------------------------------------------------------------
//	Legendre polynomial and roots
//-----------------------------------------------------------------------------

export function legendrePolynomial(n: number): polynomial {
	let P0 = new polynomial([1]);
	if (n === 0)
		return P0;

	let P1 = new polynomial([0, 1]);

	for (let k = 2; k <= n; ++k) {
		const x = new polynomial([0, 1]); // x
		const term1 = x.mul(P1).mul(2 * k - 1);
		const term2 = P0.mul(k - 1);
		const Pk = term1.sub(term2).mul(1 / k);

		P0 = P1;
		P1 = Pk;
	}

	return P1;
}

export function legendreTable(n: number): [number, number][] {
	const P		= legendrePolynomial(n);
	const dP	= P.deriv();
	const roots = P.realRoots(); // returns x_i in [-1, 1]

	return roots.map(x => {
		const dp = dP.evaluate(x);
		return [x, 2 / ((1 - x * x) * dp * dp)];
	});

}