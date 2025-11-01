/* eslint-disable no-restricted-syntax */
import {ops, scalar, scalar2, sign, extent1, maxT, extentT, isInstance, isScalar, asScalar2, asScalarT, lcm, lcmB, absB, rootB, compare } from './core';
import { factorisation, factorisationB } from './prime';
import complex, { complexT } from './complex';
import { rational, rationalB } from './rational';

const sqrt3	= Math.sqrt(3);
const defaultEpsilon = 1e-9;

//-----------------------------------------------------------------------------
//	helpers
//-----------------------------------------------------------------------------

type ifScalar<T, U> = T extends scalar<any> ? U : never;

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

function arrayOf<U>(arr: any[], g: (x: any) => x is U): arr is U[] {
	return g(arr[0]);
}

const superscriptedDigits = '⁰¹²³⁴⁵⁶⁷⁸⁹';
function superScript(n: number): string {
	let s = '';
	do {
		s = superscriptedDigits[n % 10] + s;
		n = Math.floor(n / 10);
	} while (n > 0);
	return s;
}

function toString(t: any, debug = false): string {
	return debug && typeof t === 'object' && t !== null && Symbol.for("debug.description") in t ? t[Symbol.for("debug.description")]() : String(t);
}

function coefficientString<T>(coef: T, i: number, debug: boolean): string {
	return (i ? `${coef === 1 ? '' : toString(coef, debug)}x${i > 1 ? superScript(i) : ''}` : `${toString(coef, debug)}`);
}

function polynomialString<T extends ops<any>>(coefficients: T[], debug: boolean): string {
	return  coefficients.map((coef: T, i) => coefficientString(coef, i, debug)).reverse().join('');
}

function scalarPolynomialString<T extends scalar<any>>(coefficients: T[], debug: boolean): string {
	return coefficients.map((coef, i) =>
		(coef.sign() < 0 ? ' - ' : ' + ') + coefficientString(coef.abs(), i, debug)
	).reverse().join('');
}

function lessThan<T extends scalar<T>>(a: number | bigint | T, b: number): boolean {
	return	typeof a === 'number' ? a < b
		:	typeof a === 'bigint' ? a < BigInt(b)
		:	a.lt(a.from(b));
}

//-----------------------------------------------------------------------------
//	Polynomial with real coefficients
//-----------------------------------------------------------------------------

export class polynomial {
	constructor(public c: number[]) {}
	degree()	{ return this.c.length - 1; }
	leadCoeff() { return this.c[this.c.length - 1]; }
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
	divRoot(root: number | rational): [polynomial, number] {
		let i = this.c.length - 1;
		if (i < 1)
			return [new polynomial([]), i ? this.c[0] : 0];

		const b = new Array<number>(i);
		if (typeof root === 'number') {
			b[i - 1] = this.c[i];
			while (--i)
				b[i - 1] = this.c[i] + root * b[i];
			return [new polynomial(b), this.c[0] + root * b[0]];

		} else {
			b[i - 1] = this.c[i];
			while (--i)
				b[i - 1] = this.c[i] * root.den + root.num * b[i];
			return [new polynomial(b), this.c[0] * root.den + root.num * b[0]];
		}
	}
	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && Math.abs(this.c[i]) < epsilon)
			i--;
		const f = 1 / this.c[i];
		return new polynomialN(this.c.slice(0, i).map(v => v * f));
	}
	rationalRoots(): number[] | rational[] {
		return rationalRoots(this.dup());
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
	toString(debug = false) {
		return this.c.map((coef, i) => {
			const s = i === this.degree() ? '' : coef < 0 ? ' - ' : ' + ';
			if (s)
				coef = Math.abs(coef);
			return s + coefficientString(coef, i, debug);
		}).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
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
		// complex
		let r = t.add(complex(this.c[--i]));
		while (i--)
			r = r.mul(t).add(complex(this.c[i]));
		return r;
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
	toString(debug = false) {
		return this.c.map((coef, i) =>
			(coef < 0 ? ' - ' : ' + ') + coefficientString(Math.abs(coef), i, debug)
		).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	Polynomial with bigint coefficients
//-----------------------------------------------------------------------------

export class polynomialB {
	constructor(public c: bigint[]) {}
	degree()	{ return this.c.length - 1; }
	leadCoeff() { return this.c[this.c.length - 1]; }
	dup()		{ return new polynomialB(this.c.slice()); }

	evaluate(t: bigint[]): bigint[];
	evaluate(t: bigint): bigint;
	evaluate(t: rationalB): rationalB;
	evaluate(t: bigint|rationalB|bigint[]) {
		let i = this.c.length;
		if (Array.isArray(t)) {
			const r = new Array<bigint>(t.length).fill(this.c[--i]);
			while (i--)
				r.forEach((x, j) => r[j] = x * t[j] + this.c[i]);
			return r;
		}
		if (t instanceof rationalB) {
			let acc		= this.c[--i];
			let denPow = 1n;
			while (i--) {
				denPow *= t.den;
				acc = acc * t.num + this.c[i] * denPow;
			}
			return new rationalB(acc, denPow);
/*

			const deg = --i;
			let	acc = this.c[i] * (t.num ** BigInt(i));
			while (--i)
				acc +=  this.c[i] * (t.num ** BigInt(i)) * (t.den ** BigInt(deg - i));
			return new rationalB(acc, t.den ** BigInt(deg));
			*/
		}

		let r = this.c[--i];
		while (i--)
			r = r * t + this.c[i];
		return r;
	}
	deriv() {
		return new polynomialB(this.c.slice(1).map((v, i) => v * BigInt(i + 1)));
	}
	add(b: polynomialB) {
		return new polynomialB(Array.from({length: Math.max(this.c.length, b.c.length)}, (_, i) => (this.c[i] || 0n) + (b.c[i] || 0n)));
	}
	sub(b: polynomialB) {
		return new polynomialB(Array.from({length: Math.max(this.c.length, b.c.length)}, (_, i) => (this.c[i] || 0n) - (b.c[i] || 0n)));
	}
	mul(b: bigint|polynomialB) {
		if (typeof b === 'bigint')
			return new polynomialB(this.c.map(a => a * b));

		const r = new Array<bigint>(this.c.length + b.c.length - 1).fill(0n);
		for (let i = 0; i < this.c.length; i++)
			for (let j = 0; j < b.c.length; j++)
				r[i + j] += this.c[i] * b.c[j];
		return new polynomialB(r);
	}

	divmod(b: polynomialB) {
		const bdeg 	= b.c.length - 1;
		const bt	= b.c[bdeg];
		const a		= this.c.map(v => v * bt);
		const q		= new Array<bigint>(Math.max(0, a.length - bdeg));

		for (let i = q.length; i--;) {
			const d = a[i + bdeg] / bt;
			q[i] = d;
			for (let j = 0; j < bdeg; j++)
				a[i + j] -= d * b.c[j];
		}

		let rlen = bdeg;
		while (rlen > 0 && a[rlen - 1] === 0n)
			rlen--;
		this.c = a.slice(0, rlen);
		return new polynomialB(q);
	}
	
	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && absB(this.c[i]) < epsilon)
			i--;
		const d = this.c[i];
		return new polynomialNT<rationalB>(this.c.slice(0, i).map(v => new rationalB(v, d)));
	}

	divRoot(root: bigint|rationalB): [polynomialB, bigint] {
		let i = this.c.length - 1;
		if (i < 1)
			return [new polynomialB([]), i ? this.c[0] : 0n];

		const b = new Array<bigint>(i);
		if (typeof root === 'bigint') {
			b[i - 1] = this.c[i];
			while (--i)
				b[i - 1] = this.c[i] + root * b[i];
			return [new polynomialB(b), this.c[0] + root * b[0]];

		} else {
			b[i - 1] = this.c[i];
			while (--i)
				b[i - 1] = this.c[i] * root.den + root.num * b[i];
			return [new polynomialB(b), this.c[0] * root.den + root.num * b[0]];
		}
	}

	rationalRoots(): bigint[]|rationalB[] {
		return rationalRootsB(this.dup());
	}

	realRoots() {
		const p = this.dup();
		const roots: (bigint|rationalB)[] = rationalRootsB(p);
		if (p.degree() > 0)
			roots.push(...p.normalise().realRoots());
		return roots;
	}

	toString(debug = false) {
		return this.c.map((coef, i) => {
			const s = i === this.degree() ? '' : coef < 0n ? ' - ' : ' + ';
			if (s)
				coef = absB(coef);
			return s + coefficientString(coef, i, debug);
		}).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	Polynomial with generic coefficients supporting ops interface
//-----------------------------------------------------------------------------

type inferComplexRoots<T> = T extends scalar2<infer R> ? complexT<R>[] : never;

export class polynomialT<T extends ops<T>> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length - 1; }
	leadCoeff() { return this.c[this.c.length - 1]; }
	dup()		{ return new polynomialT<T>(this.c.slice()); }

	evaluate(t: number|T): T;
	evaluate(t: number[]|T[]): T[];
	evaluate(t: number|T|number[]|T[]) {
		let i = this.c.length;
		if (i === 0)
			return 0 as any as T;
		
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

		return new polynomialT(typeof b === 'number' ? this.c.map(a => a.scale(b)) : this.c.map(a => a.mul(b)));
	}
	
	div(b: number|polynomialT<T>|T) {
		if (b instanceof polynomialT)
			return this.dup().divmod(b);
		return new polynomialT(typeof b === 'number' ? this.c.map(a => a.scale(1 / b)) : this.c.map(a => a.div(b)));
	}

	divmod(b: polynomialT<T>) {
		const a		= this.c;
		const bdeg	= b.c.length - 1;
		const alen	= a.length;
		const bt	= b.c[bdeg];
		const q		= new Array<T>(Math.max(0, alen - bdeg));
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

	rationalRoots(): ifScalar<T, ReturnType<typeof rationalRootsT>>  {
		if (arrayOf(this.c, isScalar))
			return rationalRootsT(this.c) as ifScalar<T, ReturnType<typeof rationalRootsT>>;
		return undefined as never;
	}

	realRoots(epsilon = defaultEpsilon): ifScalar<T, T[]>  {
		return this.normalise().realRoots(epsilon);
	}
	allRoots(epsilon = defaultEpsilon): inferComplexRoots<T>  {
		return this.normalise().allRoots(epsilon);
	}

	map(func: (c: T, i: number) => number) {
		return new polynomial(this.c.map((c, i) => func(c, i)));
	}
	toString(debug = false) {
		const c = this.c.slice(0, -1);
		const n = c.length;
		return coefficientString(this.c[n], n, debug) + (arrayOf(c, isScalar) ? scalarPolynomialString(c, debug) : polynomialString(c, debug));
	}
//	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	Normalised General Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

export class polynomialNT<T extends ops<T>> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length; }
	dup()		{ return new polynomialNT<T>(this.c.slice()); }

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

	realRoots(epsilon = defaultEpsilon): ifScalar<T, T[]> {
		if (isScalar(this.c[0]))
			return sturmRealRootsT(this, this.c[0].from(epsilon)) as ifScalar<T, T[]>;
		return undefined as never;
	}

	allRoots(epsilon = defaultEpsilon): inferComplexRoots<T> {
		if (isScalar(this.c[0]))
			return aberthT(this, this.c[0].from(epsilon)) as inferComplexRoots<T>;
		return undefined as never;
	}
	toString(debug = false) {
		return coefficientString(1, this.c.length, debug) + (arrayOf(this.c, isScalar) ? scalarPolynomialString(this.c, debug) : polynomialString(this.c, debug));
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
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

function sumTop2B(a: bigint[]) {
	if (a.length < 2)
		return a.length === 1 ? a[0] : 0n;
	let max1 = a[0], max2 = a[0];
	for (const v of a) {
		if (max1 < v) {
			max2 = max1;
			max1 = v;
		} else if (max2 < v) {
			max2 = v;
		}
	}
	return max1 + max2;
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


function lagrangeImproved(c: number[]|complex[]): number {
	const N = c.length;
	if (arrayN(c))
		return sumTop2(c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i))));
	return sumTop2(c.map((c, i) => Math.pow(c.abs(), 1 / (N - i))));
}

function lagrangeImprovedB(c: bigint[]): bigint {
	const N = c.length;
	return sumTop2B(c.map((c, i) => rootB(absB(c), N - i)));
}

function lagrangeImprovedT<T extends scalar2<T>>(c: T[]|complexT<T>[]) {
	const N = c.length;
	const zero = c[0] instanceof complexT ? c[0].r.from(0) : c[0].from(0);
	return sumTop2T(zero, c.map((c, i) => c.abs().pow(1 / (N - i))));
}

function realBound(c: number[]): extent1 {
	const N = c.length;
	const terms = c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i)));

	return new extent1(
		-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (c[i] > 0))),
		sumTop2(terms.filter((x, i) => c[i] < 0))
	);
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
					let 	r0		= 2 * c * rf - e;
					let 	r1		= (-c - sqrt3 * s) * rf - e;
					let 	r2		= (-c + sqrt3 * s) * rf - e;
					if (r0 > r1)
						[r0, r1] = [r1, r0];
					if (r0 > r2)
						[r0, r2] = [r2, r0];
					if (r1 > r2)
						[r1, r2] = [r2, r1];
					return	[r0, r1, r2];
			
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
				const bounds	= realBound(k);

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
	const bounds = realBound(pN.c);
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
	if (n === 1)
		return [pN.c[0].neg()];

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
				const eps = maxT(x.from(1e-12), x.abs().scale(1e-12));//Math.max(1e-12, Math.abs(x) * 1e-12);
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
	let lower = asScalarT(epsilon, bounds.min).sub(epsilon);
	let upper = asScalarT(epsilon, bounds.max).add(epsilon);
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
	const radius = lagrangeImproved(poly.c);
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

	const radius	= lagrangeImprovedT<T>(poly.c);
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

//-----------------------------------------------------------------------------
// Integer/Rational Root Theorem extractor for polynomials with bigint coefficients
//-----------------------------------------------------------------------------

function removeMultiplicityB(p: polynomialB, root: bigint|rationalB): boolean {
	while (p.degree() > 0) {
		const [ quotient, remainder ] = p.divRoot(root);
		if (remainder !== 0n)
			return false;
		p.c = quotient.c;
	}
	return true;
}

function* rationalDivisorsB(numFactors: Map<bigint, number>, denFactors: Map<bigint, number>, limit?: rationalB): Generator<rationalB> {
	const numEntries = Array.from(numFactors.entries());
	const denEntries = Array.from(denFactors.entries());
	const primes = new Set<bigint>();

	function* backtrackNum(i: number, num: bigint, den: bigint): Generator<rationalB> {
		if (limit && num * limit.den > den * limit.num)
			return;
		if (i === numEntries.length) {
			yield new rationalB(num, den);
			return;
		}
		const [p, exp] = numEntries[i++];
		if (primes.has(p)) {
			yield* backtrackNum(i, num, den);
		} else {
			for (let e = 0; e <= exp; e++, num *= p)
				yield* backtrackNum(i, num, den);
		}
	}

	function* backtrackDen(i: number, den: bigint): Generator<rationalB> {
		if (i === denEntries.length) {
			if (numEntries.length === 0)
				yield new rationalB(1n, den);
			else
				yield *backtrackNum(0, 1n, den);
			return;
		}
		const [p, exp] = denEntries[i++];
		yield* backtrackDen(i, den);
		primes.add(p);
		for (let e = 1; e <= exp; e++)
			yield* backtrackDen(i, den *= p);
	}

	yield *backtrackDen(0, 1n);
}

// modifies p to remove found roots
export function rationalRootsB(p: polynomialB): bigint[] | rationalB[] {
	const roots0: bigint[] = [];
	if (p.c[0] === 0n)
		roots0.push(0n);

	while (p.c[0] === 0n)
		p = p.divRoot(0n)[0];

	if (p.leadCoeff() === 1n) {
		const a0		= p.c[0];
		const factors	= new factorisationB(a0);
		const bound		= lagrangeImprovedB(p.c.slice(0, -1));

		for (const x of factors.divisors(bound)) {
			if (p.evaluate(x) === 0n) {
				roots0.push(x);
				if (removeMultiplicityB(p, x))
					break;
			}
			if (p.evaluate(-x) === 0n) {
				roots0.push(-x);
				if (removeMultiplicityB(p, -x))
					break;
			}
		}
		return roots0.sort(compare);
	}

	const roots: rationalB[] = roots0.map(r => new rationalB(r));

	const a0 = absB(p.c[0]);
	const an = absB(p.leadCoeff());

	const pFactors	= new factorisationB(a0);
	const qFactors	= new factorisationB(an);
	const bound 	= realBoundT(p.normalise());
	const bound2	= rationalB.from(+maxT(bound.min.neg(), bound.max));

	for (const r of rationalDivisorsB(pFactors, qFactors, bound2)) {
		let	i		= p.degree();
		let acc1	= p.c[i];
		let acc2	= p.c[i];
		for (let denPow = r.den; i--; denPow *= r.den) {
			const c = p.c[i] * denPow;
			acc1 = acc1 *  r.num + c;
			acc2 = acc2 * -r.num + c;
		}

		if (acc1 === 0n) {
			roots.push(r);
			if (removeMultiplicityB(p, r))
				break;
		}
		if (acc2 === 0n) {
			const r2 = r.neg();
			roots.push(r2);
			if (removeMultiplicityB(p, r2))
				break;
		}
	}
	return roots.sort((a, b) => a.compare(b));
}


function removeMultiplicity(p: polynomial, root: number|rational): boolean {
	while (p.degree() > 0) {
		const [ quotient, remainder ] = p.divRoot(root);
		if (remainder !== 0)
			return false;
		p.c = quotient.c;
	}
	return true;
}

function* rationalDivisors(numFactors: number[], denFactors: number[], limit?: rational): Generator<rational> {
	const numEntries = Array.from(numFactors.entries());
	const denEntries = Array.from(denFactors.entries());
	const primes = new Set<number>();

	function* backtrackNum(i: number, num: number, den: number): Generator<rational> {
		if (limit && num * limit.den > den * limit.num)
			return;
		if (i === numEntries.length) {
			yield new rational(num, den);
			return;
		}
		const [p, exp] = numEntries[i++];
		if (primes.has(p)) {
			yield* backtrackNum(i, num, den);
		} else {
			for (let e = 0; e <= exp; e++, num *= p)
				yield* backtrackNum(i, num, den);
		}
	}

	function* backtrackDen(i: number, den: number): Generator<rational> {
		if (i === denEntries.length) {
			if (numEntries.length === 0)
				yield new rational(1, den);
			else
				yield *backtrackNum(0, 1, den);
			return;
		}
		const [p, exp] = denEntries[i++];
		yield* backtrackDen(i, den);
		primes.add(p);
		for (let e = 1; e <= exp; e++)
			yield* backtrackDen(i, den *= p);
	}

	yield *backtrackDen(0, 1);
}

// modifies p to remove found roots
export function rationalRoots(p: polynomial): number[] | rational[] {
	const roots0: number[] = [];
	if (p.c[0] === 0)
		roots0.push(0);

	while (p.c[0] === 0)
		p = p.divRoot(0)[0];

	const r = p.c.map(c => rational.from(c));
	const m = lcm(...r.map(v => v.den));
	const p2 = new polynomial(r.map(v => v.num * (m / v.den)));

	p = p2;

	if (p.leadCoeff() === 1) {
		const a0		= p.c[0];
		const factors	= new factorisation(a0);
		const bound		= lagrangeImproved(p.c.slice(0, -1));

		for (const x of factors.divisors(bound)) {
			if (p.evaluate(x) === 0) {
				roots0.push(x);
				if (removeMultiplicity(p, x))
					break;
			}
			if (p.evaluate(-x) === 0) {
				roots0.push(-x);
				if (removeMultiplicity(p, -x))
					break;
			}
		}
		return roots0;
	}

	const roots: rational[] = roots0.map(r => new rational(r));

	const a0 = Math.abs(p.c[0]);
	const an = Math.abs(p.leadCoeff());

	const pFactors	= new factorisation(a0);
	const qFactors	= new factorisation(an);
	const bound 	= rational.from(lagrangeImproved(p.normalise().c));

	for (const r of rationalDivisors(pFactors, qFactors, bound)) {
		let	i		= p.degree();
		let acc1	= p.c[i];
		let acc2	= p.c[i];
		for (let denPow = r.den; i--; denPow *= r.den) {
			const c = p.c[i] * denPow;
			acc1 = acc1 *  r.num + c;
			acc2 = acc2 * -r.num + c;
		}

		if (acc1 === 0) {
			roots.push(r);
			if (removeMultiplicity(p, r))
				break;
		}
		if (acc2 === 0) {
			const r2 = r.neg();
			roots.push(r2);
			if (removeMultiplicity(p, r2))
				break;
		}
	}
	return roots.sort((a, b) => a.compare(b));
}

function rationalRootsT<T extends scalar<T>>(p: T[]) {
	if (arrayOf(p, v => isInstance<rationalB>(v, rationalB))) {
		const m = lcmB(...p.map((v: rationalB) => v.den));
		const p2 = new polynomialB(p.map((v: rationalB) => v.num * (m / v.den)));
		return rationalRootsB(p2);
	}
	if (arrayOf(p, v => isInstance<rational>(v, rational))) {
		const m = lcm(...p.map((v: rational) => v.den));
		const p2 = new polynomial(p.map((v: rational) => v.num * (m / v.den)));
		return rationalRoots(p2);
	}

//	const p2 = p.map(c => continuedFractionT(c, 20, c.from(1e-8)));
//	const p3a = p2.map(cf => rationalB.fromContinuedFraction(cf));
	const p3 = p.map(c => rationalB.from(c, 1n << 32n));
	const m = lcmB(...p3.map(v => v.den));
	const p4 = new polynomialB(p3.map(v => v.num * (m / v.den)));
	return rationalRootsB(p4);
}
