import {ops, scalar, from, extent1} from './core';
import complex, { complexT } from './complex';

const sqrt3	= Math.sqrt(3);
const defaultEpsilon = 1e-9;

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

//-----------------------------------------------------------------------------
//	Polynomial with real coefficients
//-----------------------------------------------------------------------------

export class polynomial {
	constructor(public c: number[]) {}
	degree()	{ return this.c.length - 1; }

	evaluate(t: complex): complex;
	evaluate(t: number): number;
	evaluate(t: number[]): number[];
	evaluate(t: number|number[]|complex) {
		if (typeof t === 'number') {
			let r = 0;
			for (let i = this.c.length; i--; )
				r = r * t + this.c[i];
			return r;
		}
		if (Array.isArray(t)) {
			const r = new Array<number>(t.length).fill(0);
			for (let i = this.c.length; i--; )
				r.forEach((x, j) => r[j] = x * t[j] + this.c[i]);
			return r;
		}
		// else if (t instanceof complex) {
		let r = complex(0, 0);
		for (let i = this.c.length; i--; )
			r = r.mul(t).add(complex(this.c[i]));
		return r;
		//}
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
	divmod(b: number|polynomial) {
		if (typeof b === 'number')
			return this.mul(1 / b);

		const blen	= b.c.length;
		const bt = b.c[blen - 1];
		const dc = new Array<number>(this.c.length - blen);

		for (let i = dc.length; --i;) {
			const d = this.c[i + blen] / bt;
			for (let j = 0; j < b.c.length; j++)
				this.c[i - j] -= d * b.c[j];
			dc[i] = d;
		}
		this.c.length = b.c.length - 1;
		return new polynomial(dc);
	}
	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && Math.abs(this.c[i]) < epsilon)
			i--;
		const f = 1 / this.c[i];
		return new polynomialN(this.c.slice(0, i).map(v => v * f));
	}
	roots(): number[] {
		return this.normalise().roots();
	}
	allRoots(): (complex|number)[] {
		return this.normalise().allRoots();
	}
	refine_roots(x: number[], count = 1) {
		const	dpoly	= this.deriv();
		const	ddpoly	= dpoly.deriv();
	
		while (count--) {
			x = x.map(x => {
				const	f	= this.evaluate(x);
				const	f1	= dpoly.evaluate(x);
				const	f2	= ddpoly.evaluate(x);
				return x - (f * f1 * 2) / (f1 * f1 * 2 - f * f2);
			});
		}
		return x;
	}
}

//-----------------------------------------------------------------------------
//	Normalised Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

export class polynomialN {
	constructor(public c: number[]) {}
	degree()	{ return this.c.length; }

	evaluate(t: complex): complex;
	evaluate(t: number): number;
	evaluate(t: number[]): number[];
	evaluate(t: number|number[]|complex) {
		if (typeof t === 'number') {
			let r = 1;
			for (let i = this.c.length; i--; )
				r = r * t + this.c[i];
			return r;
		}
		if (Array.isArray(t)) {
			const r = new Array<number>(t.length).fill(1);
			for (let i = this.c.length; i--; )
				r.forEach((x, j) => r[j] = x * t[j] + this.c[i]);
			return r;
		}
//		if (t instanceof complex) {
			let r = complex(1, 0);
			for (let i = this.c.length; i--; )
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
		const blen	= b.c.length;
		const dc = new Array<number>(this.c.length - blen);

		for (let i = dc.length; --i;) {
			const d = this.c[i + blen];
			for (let j = 0; j < b.c.length; j++)
				this.c[i - j] -= d * b.c[j];
			dc[i] = d;
		}
		this.c.length = b.c.length - 1;
		return new polynomial(dc);
	}
	roots(epsilon = defaultEpsilon): number[] {
		return normPolyRealRoots(this.c, epsilon);
	}
	allRoots(epsilon = defaultEpsilon): (complex|number)[] {
		if (this.c.length > 5)
			return aberth(this);
		return normPolyComplexRoots(this.c, epsilon);
	}
}

//-----------------------------------------------------------------------------
//	Polynomial with generic coefficients supporting ops interface
//-----------------------------------------------------------------------------

function lessThan(a: number | scalar<any>, b: number) {
	return a.valueOf() < b;
}

export class polynomialT<T extends ops<T>> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length - 1; }

	evaluate(t: number|T): T;
	evaluate(t: number[]|T[]): T[];
	evaluate(t: number|T|number[]|T[]) {
		let i = this.c.length - 1;
		const lead = this.c[i];
		if (Array.isArray(t)) {
			const r = new Array<T>(t.length).fill(lead);
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
			let r = lead;
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
		if (typeof b === 'number')
			return new polynomialT(this.c.map(a => a.scale(b)));

		if (b instanceof polynomialT) {
			const zero = this.c[0].scale(0);
			return new polynomialT(Array.from({ length: this.c.length + b.c.length - 1 }, (_, k) => {
				let sum = zero;
				for (let i = Math.max(0, k - b.c.length + 1); i <= Math.min(this.c.length - 1, k); i++)
					sum = sum.add(this.c[i].mul(b.c[k - i]));
				return sum;
			}));
		}
		return new polynomialT(this.c.map(a => a.mul(b)));
	}
	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && lessThan(this.c[i].mag(), epsilon))
			i--;
		const f = this.c[i];
		return new polynomialNT<T>(this.c.slice(0, i).map(v => v.div(f)));
	}
	map(func: (c: T, i: number) => number) {
		return new polynomial(this.c.map((c, i) => func(c, i)));
	}
}

//-----------------------------------------------------------------------------
//	Normalised General Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

//type polynum<C extends ops<C>, S = C extends complex ? number : C extends complexT<infer S> ? S : C extends scalar<any> ? C : never> = ops<C> & { abs(): S };

export class polynomialNT<T extends ops<T>> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length; }


	evaluate(t: T): T;
	evaluate(t: T[]): T[];
	evaluate(t: T|T[]) {
		let i = this.c.length - 1;
		if (Array.isArray(t)) {
			const r = t.slice();
			r.forEach((x, j) => r[j] = x.add(this.c[i]));
			while (i--)
				r.forEach((x, j) => r[j] = x.mul(t[j] as T).add(this.c[i]));
			return r;
		} else {
			let r = t.add(this.c[i]);
			while (i--)
				r = r.mul(t).add(this.c[i]);
			return r;
		}
	}

	deriv() {
		return new polynomialT([...this.c.slice(1).map((v, i) => v.scale(i + 1))]);//, this.c.length]);
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
//	roots(epsilon = defaultEpsilon): number[] {
//		return normPolyRealRoots(this.c, epsilon);
//	}
//	allRoots(epsilon = defaultEpsilon): (complex|number)[] {
//		return aberthT(this, epsilon);
//	}
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


// Lower bound: apply improved lagrange to reversed coefficients
//const lower = 1 / sumTop2(k.c.map((c, i) => Math.pow(Math.abs(c), 1 / (i + 1))));
//return new extent1(lower, upper);

// Upper bound (improved lagrange)

function lagrangeImproved(k: polynomialN): number {
	const N = k.c.length;
	return sumTop2(k.c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i))));
}

function realBound(k: polynomialN): extent1 {
	const N = k.c.length;
	const terms = k.c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i)));

	return new extent1(
		-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (k.c[i] > 0))),
		sumTop2(terms.filter((x, i) => k.c[i] < 0))
	);
}
/*
function samuelsonBounds(k: polynomialN): extent1 {
	const N		= k.c.length;
	const aN1	= k.c[N - 1];
	const aN2	= k.c[N - 2];
	const a		= -aN1 / (N + 1);
	const b		= N / (N + 1) * Math.sqrt(aN1 * aN1 - 2 * (N + 1) / N * aN2);
	return new extent1(a - b, a + b);
}
*/
function lagrangeImprovedComplex(k: polynomialNT<complex>): number {
	const N = k.c.length;
	return sumTop2(k.c.map((c, i) => Math.pow(c.abs(), 1 / (N - i))));
}

function sumTop2T<T extends scalar<T>>(a: T[]) {
	let max1 = a[0], max2 = a[0];;
	for (const v of a) {
		if (v.gt(max1)) {
			max2 = max1;
			max1 = v;
		} else if (v.gt(max2)) {
			max2 = v;
		}
	}
	return max1.add(max2);
}

function lagrangeImprovedT<T extends scalar<T>>(k: polynomialNT<T>|polynomialNT<complexT<T>>) {
	const N = k.c.length;
	return sumTop2T(k.c.map((c, i) => c.abs().pow(1 / (N - i))));
}

//-----------------------------------------------------------------------------
//	halley's method to refine roots
//-----------------------------------------------------------------------------

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
	const	rootsd	= dpoly.roots();
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
				// ... and take the one real solution to build two quadratic equations
				const m = r[0];
				const v = Math.sqrt(m * 2);
				const u = q / Math.sqrt(m * 8);

				return [...normPolyRealRoots([m + p / 2 - u,  v], epsilon), ...normPolyRealRoots([m + p / 2 + u, -v], epsilon)].map(i => i - a / 4);
				//if (ra.length === 0 && rb.length === 0)
				//	return [];
				//return halley(new polynomialN(k), [...ra, ...rb].map(i => i - a / 4));
			}

			case 5: {
				const poly		= new polynomialN(k);
				const dpoly		= poly.normalised_deriv();
				const d2poly	= dpoly.normalised_deriv();
				const r1		= dpoly.roots();
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
				return aberth(new polynomialN(k)).filter(r => Math.abs(r.i) < epsilon).map(r => r.r).sort((a, b) => a - b);
		}
	}
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
					// ... and take the one real solution to build two quadratic equations
					const m = r[0];
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
	const radius = poly instanceof polynomialN ? lagrangeImproved(poly) : lagrangeImprovedComplex(poly);
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

function isComplexPolyN<T extends scalar<T>>(poly: polynomialNT<T>|polynomialNT<complexT<T>>): poly is polynomialNT<complexT<T>> {
	return poly.c[0] instanceof complexT;
}

export function aberthT<T extends scalar<T>>(poly: polynomialNT<T>|polynomialNT<complexT<T>>, tolerance?: T, maxIterations = 100) {
	let f: (v: number) => T;
	let evaluate: (p: any, t: complexT<T>) => complexT<T>;

	if (isComplexPolyN(poly)) {
		f = from(poly.c[0].r);
		evaluate = (p, t) => p.evaluate(t);
	} else {
		f = from(poly.c[0]);
		evaluate = (p, t) => {
			let i = p.c.length - 1;
			let r = t.add(new complexT<T>(p.c[i], p.c[i].scale(0)));
			while (i--)
				r = r.mul(t).add(new complexT<T>(p.c[i], p.c[i].scale(0)));
			return r;
		};
	}

	const n		= poly.degree();
	const zero	= f(0), one = f(1);
	const czero	= new complexT<T>(zero, zero), cone = new complexT<T>(one, zero);
	const dpoly = poly.normalised_deriv();

	const radius	= lagrangeImprovedT<T>(poly);
	const roots		= Array.from({length: n}, (_, i) => complexT.fromPolar(radius, 2 * Math.PI * i / n));

	tolerance ??= f(1e-6);

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
			const absCorrection = correction.abs();
			if (absCorrection.gt(maxCorrection))
				maxCorrection = absCorrection;
		}
		if (tolerance.gt(maxCorrection))
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
	const roots = P.roots(); // returns x_i in [-1, 1]

	return roots.map(x => {
		const dp = dP.evaluate(x);
		return [x, 2 / ((1 - x * x) * dp * dp)];
	});

}
