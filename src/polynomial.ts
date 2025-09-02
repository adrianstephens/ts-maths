import {ops, extent1} from './vector.js';
import complex from './complex.js';

const sqrt3	= Math.sqrt(3);
const defaultEpsilon = 1e-9;

function copySign(a: number, b: number) {
	return b < 0 ? -Math.abs(a) : Math.abs(a);
}

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
		if (this.c.length > 5)
			return aberth(this).filter(r => Math.abs(r.i) < epsilon).map(r => r.r).sort((a, b)=> a - b);
		return normPolyRealRoots(this.c, epsilon);
	}
	allRoots(epsilon = defaultEpsilon): (complex|number)[] {
		if (this.c.length > 5)
			return aberth(this);
		return normPolyComplexRoots(this.c, epsilon);
	}
}

//-----------------------------------------------------------------------------
//	lagrange root bounds
//-----------------------------------------------------------------------------

function reduceMax(a: number[]) {
	return a.reduce((max, i) => Math.max(max, i), 0);
}

function bound_lagrange(k: polynomialN) {
	const N	= k.c.length;
	const bounds = k.c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i)));
	const upper	= bounds.filter((b, i) => k.c[i] < 0);
	const lower	= bounds.filter((b, i) => (k.c[i] * (i & 1 ? -1 : 1)) < 0);
	const u1	= reduceMax(upper);
	const l1	= reduceMax(lower);
	
	return new extent1(
		-(l1 + reduceMax(lower.filter(i => i != l1))),
		u1 + reduceMax(upper.filter(i => i !== u1))
	);
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
/*
function halley(poly: polynomial|polynomialN, x: number[]) {
	const	dpoly	= poly.deriv();
	const	ddpoly	= dpoly.deriv();
	return _halley(poly, dpoly, ddpoly, x);
}
*/

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

function normPolyRealRoots(k: number[], epsilon: number): number[] {
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
			//biquadratic case
			if (k[1] === 0 && k[3] === 0) {
				const r = normPolyRealRoots([k[0], k[2]], epsilon).map(r => Math.sqrt(r));
				return r.length === 1
					? [-r[0], r[0]]
					: [-r[1], -r[0], r[0], r[1]];
			}

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
			const vals 		= dpoly.evaluate(r1);
			const bounds	= bound_lagrange(poly);

			let r: number[];

			if (r1.length === 4) {
				r = [];
				let prevv = -1;
				let prevr = bounds.min;
				for (let i = 0; i < 4; i++) {
					if (vals[i] * prevv <= 0)
						r.push((prevr + r1[i]) / 2);
					prevv = vals[i];
					prevr = r1[i];
				}
				if (vals[3] < 0)
					r.push((r1[3] + bounds.max) / 2);

			} else {
				const ranges = [
					bounds,
				];
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
			return [];
	}
}

function normPolyComplexRoots(k: number[], epsilon: number): (complex|number)[] {
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
			//biquadratic case
			if (k[1] === 0 && k[3] === 0)
				return normPolyComplexRoots([k[0], k[2]], epsilon).map(r => typeof r === 'number' ? Math.sqrt(r) : complex.conjugatePair(complex.sqrt(r))).flat();

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


export function aberth(poly: polynomialN, tolerance = 1e-6, maxIterations = 100) {
	const bounds = bound_lagrange(poly);
	const radius = Math.max(bounds.max, -bounds.min);
	const roots	= Array.from({length: poly.degree()}, (_, i) => complex.from(radius, 2 * Math.PI * i / poly.degree()));

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

export class polynomialT<T extends ops<T>> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length - 1; }

	evaluate(t: number): T;
	evaluate(t: number[]): T[];
	evaluate(t: number|number[]) {
		if (typeof t === 'number') {
			let r = this.c.at(-1)!;
			for (let i = this.c.length - 1; i--; )
				r = r.mul(t).add(this.c[i]);
			return r;
		}
		if (Array.isArray(t)) {
			const r = new Array<T>(t.length).fill(this.c.at(-1)!);
			for (let i = this.c.length - 1; i--; )
				r.forEach((x, j) => r[j] = x.mul(t[j]).add(this.c[i]));
			return r;
		}
	}
	deriv() {
		return new polynomialT(this.c.slice(1).map((v, i) => v.mul(i + 1)));
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
	
	map(func: (c: T, i: number) => number) {
		return new polynomial(this.c.map((c, i) => func(c, i)));
	}
}
