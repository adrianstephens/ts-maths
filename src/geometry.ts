import {vops, float2, normalise, approx_equal, mid, lerp} from './vector';
import {Polynomial, refine_roots, legendreTable} from './polynomial';

function NewtonRaphson(F: (t: number) => number, F_prime: (t: number) => number, t: number, maxIter = 20, tol = 1e-6) {
	for (let i = 0; i < maxIter; i++) {
		const f			= F(t);
		if (Math.abs(f) < tol)
			break;
		t -= f / F_prime(t);
		//if (Math.abs(delta) < tol)
		//	break;
	}
	return t;
}


/*
function GaussLegendre(t: number, F: (x: number) => number, n: number) {
    const weights = computeWeights(n);
    return weights.reduce((sum, [x, w]) => sum + w * F((t / 2) * (x + 1)), 0) * t / 2;
}
*/

function GaussLegendre(t: number, F: (t: number) => number, weights: [number, number][]) {
	return weights.reduce((sum, i) => sum + i[1] * F((t / 2) * (i[0] + 1)), 0) * t / 2;
}

const weights5 = legendreTable(5);
/*: [number, number][] = [
	[-0.90618,	0.236927],
	[-0.538469,	0.478629],
	[ 0,		0.568889],
	[ 0.538469,	0.478629],
	[ 0.90618,	0.236927]
];
*/
export class plane<T extends vops<T>> {
	static fromVert<T  extends vops<T>>(n: T, p: T) {
		n = normalise(n);
		return new plane(n, n.dot(p));
	}
	protected constructor(public n: T, public d: number) {
	}
	dist(p: T): number {
		return this.n.dot(p) - this.d;
	}
}

export class plane2 extends plane<float2> {
	static fromVerts(p0: float2, p1: float2) {
		return plane.fromVert(p1.sub(p0).perp(), p0);
	}
}

export function colinear<C extends vops<C>>(p1: C, p2: C, epsilon = 1e-9) {
	return p1.dot(p2) > (p1.len() * p2.len()) * (1 - epsilon);
}

//-----------------------------------------------------------------------------
//	shape
//-----------------------------------------------------------------------------

export interface shape<T extends vops<T>> {
	evaluate(t: number): T;
	tangent(t: number): T;
	evaluateWithDir(t: number) : {pos: T, dir: T};
	lengthTo(t: number): number;
}

/*
export class offsetShape0<T extends ops<T>, S extends shape<T>> implements shape<T> {
	offset: number;
	scale:	number;
	constructor(private shape: S, offset0: number, offset1: number) {
		this.offset	= offset0;
		this.scale	= offset1 - offset0;
	}
	getT(t: number)				{ return t * this.scale + this.offset; }
	evaluate(t: number)			{ return this.shape.evaluate(this.getT(t)); }
	tangent(t: number)			{ return this.shape.tangent(this.getT(t)).mul(this.scale); }
	evaluateWithDir(t: number)	{
		const result = this.shape.evaluateWithDir(this.getT(t));
		return {pos: result.pos, dir: result.dir.mul(this.scale)};
	}
	lengthTo(t: number)			{ return this.shape.lengthTo(this.getT(t)); }
}
*/
type shapeSlice<S> = S & {
	getT(t: number): number;
	slice(t0: number, t1:number): shapeSlice<S>;
}

export function shapeSlice<T extends vops<T>, S extends shape<T>>(base: S, t0: number, t1: number): shapeSlice<S> {
	const scale	= t1 - t0;
	const shape	= Object.create(base);

	const getT = (t: number) => t * scale + t0;

	shape.getT			= getT;
	shape.evaluate		= (t: number) => base.evaluate(getT(t));
	shape.tangent		= (t: number) => base.tangent(getT(t)).scale(scale);
	shape.evaluateWithDir = (t: number) => {
		const result = base.evaluateWithDir(getT(t));
		return {
			pos: result.pos,
			dir: result.dir.scale(scale)
		};
	};
	shape.lengthTo	= (t: number) => base.lengthTo(getT(t));
	shape.slice		= (t0: number, t1: number) => { return shapeSlice(base, getT(t0), getT(t1)); };

	return shape;
}

export function distanceToPoint<T extends vops<T>>(b: shape<T>, p: T, steps = 10): number {
	let minDist = 1e9;
	let last	= b.evaluate(0);
	for (let i = 1; i <= steps; i++) {
		const pos	= b.evaluate(i / steps);
		const dist	= new line<T>(last, pos).distanceToPoint(p);
		minDist		= dist < minDist ? dist : minDist;
		last		= pos;
	}

	return minDist;
}

export function distanceToT<T extends vops<T>>(b: shape<T>, distance: number, steps = 10, tol = 1e-6): number {
	//const length = b.lengthTo;
	//const S = length(1);
	const S = b.lengthTo(1);
	if (distance < 0)
		distance += S;
	distance = Math.max(0, Math.min(distance, S));

	return NewtonRaphson(
		t => b.lengthTo(t) - distance,
		t => b.tangent(t).len(),
		distance / S,	// Initial guess: Linear interpolation
		steps,
		tol
	);
}

//-----------------------------------------------------------------------------
//	line
//-----------------------------------------------------------------------------

export class line<T extends vops<T>> implements shape<T> {
	constructor(public p0: T, public p1: T) {}

	dir() 				{ return this.p1.sub(this.p0); }
	len() 				{ return this.dir().len(); }
	evaluate(t: number) { return this.p0.scale(1 - t).add(this.p1.scale(t)); }
	tangent()			{ return this.dir(); }
	evaluateWithDir(t: number) {
		return {pos: this.evaluate(t), dir: this.p1.sub(this.p0)};
	}
	lengthTo(t: number)	{ return t * this.len(); }
	closestPoint(p: T) {
		const   d = this.p1.sub(this.p0);
		const   u = Math.max(Math.min(p.sub(this.p0).dot(d) / d.lensq(), 1), 0);
		return this.p0.add(d.scale(u));
	}
	distanceToPoint(p: T): number {
		return this.closestPoint(p).sub(p).len();
	}
	matrix() {
		return {x: this.dir(), y: this.dir().perp(), z: this.p0};
	}
}

export class line2 extends line<float2> {
	intersection(b: line2, colinear = 1e-9) {
		const	d = this.dir().cross(b.dir());
		return Math.abs(d) < colinear ? mid(this.p0, b.p0) : this.evaluate(b.p0.sub(this.p0).cross(b.dir()) / d);
	}
}


//-----------------------------------------------------------------------------
//	circle
//-----------------------------------------------------------------------------

export class circle implements shape<float2> {
	constructor(public centre: float2, public radius: number) {}
	evaluate(t: number) {
		return this.centre.add(float2.cossin(t * Math.PI * 2).scale(this.radius));
	}
	tangent(t: number) {
		return float2.cossin((t + .25) * Math.PI * 2).scale(this.radius * Math.PI * 2);
	}
	evaluateWithDir(t: number) {
		const cs = float2.cossin(t * Math.PI * 2).scale(this.radius);
		return {pos: this.centre.add(cs), dir: cs.perp().scale(Math.PI * 2)};
	}
	lengthTo(t: number) {
		return t * Math.PI * 2 * this.radius;
	}
}

//-----------------------------------------------------------------------------
//	beziers
//-----------------------------------------------------------------------------

export class bezier2<T extends vops<T>> implements shape<T> {
	constructor(public c0: T, public c1: T, public c2: T) {}
	public evaluate(t: number) {
		return this.c0.scale((1 - t) * (1 - t)).add(this.c1.scale(2 * t * (1 - t))).add(this.c2.scale(t * t));
	}
	public tangent(t: number) {
		return this.c0.scale(2 * t - 2).add(this.c1.scale(2 - t * 4)).add(this.c2.scale(t * 2));
	}
	public evaluateWithDir(t: number) {
		return {
			pos: this.evaluate(t),
			dir: this.tangent(t)
		};
	}
	lengthTo(t: number) {
		return GaussLegendre(t, (u: number) => this.tangent(u).len(), weights5);
	/*
		// Compute intermediate vectors

		const c01 = this.c1.sub(this.c0);
		const c12 = this.c2.sub(this.c1);
		const A = c01.dot(c01);
		const B = c12.dot(c01);
		const C = Math.sqrt(A);

		return Math.abs(B) < 1e-10
			? t => t * this.c2.sub(this.c0).len()	// Simplified case: P1 is equidistant to P0 and P2 (B = 0)
			: t => {								// General case (closed-form solution)
				const sqrtTerm = Math.sqrt(B * B * t * t + 2 * A * B * t + A * A);
				return (A + B * t) * sqrtTerm / B + (A * C / B) * Math.log((B * (A + B * t + sqrtTerm)) / (A * C));
			};
			*/
	}
	split(t = 0.5) {
		const	p0	= lerp(this.c0,	this.c1,	t),
				p1	= lerp(this.c1,	this.c2,	t),
				cs	= lerp(p0,		p1,			t);
		return new bezierSpline2<T>([this.c0, p0, cs, p1, this.c2]);
	}
	slice(t0: number, t1: number) {
		const b = t0 ? this.split(t0)[1] : this;
		return t1 === 1 ? b : b.split((t1 - t0) / (1 - t0))[0];
	}
}

export class bezier3<T extends vops<T>> implements shape<T> {
	constructor(public c0: T, public c1: T, public c2: T, public c3: T) {}
	public evaluate(t: number) {
		const u  = 1 - t;
		const u2 = u * u;
		const t2 = t * t;
		return this.c0.scale(u2 * u).add(this.c1.scale(3 * t * u2)).add(this.c2.scale(3 * t2 * u)).add(this.c3.scale(t2 * t));
	}
	public tangent(t: number) {
		const u  = 1 - t;
		const u2 = u * u;
		const t2 = t * t;
		return this.c0.scale(-3 * u2).add(this.c1.scale(3 * (1 - 4 * t + 3 * t2))).add(this.c2.scale(3 * t * (2 - 3 * t))).add(this.c3.scale(3 * t2));
	}
	public evaluateWithDir(t: number) {
		return {
			pos: this.evaluate(t),
			dir: this.tangent(t)
		};
	}

	lengthTo(t: number): number {
		return GaussLegendre(t, (u: number) => this.tangent(u).len(), weights5);
	}
	split(t = 0.5) {
		const	h	= lerp(this.c1,	this.c2,	t),
				cl1 = lerp(this.c0,	this.c1,	t),
				cr2 = lerp(this.c2,	this.c3,	t),
				cl2 = lerp(cl1,		h,			t),
				cr1 = lerp(h,		cr2,		t),
				cs	= lerp(cl2,		cr1,		t);
		return new bezierSpline3([this.c0, cl1, cl2, cs, cr1, cr2, this.c3]);
	}
	slice(t0: number, t1: number) {
		const b = t0 ? this.split(t0)[1] : this;
		return t1 === 1 ? b : b.split((t1 - t0) / (1 - t0))[0];
	}

	spline()	{ 
		return Polynomial([
			this.c0,	//t
			this.c1.sub(this.c0).scale(3),	//t
			this.c0.sub(this.c2).scale(3).sub(this.c1.scale(6)),	//t^2
			this.c3.sub(this.c0).add(this.c1.sub(this.c2).scale(3))	//t^3
		]);
	}
}

//-----------------------------------------------------------------------------
//	splines (sequence of connected lines or beziers)
//-----------------------------------------------------------------------------

abstract class spline<T extends vops<T>, S extends shape<T>> implements shape<T> {
	[i: number]: S;

	constructor(public control: T[]) {
		return new Proxy(this, {
			get: (target, prop: string | symbol) => {
				const index = Number(prop);
				return !isNaN(index) ? target.get(index) : Reflect.get(target, prop);
			}
		});
	}

	abstract get length(): number;
	abstract get(index: number): S;

	evaluate(t: number) {
		const t1 = t * this.length;
		return this[Math.floor(t1)].evaluate(t1 % 1);
	}
	tangent(t: number) {
		const t1 = t * this.length;
		return this[Math.floor(t1)].tangent(t1 % 1);
	}
	evaluateWithDir(t: number) {
		const t1 = t * this.length;
		return this[Math.floor(t1)].evaluateWithDir(t1 % 1);
	}
	lengthTo(t: number) {
		const t1 = t * this.length;
		let i = 0, sum = 0;
		while (i + 1 < t1)
			sum += this[i++].lengthTo(1);
		return sum + this[i].lengthTo(t1 - i);
	}
}

export class polygon<T extends vops<T>> extends spline<T, line<T>> {
	public get length() {
		return this.control.length;
	}
	public get(i: number): line<T> {
		return new line<T>(this.control[i], this.control[(i + 1) % this.control.length]);
	}
	slice(t0: number, t1: number) {
		const p0 = this.evaluate(t0), p1 = this.evaluate(t1);
		const i0 = Math.floor(t0 * this.length);
		const i1 = Math.floor(t1 * this.length);
		return new bezierSpline2([p0, ...this.control.slice(i0 + 1, i1 + 1), p1]);
	}
}

export class bezierSpline2<T extends vops<T>> extends spline<T, bezier2<T>> {
	get length() {
		return (this.control.length - 1) / 2;
	}
	get(i: number): bezier2<T> {
		return new bezier2(this.control[i * 2], this.control[i * 2 + 1], this.control[i * 2 + 2]);
	}
	slice(t0: number, t1: number) {
		const t0a = t0 * this.length, i0 = Math.floor(t0a);
		const t1a = t1 * this.length, i1 = Math.floor(t1a);
		if (i0 === i1) {
			const b0 = this[i0].slice(t0a - i0, t1a - i0);
			return new bezierSpline2([b0.c0, b0.c1, b0.c2]);
		}
		const b0 = this[i0].slice(t0a - i0, 1);
		const b1 = this[i1].slice(0, t1a - i1);
		return new bezierSpline2([b0.c0, b0.c1, ...this.control.slice((i0 + 1) * 2, i1 * 2), b1.c0, b1.c1, b1.c2]);
	}
}

export class bezierSpline3<T extends vops<T>> extends spline<T, bezier3<T>> {
	get length() {
		return (this.control.length - 1) / 3;
	}
	get(i: number): bezier3<T> {
		return new bezier3(this.control[i * 3], this.control[i * 3 + 1], this.control[i * 3 + 2], this.control[i * 3 + 3]);
	}
	slice(t0: number, t1: number) {
		const t0a = t0 * this.length, i0 = Math.floor(t0a);
		const t1a = t1 * this.length, i1 = Math.floor(t1a);
		if (i0 === i1) {
			const b0 = this[i0].slice(t0a - i0, t1a - i0);
			return new bezierSpline3([b0.c0, b0.c1, b0.c2, b0.c3]);
		}
		const b0 = this[i0].slice(t0a - i0, 1);
		const b1 = this[i1].slice(0, t1a - i1);
		return new bezierSpline3([b0.c0, b0.c1, b0.c2, ...this.control.slice((i0 + 1) * 3, i1 * 3), b1.c0, b1.c1, b1.c2, b1.c3]);
	}
}

//-----------------------------------------------------------------------------
//	reduce spline<bezier3> to spline<bezier2>
//-----------------------------------------------------------------------------

function curvature_from_tangent(t: Polynomial<float2>) {
	const	t2	= t.deriv();
	const	p	= t.map(c => c.x).mul(t2.map(c => c.y)).sub(t2.map(c => c.x).mul(t.map(c => c.y)));
	return Polynomial(p.c.slice(0, t.c.length * 2 - 1));
}

export function reduce_spline(b: bezier3<float2>, max: number, tol: number) : bezierSpline2<float2> {
	const	p: float2[] = [];
	const	tol2	= tol * tol;

	function simple(b3: bezier3<float2>, max: number) {
		let c1: float2;
	
		if (approx_equal(b3.c3, b3.c1)) {
			c1 = b3.c2;
		} else if (approx_equal(b3.c2, b3.c3)) {
			c1 = b3.c1;
		} else {
			const	t0	= new line2(b3.c0, b3.c1);
			const	t1	= new line2(b3.c3, b3.c2);
			//c1	= colinear(t0.dir(), t1.dir()) ? mid(b3.c0, b3.c3) : t0.evaluate(intersectionT2(t0, t1));
			c1	= t0.intersection(t1);
		}
		
		if (p.length + 2 * 2 < max) {
			const 	b2 		= new bezier2<float2>(b3.c0, c1, b3.c3);
			const	p2		= b2.evaluate(0.5);
			const	s0		= b3.spline().sub(p2);
			const	s1		= s0.map(c => c.lensq());
			const	s		= s1.deriv();
			const	root	= refine_roots(s, [0.5], 2);
			const	p3		= b3.evaluate(root[0]);
	
			if (p3.sub(p2).lensq() > tol2) {
				const	c	= b3.split(0.5);
				simple(c[0], (max - p.length) / 2);
				simple(c[1], max);
				return;
			}
		}
		p.push(c1);
		p.push(b3.c3);
	}
	
	function checked(b3: bezier3<float2>, max: number) {
		if (p.length + 2 * 2 < max) {
			const	t0	= b3.tangent(0);
			const	s	= b3.spline().deriv();
			const	s2	= s.map(c => c.dot(t0));
			const	roots = s2.realRoots();
			if (roots.length) {
				const	root = roots.length > 1 && roots[0] < 0 ? roots[1] : roots[0];
				if (root > 0 && root < 0.9) {
					const	c	= b3.split(root);
					simple(c[0], (max - p.length) / 2);
					checked(c[1], max);
					return;
				}
			}
		}
		simple(b3, max);
	}

	// find inflection points
	const	inflection = curvature_from_tangent(b.spline().deriv()).realRoots();

	if (inflection[0] > 0.05 && inflection[0] < 0.95) {
		let	c = b.split(inflection[0]);

		if (inflection[1] < 0.95) {
			checked(c[0], max / 3);
			c	= c[1].split((inflection[1] - inflection[0]) / (1 - inflection[0]));
			checked(c[0], (max - p.length) / 2);
			checked(c[1], max);

		} else {
			checked(c[0], max / 2);
			checked(c[1], max);
		}

	} else if (inflection[1] > 0.05 && inflection[1] < 0.95) {
		const	c = b.split(inflection[1]);
		checked(c[0], max / 2);
		checked(c[1], max);
	} else {
		checked(b, max);
	}
	return new bezierSpline2(p);
}
