import {ops, float2, normalise, approx_equal, mid, lerp} from './vector.js';
import {polynomial, polynomialT} from './polynomial.js';

function NewtonRaphson(F: (t: number) => number, F_prime: (t: number) => number, t: number, maxIter = 20, tol = 1e-6) {
	for (let i = 0; i < maxIter; i++) {
		const f			= F(t);
		const f_prime	= F_prime(t);
		const delta		= f / f_prime;
		t -= delta;
        if (Math.abs(delta) < tol)
			break;
	}
	return t;
}

function GaussLegendre(t: number, F: (t: number) => number, weights: {node: number, weight: number}[]) {
	return weights.reduce((sum, i) => sum + i.weight * F((t / 2) * (i.node + 1)), 0) * t / 2;
}

export class plane<T extends ops<T>> {
	static fromVert<T  extends ops<T>>(n: T, p: T) {
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

export function colinear<C extends ops<C>>(p1: C, p2: C, epsilon = 1e-9) {
	return p1.dot(p2) > (p1.len() * p2.len()) * (1 - epsilon);
}

//-----------------------------------------------------------------------------
//	shape
//-----------------------------------------------------------------------------

export interface shape<T extends ops<T>> {
	evaluate(t: number): T;
	tangent(t: number): T;
	evaluateWithDir(t: number) : {pos: T, dir: T};
	lengthTo(t: number): number;
}

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

export function offsetShape<T extends ops<T>, S extends shape<T>>(baseShape: S, offset0: number, offset1: number): S & {getT(t: number): number} {
	const scale = offset1 - offset0;

	const offsetShape = Object.create(baseShape);
	offsetShape.getT		= (t: number) => t * scale + offset0;
	offsetShape.evaluate	= (t: number) => baseShape.evaluate(offsetShape.getT(t));
	offsetShape.tangent		= (t: number) => baseShape.tangent(offsetShape.getT(t)).mul(scale);
	offsetShape.evaluateWithDir = (t: number) => {
		const result = baseShape.evaluateWithDir(offsetShape.getT(t));
		return {
			pos: result.pos,
			dir: result.dir.mul(scale)
		};
	};
	offsetShape.length		= (t: number) => baseShape.lengthTo(offsetShape.getT(t));

	return offsetShape;
}

export function distanceToPoint<T extends ops<T>>(b: shape<T>, p: T, steps = 10): number {
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

export function distanceToT<T extends ops<T>>(b: shape<T>, distance: number, steps = 10, tol = 1e-6): number {
	//const length = b.lengthTo;
    //const S = length(1);
    const S = b.lengthTo(1);
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

export class line<T extends ops<T>> implements shape<T> {
	constructor(public p0: T, public p1: T) {}

	dir() 				{ return this.p1.sub(this.p0); }
	len() 				{ return this.dir().len(); }
	evaluate(t: number) { return this.p0.mul(1 - t).add(this.p1.mul(t)); }
	tangent()			{ return this.dir(); }
	evaluateWithDir(t: number) {
		return {pos: this.evaluate(t), dir: this.p1.sub(this.p0)};
	}
	lengthTo(t: number)	{ return t * this.len(); }
	closestPoint(p: T) {
		const   d = this.p1.sub(this.p0);
		const   u = Math.max(Math.min(p.sub(this.p0).dot(d) / d.lensq(), 1), 0);
		return this.p0.add(d.mul(u));
	}
	distanceToPoint(p: T): number {
		return this.closestPoint(p).sub(p).len();
	}
}

export class line2 extends line<float2> {}

function intersectionT2(a: line2, b: line2, colinear = 1e-9) {
	const	d = a.dir().cross(b.dir());
	return d < colinear ? 0.5 : b.p0.sub(a.p0).cross(b.dir()) / d;
}

//-----------------------------------------------------------------------------
//	circle
//-----------------------------------------------------------------------------

export class circle implements shape<float2> {
	constructor(public centre: float2, public radius: number) {}
	evaluate(t: number) {
		return this.centre.add(float2.cossin(t * Math.PI * 2).mul(this.radius));
	}
	tangent(t: number) {
		return float2.cossin((t + .25) * Math.PI * 2).mul(this.radius * Math.PI * 2);
	}
	evaluateWithDir(t: number) {
		const cs = float2.cossin(t * Math.PI * 2).mul(this.radius);
		return {pos: this.centre.add(cs), dir: cs.perp().mul(Math.PI * 2)};
	}
	lengthTo(t: number) {
		return t * Math.PI * 2 * this.radius;
	}
}

//export function triangle(a: float2, b: float2, c: float2) {
//	return float2x3(b.sub(a), c.sub(a), a);
//}

//-----------------------------------------------------------------------------
//	beziers
//-----------------------------------------------------------------------------

export class bezier2<T extends ops<T>> implements shape<T> {
	constructor(public c0: T, public c1: T, public c2: T) {}
	public evaluate(t: number) {
		return this.c0.mul((1 - t) * (1 - t)).add(this.c1.mul(2 * t * (1 - t))).add(this.c2.mul(t * t));
	}
	public tangent(t: number) {
		return this.c0.mul(2 * t - 2).add(this.c1.mul(2 - t * 4)).add(this.c2.mul(t * 2));
	}
	public evaluateWithDir(t: number) {
		return {
			pos: this.evaluate(t),
			dir: this.tangent(t)
		};
	}
	split(t = 0.5) {
		const	p0	= lerp(this.c0,	this.c1,	t),
				p1	= lerp(this.c1,	this.c2,	t),
				cs	= lerp(p0,		p1,			t);
		return new bezierSpline2<T>([this.c0, p0, cs, p1, this.c2]);
	}

	get lengthTo(): (t:number)=>number {
		// Compute intermediate vectors
		const c01 = this.c1.sub(this.c0);
		const c12 = this.c2.sub(this.c1);
		const A = c01.dot(c01);
		const B = c12.dot(c01);
		const C = Math.sqrt(A);

		return Math.abs(B) < 1e-10
			// Simplified case: P1 is equidistant to P0 and P2 (B = 0)
			? t => t * this.c2.sub(this.c0).len()
			// General case (closed-form solution)
			: t => {
				const sqrtTerm = Math.sqrt(B * B * t * t + 2 * A * B * t + A * A);
				return (A + B * t) * sqrtTerm / B + (A * C / B) * Math.log((B * (A + B * t + sqrtTerm)) / (A * C));
			};
	}

}

export class bezier3<T extends ops<T>> implements shape<T> {
	constructor(public c0: T, public c1: T, public c2: T, public c3: T) {}
	public evaluate(t: number) {
		const u  = 1 - t;
		const u2 = u * u;
		const t2 = t * t;
		return this.c0.mul(u2 * u).add(this.c1.mul(3 * t * u2)).add(this.c2.mul(3 * t2 * u)).add(this.c3.mul(t2 * t));
	}
	public tangent(t: number) {
		const u  = 1 - t;
		const t2 = t * t;
		const u2 = u * u;
		return this.c0.mul(-3 * u2).add(this.c1.mul(3 * (1 - 4 * t + 3 * t2))).add(this.c2.mul(3 * t * (2 - 3 * t))).add(this.c3.mul(3 * t2));
	}
	public evaluateWithDir(t: number) {
		return {
			pos: this.evaluate(t),
			dir: this.tangent(t)
		};
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

	spline()	{ 
		return new polynomialT<T>([
			this.c0,	//t
			this.c1.sub(this.c0).mul(3),	//t
			this.c0.sub(this.c2).mul(3).sub(this.c1.mul(6)),	//t^2
			this.c3.sub(this.c0).add(this.c1.sub(this.c2).mul(3))	//t^3
		]);
	}

	lengthTo(t: number): number {
		return GaussLegendre(t,
			(u: number) => this.tangent(u).len(),
			[{node: -0.90618, weight: 0.236927}, {node: -0.538469, weight: 0.478629}, {node: 0, weight: 0.568889}, {node: 0.538469, weight: 0.478629}, {node: 0.90618, weight: 0.236927}]
		);
	}
}


//-----------------------------------------------------------------------------
//	splines (sequence of connected shapes)
//-----------------------------------------------------------------------------

abstract class spline<T extends ops<T>, S extends shape<T>> implements shape<T> {
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

export class polygon<T extends ops<T>> extends spline<T, line<T>> {
	public get length() {
		return this.control.length;
	}
    public get(index: number): line<T> {
        return new line<T>(this.control[index], this.control[(index + 1) % this.control.length]);
    }
}

export class bezierSpline2<T extends ops<T>> extends spline<T, bezier2<T>> {
	public get length() {
		return (this.control.length - 1) / 2;
	}
    public get(index: number): bezier2<T> {
        return new bezier2(this.control[index * 2], this.control[index * 2 + 1], this.control[index * 2 + 2]);
    }
}

export class bezierSpline3<T extends ops<T>> extends spline<T, bezier3<T>> {
	public get length() {
		return (this.control.length - 1) / 3;
	}
    public get(index: number): bezier3<T> {
        return new bezier3(this.control[index * 3], this.control[index * 3 + 1], this.control[index * 3 + 2], this.control[index * 3 + 3]);
    }
}
function reduce_simple(b3: bezier3<float2>, p: float2[], max: number, tol2: number) {
	let c1: float2;

	if (approx_equal(b3.c3, b3.c1)) {
		c1 = b3.c2;
	} else if (approx_equal(b3.c2, b3.c3)) {
		c1 = b3.c1;
	} else {
		const	t0	= new line2(b3.c0, b3.c1);
		const	t1	= new line2(b3.c3, b3.c2);
		c1	= colinear(t0.dir(), t1.dir()) ? mid(b3.c0, b3.c3) : t0.evaluate(intersectionT2(t0, t1));
	}

	const b2 = new bezier2<float2>(b3.c0, c1, b3.c3);

	if (p.length + 2 * 2 < max) {
		//auto	p3		= b3.evaluate(half);
		const	p2		= b2.evaluate(0.5);
		const	s0		= b3.spline().sub(p2);
		const	s		= s0.map(c => c.lensq()).deriv();
		const	root	= s.refine_roots([0.5], 2);
		const	p3		= b3.evaluate(root[0]);

		if (p3.sub(p2).lensq() > tol2) {
			const	c	= b3.split(0.5);
			reduce_simple(c[0], p, (max - p.length) / 2, tol2);
			reduce_simple(c[1], p, max, tol2);
			return;
		}
	}
	p.push(b2.c1);
	p.push(b2.c2);
	return p;
}

function reduce_check(b3: bezier3<float2>, p: float2[], max: number, tol2: number) {
	if (p.length + 2 * 2 < max) {
		const	t0	= b3.tangent(0);
		const	s	= b3.spline().deriv();
		const	s2	= s.map(c => c.dot(t0));
		const	roots = s2.roots();
		if (roots.length) {
			const	root = roots.length > 1 && roots[0] < 0 ? roots[1] : roots[0];
			if (root > 0 && root < 0.9) {
				const	c	= b3.split(root);
				reduce_simple(c[0], p, (max - p.length) / 2, tol2);
				reduce_check(c[1], p, max, tol2);
				return;
			}
		}
	}
	return reduce_simple(b3, p, max, tol2);
}

function curvature_from_tangent(t: polynomialT<float2>) {
	const	t2	= t.deriv();
	const	p	= t.map(c => c.x).mul(t2.map(c => c.y)).sub(t2.map(c => c.x).mul(t.map(c => c.y)));
	return new polynomial(p.c.slice(0, t.c.length * 2 - 1));
}

export function reduce_spline(b: bezier3<float2>, max: number, tol: number) {
	const	p: float2[] = [];
	const	tol2	= tol * tol;

	// find inflection points
	const	inflection = curvature_from_tangent(b.spline().deriv()).roots();

	if (inflection[0] > 0.05 && inflection[0] < 0.95) {
		let	c = b.split(inflection[0]);
		const b0 = c[0];
		const b1 = c[1];

		if (inflection[1] < 0.95) {
			reduce_check(b0, p, max / 3, tol2);
			c	= b1.split((inflection[1] - inflection[0]) / (1 - inflection[0]));
			reduce_check(c[0], p, (max - p.length) / 2, tol2);
			reduce_check(c[1], p, max, tol2);

		} else {
			reduce_check(b0, p, max / 2, tol2);
			reduce_check(b1, p, max, tol2);
		}

	} else if (inflection[1] > 0.05 && inflection[1] < 0.95) {
		const	c = b.split(inflection[1]);
		reduce_check(c[0], p, max / 2, tol2);
		reduce_check(c[1], p, max, tol2);
	} else {
		reduce_check(b, p, max, tol2);
	}
}
