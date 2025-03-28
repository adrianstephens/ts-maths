import {
	float2, ops,
} from './vector';

export class line<T extends ops<T>> {
	constructor(public p0: T, public p1: T) {}

	public evaluate(t: number) {
		return this.p0.mul(1 - t).add(this.p1.mul(t));
	}
	public closestPoint(p: T) {
		const   d = this.p1.sub(this.p0);
		const   u = Math.max(Math.min(p.sub(this.p0).dot(d) / d.lensq(), 1), 0);
		return this.p0.add(d.mul(u));
	}
	public distanceToPoint(p: T): number {
		return this.closestPoint(p).sub(p).len();
	}
}

export class line2 extends line<float2> {}

export interface shape<T extends ops<T>> {
	evaluate(t: number): T;
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

export class bezier2<T extends ops<T>> {
	constructor(public c0: T, public c1: T, public c2: T) {}
	public evaluate(t: number) {
		const w0 = (1 - t) * (1 - t);
		const w1 = 2 * t * (1 - t);
		const w2 = t * t;
		return this.c0.mul(w0).add(this.c1.mul(w1)).add(this.c2.mul(w2));
	}
}

export class bezier3<T extends ops<T>> {
	constructor(public c0: T, public c1: T, public c2: T, public c3: T) {}
	public evaluate(t: number) {
		const u  = 1 - t;
		const u2 = u * u;
		const w0 = u2 * u;
		const w1 = 3 * t * u2;
		const w2 = 3 * t * t * u;
		const w3 = t * t * t;
		return this.c0.mul(w0).add(this.c1.mul(w1)).add(this.c2.mul(w2)).add(this.c3.mul(w3));
	}
}