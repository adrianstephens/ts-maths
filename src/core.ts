
export abstract class ops<C extends ops<C>> {
	abstract neg(): 			C;
	abstract scale(b: number):	C;
	abstract mul(b: C):			C;
	abstract div(b: C):			C;
	abstract add(b: C): 		C;
	abstract sub(b: C): 		C;
	abstract mag():             number | scalar<any>;
}

export abstract class scalar<C extends scalar<C>> extends ops<C> {
	abstract abs():				C;
	abstract pow(n: number):	C;
	abstract gt(b: C):			boolean;
	abstract sqrt(): 			C;
	abstract valueOf():			number;
	static from(_n: number):	scalar<any> { throw new Error("abstract"); }
}

export function	from<C extends scalar<C>>(c: C): (n: number) => C {
	return (n: number) => (c.constructor as typeof scalar).from(n) as C;
}

export function sincos(angle: number) {
	return {s: Math.sin(angle), c: Math.cos(angle)};
}
export function sin2cos(x: number) {
	 return Math.sqrt((1 - x) * (1 + x));
}

//-----------------------------------------------------------------------------
// 1D
//-----------------------------------------------------------------------------

export class extent1 {
	static fromCentreExtent(centre: number, size: number) {
		const half = size * 0.5;
		return new extent1(centre - half, centre + half);
	}
	static from<U extends Iterable<number>>(items: U) {
		const ext = new extent1;
		for (const i of items)
			ext.add(i);
		return ext;
	}
	constructor(
		public min	= Infinity,
		public max	= -Infinity
	) {}
	extent() {
		return this.max - this.min;
	}
	centre() {
		return (this.min + this.max) * 0.5;
	}
	add(p: number) {
		this.min = Math.min(this.min, p);
		this.max = Math.max(this.max, p);
	}
	combine(b: extent1) {
		this.min = Math.min(this.min, b.min);
		this.max = Math.max(this.max, b.max);
	}
	encompasses(b: extent1) {
		return this.min <= b.min && this.max >= b.max;
	}
	overlaps(b: extent1) {
		return this.min <= b.max && this.max >= b.min;
	}
	contains(p: number) {
		return this.min <= p && this.max >= p;
	}
	clamp(p: number) {
		return Math.min(Math.max(p, this.min), this.max);
	}
}
