
export interface ops<C extends ops<C>> {
	neg(): 				C;
	scale(b: number):	C;
	mul(b: C):			C;
	div(b: C):			C;
	add(b: C): 			C;
	sub(b: C): 			C;
	mag():				number | scalar2<any>;
}

export interface scalar<C extends scalar<C>> extends ops<C> {
	sign():				number;
	abs():				C;
	recip():			C;
	divmod(b: C):		number | bigint;
	lt(b: C):			boolean;
	from(n: number | bigint):	C;
}

export interface scalar2<C extends scalar2<C>> extends scalar<C> {
	sqrt(): 			C;
	pow(n: number):		C;
}

export function sign(a: number) {
	return a > 0 ? 1 : a < 0 ? -1 : 0;
}

export function sincos(angle: number) {
	return {s: Math.sin(angle), c: Math.cos(angle)};
}
export function sin2cos(x: number) {
	 return Math.sqrt((1 - x) * (1 + x));
}

//-----------------------------------------------------------------------------
// number
//-----------------------------------------------------------------------------

export function gcd(a: number, b: number): number {
	while (b)
		[a, b] = [b, a % b];
	return a;
}

export class scalarN implements scalar2<scalarN> {
	constructor(public value: number) {}
	neg(): 				scalarN		{ return new scalarN(-this.value); }
	scale(b: number):	scalarN		{ return new scalarN(this.value * b); }
	mul(b: scalarN):	scalarN		{ return new scalarN(this.value * b.value); }
	div(b: scalarN):	scalarN		{ return new scalarN(this.value / b.value); }
	add(b: scalarN):	scalarN		{ return new scalarN(this.value + b.value); }
	sub(b: scalarN):	scalarN		{ return new scalarN(this.value - b.value); }
	mag():				number 		{ return Math.abs(this.value); }
	sign():				number		{ return sign(this.value); }
	abs():				scalarN		{ return new scalarN(Math.abs(this.value)); }
	recip():			scalarN		{ return new scalarN(1 / this.value); }
	divmod(b: scalarN):	number		{ const q = Math.floor(this.value / b.value); this.value -= q * b.value; return q; }
	pow(n: number):		scalarN		{ return new scalarN(this.value ** n); }
	lt(b: scalarN):		boolean		{ return this.value < b.value; }
	sqrt(): 			scalarN		{ return new scalarN(Math.sqrt(this.value)); }
	from(n: number | bigint)		{ return new scalarN(Number(n)); }

	valueOf():			number		{ return this.value; }
}

export function isScalar(x: ops<any>): x is scalar<any> {
	return 'sign' in x;
}
export function asScalar(x: number|scalar<any>): scalar<any> {
	if (typeof x === 'number')
		return new scalarN(x);
	return x;
}
export function asScalar2(x: number|scalar2<any>): scalar2<any> {
	if (typeof x === 'number')
		return new scalarN(x);
	return x;
}
export function asScalarT<T extends scalar<T>>(from: T, x: number|T): T {
	if (typeof x === 'number')
		return from.from(x);
	if (x instanceof from.constructor)
		return x;
	return from.from(+x);
}


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

//-----------------------------------------------------------------------------
// bigint
//-----------------------------------------------------------------------------

export function signB(a: bigint): number {
	return a === 0n ? 0 : a < 0n ? -1 : 1;
}
export function absB(a: bigint): bigint {
	return a < 0n ? -a : a;
}
export function maxB(a: bigint, b: bigint): bigint {
	return a < b ? b : a;
}
export function minB(a: bigint, b: bigint): bigint {
	return a < b ? a : b;
}

export function gcdB(a: bigint, b: bigint): bigint {
	while (b)
		[a, b] = [b, a % b];
	return a;
}

export class extentB {
	static fromCentreExtent(centre: bigint, size: bigint) {
		const half = size / 2n;
		return new extentB(centre - half, centre + half);
	}
	static from<U extends Iterable<bigint>>(items: U) {
		let ext;// = new extentT<T>;
		for (const i of items) {
			if (!ext)
				ext = new extentB(i, i);
			else
				ext.add(i);
		}
		return ext;
	}
	constructor(
		public min: bigint,
		public max: bigint
	) {}
	extent() {
		return this.max - this.min;
	}
	centre() {
		return (this.min + this.max) / 2n;
	}
	add(p: bigint) {
		this.min = minB(this.min, p);
		this.max = maxB(this.max, p);
	}
	combine(b: extentB) {
		this.min = minB(this.min, b.min);
		this.max = maxB(this.max, b.max);
	}
	encompasses(b: extentB) {
		return this.min <= b.min && this.max >= b.max;
	}
	overlaps(b: extentB) {
		return this.min <= b.max && this.max >= b.min;
	}
	contains(p: bigint) {
		return this.min <= p && this.max >= p;
	}
	clamp(p: bigint) {
		return minB(maxB(p, this.min), this.max);
	}
}

//-----------------------------------------------------------------------------
// Generics
//-----------------------------------------------------------------------------

export function maxT<T extends scalar<T>>(a: T, b: T) {
	return a.lt(b) ? b : a;
}
export function minT<T extends scalar<T>>(a: T, b: T) {
	return a.lt(b) ? a : b;
}

export function gcdT<T extends scalar<T>>(a: T, b: T): T {
	a = a.abs();
	b = b.abs();

	while (b) {
		a.divmod(b);
		[a, b] = [b, a];
	}
	return a;
}

export class extentT<T extends scalar<T>> {
	static fromCentreExtent<T extends scalar<T>>(centre: T, size: T) {
		const half = size.scale(0.5);
		return new extentT(centre.sub(half), centre.add(half));
	}
	static from<T extends scalar<T>, U extends Iterable<T>>(items: U) {
		let ext;// = new extentT<T>;
		for (const i of items) {
			if (!ext)
				ext = new extentT(i, i);
			else
				ext.add(i);
		}
		return ext;
	}
	constructor(
		public min:T,
		public max:T
	) {}
	extent() {
		return this.max.sub(this.min);
	}
	centre() {
		return this.min.add(this.max).scale(0.5);
	}
	add(p: T) {
		this.min = minT(this.min, p);
		this.max = maxT(this.max, p);
	}
	combine(b: extentT<T>) {
		this.min = minT(this.min, b.min);
		this.max = maxT(this.max, b.max);
	}
	encompasses(b: extentT<T>) {
		return !b.min.lt(this.min) && !this.max.lt(b.max);
	}
	overlaps(b: extentT<T>) {
		return !b.max.lt(this.min) && !this.max.lt(b.min);
	}
	contains(p: T) {
		return !p.lt(this.min) && !this.max.lt(p);
	}
	clamp(p: T) {
		return minT(maxT(p, this.min), this.max);
	}
}
