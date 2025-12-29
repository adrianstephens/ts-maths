import {ops, hasop} from './core';

interface WithZero<T> {
	zero(): T;
}

//-----------------------------------------------------------------------------
// Statistics
//-----------------------------------------------------------------------------

export class statistics1 {
	sum		= 0;
	sum2	= 0;
	min		= 0;
	max		= 0;
	count	= 0;

	static from<U extends Iterable<number>>(items: U) {
		const stats = new statistics1;
		for (const i of items)
			stats.add(i);
		return stats;
	}

	add(v: number) {
		if (!this.count) {
			this.min = v;
			this.max = v;
		} else {
			this.min = Math.min(this.min, v);
			this.max = Math.max(this.max, v);
		}
		this.sum += v;
		this.sum2 += v * v;
		this.count++;
		return this;
	}
	get mean() {
		return this.sum / this.count;
	}
	get variance() {
		const mean = this.mean;
		return this.sum2 / this.count - mean * mean;
	}
	get standardDeviation() {
		return Math.sqrt(this.variance);
	}
}

type statsOps<T> = ops<T> & hasop<'min'|'max'>

export class statistics<T extends statsOps<T>> {
	sum:	T;
	sum2:	T;
	min:	T;
	max:	T;
	count	= 0;

	static from<T extends statsOps<T>, U extends Iterable<T>>(items: U, Type?: WithZero<T>) {
		if (!Type) {
			const first = items[Symbol.iterator]().next();
			if (first.done)
				throw new Error("Cannot create statistics from empty iterable");
			Type = first.value.constructor as (new (...args: any[]) => T) & WithZero<T>;
		}
		const stats = new statistics<T>(Type);
		for (const item of items)
			stats.add(item);
		return stats;
	}

	constructor(public Type: WithZero<T>) {
		this.sum	= Type.zero();
		this.sum2	= Type.zero();
		this.min	= Type.zero();
		this.max	= Type.zero();
	}

	add(v: T) {
		if (!this.count) {
			this.min = v;
			this.max = v;
		} else {
			this.min = this.min.min(v);
			this.max = this.max.max(v);
		}
		this.sum	= this.sum.add(v);
		this.sum2	= this.sum2.add(v.mul(v));
		this.count++;
		return this;
	}
	get mean() {
		return this.sum.scale(1 / this.count);
	}
	get variance() {
		const mean = this.mean;
		return this.sum2.scale(1 / this.count).sub(mean.mul(mean));
	}
	//standardDeviation() {
	//	return this.variance.sqrt();
	//}
}

//-----------------------------------------------------------------------------
// colour
//-----------------------------------------------------------------------------

export interface rgb {
	r: number,
	g: number,
	b: number,
}
export interface hsv {
	h: number,
	s: number,
	v: number,
}

function rgb_to_hsv(r: number, g: number, b: number): hsv {
	const v = Math.max(Math.max(r, g), b);
	const c = v - Math.min(Math.min(r, g), b);
	return {
		h: c == 0 ? 0 : ((v == r ? g - b : v == g ? b - r : r - g) / c + (v == r ? 0 : v == g ? 2 : 4)) / 6,
		s: v == 0 ? 0 : c / v,
		v
	};
}

function hsv_to_rgb(h: number, s: number, v: number): rgb {
	const	c = v * s;
	const	m = v - c;
	let		x = v, y = v;

	const	i = Math.floor(h * 3);
	const	f = h * 3 - i;
	if (f >= 0.5)
		x -= c * (f - 0.5);
	else
		y -= c * (0.5 - f);

	switch (i) {
		case 0: return { r: x, g: y, b: m };
		case 1: return { r: m, g: x, b: y };
		case 2: return { r: y, g: m, b: x };
		default: return { r: 0, g: 0, b: 0 };
	}
}

export class colour implements rgb {
	static fromHSV(h: number, s: number, v: number)	{
		const rgb = hsv_to_rgb(h, s, v);
		return new colour(rgb.r, rgb.g, rgb.b);
	}
	static fromRGB(r: number, g: number, b: number)	{
		return new colour(r, g, b);
	}
	constructor(public r: number, public g: number, public b: number) {}
	rgb()	{ return this; }
	hsv()	{ return rgb_to_hsv(this.r, this.g, this.b); }
}