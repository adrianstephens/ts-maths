import {Operators, ops1, scalar, scalarExt, has, has0, hasop, divmodto, Immutable, hasProperty} from "./core";

//-----------------------------------------------------------------------------
// Generics
//-----------------------------------------------------------------------------

class extentT<T extends has<'lt'>> {
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
		this.min = gen.min(this.min, p);
		this.max = gen.max(this.max, p);
	}
	combine(b: extentT<T>) {
		this.min = gen.min(this.min, b.min);
		this.max = gen.max(this.max, b.max);
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
		return gen.min(gen.max(p, this.min), this.max);
	}
}

export type canDenominator = Pick<scalarExt<any>, 'from'|'recip'|'lt'> & divmodto<any>;

export type GenTypes = number | bigint | ops1<any, any>;

export const gen = {
	dup<T extends GenTypes>(a: T) 		{ return typeof a === 'object' ? a.dup() : a; },
	neg<T extends GenTypes>(a: T) 		{ return typeof a === 'object' ? a.neg() : -a; },
	add<T extends GenTypes>(a: T, b: T)	{ return typeof a === 'object' ? a.add(b) : (a as number) + (b as number); },
	sub<T extends GenTypes>(a: T, b: T)	{ return typeof a === 'object' ? a.sub(b) : (a as number) - (b as number); },
	mul<T extends GenTypes>(a: T, b: T)	{ return typeof a === 'object' ? a.mul(b) : (a as number) * (b as number); },
	div<T extends GenTypes>(a: T, b: T)	{ return typeof a === 'object' ? a.div(b) : (a as number) / (b as number); },

	ipow<T extends has0<'mul'>>(base: T, exp: number, one?: T): T {
		let result = exp & 1 ? base : one;
		for (exp >>= 1; exp; exp >>= 1) {
			base = base.mul(base);
			if (exp & 1)
				result = result ? result.mul(base) : base;
		}
		return result!;
	},

	eq<T extends has0<'eq'>>(a: T, b: T) { return a.eq(b); },
	lt<T extends has0<'lt'>>(a: T, b: T) { return a.lt(b); },

	copySign<T extends scalar<T>>(a: T, b: T) {
		return b.sign() < 0 ? a.abs().neg() : a.abs();
	},

	max<T extends has0<'lt'>>(...values: T[]) {
		return values.reduce((a, b) => a.lt(b) ? b : a);
	},
	min<T extends has0<'lt'>>(...values: T[]) {
		return values.reduce((a, b) => a.lt(b) ? a : b);
	},

	compare<T extends has0<'lt'>>(a: T, b: T): number {
		return a.lt(b) ? -1 : b.lt(a) ? 1 : 0;
	},

	gcd<T extends Pick<scalarExt<any>, 'sign'|'abs'|'dup'> & divmodto<any>>(...values: T[]): T {
		let a: T | undefined;
		for (let b of values) {
			b = b.dup().abs();
			if (a && a.sign() !== 0) {
				while (b.sign() !== 0) {
					a.divmod(b);
					[a, b] = [b, a];
				}
			} else {
				a = b;
			}
		}
		return a!;
	},

	extendedGcd<T extends has<'scale'|'sign'|'abs'|'dup'> & divmodto<any>>(a: T, b: T, one: T) {
		let s0 = one, s = one.scale(0), t0 = s, t = one;
		while (b.sign() !== 0) {
			const q	= a.divmod(b);
			[a, b, s0, t0, s, t] = [b, a, s, t, s0.sub(s.mul(q)), t0.sub(t.mul(q))];
		}
		// x * a + y * b = g
		return { g: a, x: s0, y: t0 };
	},


	lcm<T extends Pick<scalarExt<any>, 'divmod'|'sign'|'dup'|'abs'|'scale'>>(...values: T[]) {
		let a: T | undefined;
		for (const b of values)
			a = a ? b.scale(Number(a.divmod(gen.gcd(a, b)))) : b;
		return a!;
	},

	denominator<T extends canDenominator>(x: T, maxDen: bigint, eps?: T): bigint {
		const one = x.from(1);
		x.divmod(one);
		if (eps && x.lt(eps))
			return 1n;

		let k1 = 1n, k2 = 0n;
		do {
			x = x.recip();
			[k2, k1] = [k1, BigInt(x.divmod(one)) * k1 + k2];
		} while ((!eps || eps.lt(x)) && k1 < maxDen);
		return k1;
	},

	commonDenominator<T extends canDenominator & has0<'scale'>>(numbers: T[], maxDen = 1000n, eps?: T) {
		let scale = 1n;
		for (const n of numbers) {
			scale *= gen.denominator(n.scale(Number(scale)), maxDen, eps);
			if (scale > maxDen)
				return 0;
		}
		return scale;
	},

	rationalApprox<T extends canDenominator & has0<'dup'>>(x: Immutable<T>, maxDen: bigint, eps?: T): [bigint, bigint] {
//		const den = this.denominator(x.dup(), maxDen, eps);
//		return [BigInt(x.dup().divmod(x.from(den))), den];
		const one = x.from(1);
		let b = x.dup();
		let h1 = BigInt(b.divmod(one)), h2 = 1n;
		let k1 = 1n, k2 = 0n;

		if (b.sign() < 0) {
			--h1;
			b = b.add(one);
		}

		while (b.sign() !== 0 && k1 < maxDen && (!eps || eps.lt(b.abs()))) {
			b = b.recip();
			const f = BigInt(b.divmod(one));
			[h2, h1, k2, k1] = [h1, h1 * f + h2, k1, k1 * f + k2];
		}
		return [h1, k1];
	},
	
	continuedFractionT<T extends canDenominator & has0<'dup'|'sign'|'abs'|'add'>>(x: T, maxTerms = 64, eps?: T): (bigint|number)[] {
		const out: (bigint|number)[] = [];
		const one = x.from(1);

		x = x.dup();
		let a = x.divmod(one);
		if (x.sign() < 0) {
			--a;
			x = x.add(one);
		}
		out.push(a);

		for (let i = 1; i < maxTerms && (x.sign() !== 0 && (eps === undefined || eps.lt(x.abs()))); i++) {
			x = x.recip();
			out.push(x.divmod(one));
		}
		return out;
	},

	modPow<T extends hasop<'mul'> & divmodto<any>>(base: T, exp: number, mod: T): T {
		let result: T | undefined;
		while (exp) {
			base.divmod(mod);
			if (exp & 1) {
				result = result ? result.mul(base) : base;
				result!.divmod(mod);
			}
			base = base.mul(base);
			exp >>= 1;
		}
		return result!;
	},

	OperatorsBase<T extends ops1<T>>(_con: new (...args: any[]) => T) {
		const con		= _con as any;
		const proto		= con.prototype as Record<string, any>;
		
		const r: Partial<Operators<T>> = {
			func: (name: string, args: T[]) => {
				const staticFn = con[name];
				if (typeof staticFn === 'function')
					return staticFn.apply(con, args as any);

				const protoFn = proto[name];
				if (typeof protoFn === 'function')
					return protoFn.apply(args[0], (args as any).slice(1));

				return undefined;
			}
		};

		// guaranteed by the ops constraint
		r.dup = (a: T) => a.dup();
//		r.neg = (a: T) => a.neg();
		r.scale = (a: T, b: number) => a.scale(b);
		r.add = (a: T, b: T) =>	a.add(b);
		r.sub = (a: T, b: T) => a.sub(b);
		r.mul = (a: T, b: T) =>	a.mul(b);
		r.div = (a: T, b: T) =>	a.div(b);

		// static or prototype 'from'
		if (typeof con.from === 'function')
			r.from = (n: number) => con.from(n);
		else if (typeof proto.from === 'function')
			r.from = (n: number) => proto.from(n);

		// include prototype-backed binary ops only when present
		if ('eq' in proto)
			r.eq = (a: T, b: T) => (a as any).eq(b);
		if ('lt' in proto)
			r.lt = (a: T, b: T) => (a as any).lt(b);
		if ('pow' in proto)
			r.pow = (a: T, b: T) =>	(a as any).pow(b);

		if ('rpow' in proto)
			r.rpow = (a: T, n: number, d: number) => (a as any).rpow(n, d);

		r.ipow = 'ipow' in proto
			? (a: T, b: number) => (a as any).ipow(b)
			: (a: T, b: number) => gen.ipow(a, b);


		return r as unknown as Pick<Operators<T>, (keyof Operators<T> & keyof T) | 'func' | 'ipow'>;
	},

	extent: extentT,
};

export abstract class Mod<T> {
	constructor(public v: T) {}
	abstract dup() 				: this;
	abstract neg() 				: this;
	abstract scale(n: number) 	: this;
	abstract add(b: Mod<T>)		: this;
	abstract sub(b: Mod<T>)		: this;
	abstract mul(b: Mod<T>)		: this;
	abstract div(b: Mod<T>)		: this;
	abstract recip()	 		: this;
	abstract from(n: number) 	: this;
	abstract ipow(n: number)	: this;
	abstract sign()	 			: number;
	abstract eq(b: Mod<T>)		: boolean;
	abstract toString() 		: string;
}

export type ModFactory<T> = (new (v: T) => Mod<T>) & {
	wrap(p: T): Mod<T>;
//	_create?(p: T): Mod<T>;
};

// Generic polynomial-modulo-(r) wrapper factory for base=Polynomial<number>
export function ModFactory<T extends has<'sign' | 'abs' | 'dup' | 'from' | 'eq'> & divmodto<any>>(r: T) {
	class M extends Mod<T> {
		static wrap(p: T) {
			p = p.dup();
			p.divmod(r);
			return new this(p);
		}
		static _create(p: T) {
			return new this(p);
		}
		_create(p: T): this {
			return new (this.constructor as any)(p);
		}
		wrap(p: T): this {
			p = p.dup();
			p.divmod(r);
			return new (this.constructor as any)(p);
		}
		dup() { return this.wrap(this.v.dup()); }
		neg() { return this.wrap(this.v.neg()); }
		scale(n: number) { return this.wrap(this.v.scale(n)); }
		add(b: M) { return this.wrap(this.v.add(b.v)); }
		sub(b: M) { return this.wrap(this.v.sub(b.v)); }
		mul(b: M) { return this.wrap(this.v.mul(b.v)); }
		div(b: M) {	return this.mul(b.recip());	}
		recip() {
			const { g, x } = gen.extendedGcd(this.v.dup(), r.dup(), r.from(1)); // x * this + y * r = g
			return this.wrap(x.div(g));
			//throw new Error('Mod.recip: inverse does not exist');
		}
		from(n: number) { return this.wrap(this.v.from(n)); }
		ipow(n: number): this {
			if (n >= 0)
				return this.wrap(gen.modPow(this.v, n, r));
			return this.recip().ipow(-n);
		}
		// comparisons / sign
		sign()		{ return this.v.sign(); }
		eq(b: M)	{ return this.v.eq(b.v); }
		toString()	{ return this.v.toString(); }
	}
	return M;
}

export default gen;
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace gen {
	export type extent<T extends has<'lt'>> = extentT<T>;
}


