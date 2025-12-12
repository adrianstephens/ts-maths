import {Operators, ops, scalar, scalarRational, scalarExt, has, has0, Immutable, hasProperty} from "./core";

//-----------------------------------------------------------------------------
// Generics
//-----------------------------------------------------------------------------

export function OperatorsBase<T extends ops<T>>(_con: new (...args: any[]) => T) {
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
	r.neg = (a: T) => a.neg();
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
		: (a: T, b: number) => Gen.ipow(a, b);


	return r as unknown as Pick<Operators<T>, (keyof Operators<T> & keyof T) | 'func' | 'ipow'>;
}

export type canDenominator = Pick<scalarExt<any>, 'from'|'divmod'|'recip'|'lt'>;

export const Gen = {
	dup<T extends ops<T>>(a: T) 		{ return a.dup(); },
	neg<T extends ops<T>>(a: T) 		{ return a.neg(); },
	add<T extends ops<T>>(a: T, b: T)	{ return a.add(b); },
	sub<T extends ops<T>>(a: T, b: T)	{ return a.sub(b); },
	mul<T extends ops<T>>(a: T, b: T)	{ return a.mul(b); },
	div<T extends ops<T>>(a: T, b: T)	{ return a.div(b); },

	ipow<T extends ops<T>>(base: T, exp: number): T {
		let result = exp & 1 ? base : undefined;
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

	gcd<T extends Pick<scalarExt<any>, 'sign'|'abs'> & hasProperty<'divmod', (b: T)=>any>>(...values: T[]): T {
		let a: T | undefined;
		for (let b of values) {
			b = b.abs();
			if (a && a.sign()) {
				while (b.sign()) {
					a.divmod(b);
					[a, b] = [b, a];
				}
			} else {
				a = b;
			}
		}
		return a!;
	},

	lcm<T extends Pick<scalarExt<any>, 'divmod'|'sign'|'abs'|'scale'>>(...values: T[]) {
		let a: T | undefined;
		for (const b of values)
			a = a ? b.scale(Number(a.divmod(Gen.gcd(a, b)))) : b;
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
			const f = BigInt(x.divmod(one));
			[k2, k1] = [k1, f * k1 + k2];
		} while ((!eps || eps.lt(x)) && k1 < maxDen);
		return k1;
	},

	commonDenominator<T extends canDenominator & has0<'scale'>>(numbers: T[], maxDen = 1000n, eps?: T) {
		let scale = 1n;
		for (const n of numbers) {
			scale *= Gen.denominator(n.scale(Number(scale)), maxDen, eps);
			if (scale > maxDen)
				return 0;
		}
		return scale;
	},

	rationalApprox<T extends scalar<T> & canDenominator>(x: Immutable<T>, maxDen: bigint, eps?: T): [bigint, bigint] {
		const den = this.denominator(x.dup(), maxDen, eps);
		return [BigInt(x.dup().divmod(x.from(den))), den];
/*
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
		*/
	},
	
	continuedFractionT<T extends scalarRational<T>>(x: T, maxTerms = 64, eps?: T): (bigint|number)[] {
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

	modPow<T extends has<'divmod'>>(base: T, exp: number, mod: T): T {
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
	}
};

export default Gen;

export class extentT<T extends has<'lt'>> {
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
		this.min = Gen.min(this.min, p);
		this.max = Gen.max(this.max, p);
	}
	combine(b: extentT<T>) {
		this.min = Gen.min(this.min, b.min);
		this.max = Gen.max(this.max, b.max);
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
		return Gen.min(Gen.max(p, this.min), this.max);
	}
}
