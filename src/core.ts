
export type hasProperty<K extends string, S> = { [P in K]: S };


export function isInstance<T>(x: any, i: (new (...args: any[]) => T) | ((...args: any[]) => T)): x is T {
	return x instanceof i || x.prototype === i.prototype;
}

export function hasStatic(x: any, f: string) {
	const c = x.constructor;
	if (f in c)
		return c[f] as (...args: any[]) => any;
	return undefined;
}
export function arrayOf<U>(arr: any[], g: (x: any) => x is U): arr is U[] {
	for (const i in arr)
		return g(arr[i]);
	return false;
}

// Transform method type to free function type
export type AsFreeFunction<T, K extends keyof T> = T[K] extends (this: infer This, ...args: infer Args) => infer R ? (self: This, ...args: Args) => R : never;

export interface Operators<T> {
	from(n: number): T;
	func(name: string, args: T[]): T | undefined;
	variable(name: string): T | undefined;

	dup(a: T): T;
	neg(a: T): T;
	scale(a: T, b: number):	T;
	add(a: T, b: T): T;
	sub(a: T, b: T): T;
	mul(a: T, b: T): T;
	div(a: T, b: T): T;

	ipow(a: T, n: number): T;
	rpow(a: T, n: number, d: number): T;
	pow(a: T, b: T): T;
	eq(a: T, b: T): any;
	lt(a: T, b: T): any;

//	optional
//	npow(a: T, n: number): T;
}


export function Type<T>(operators: Operators<T>) {
	return class Ops implements ops<Ops> {
		constructor(public value: T) {}
		from(n: number): Ops 	{ return new Ops(operators.from(n)); }
		dup(): Ops				{ return new Ops(operators.dup(this.value)); }
		neg(): Ops				{ return new Ops(operators.neg(this.value)); }
		scale(b: number): Ops	{ return new Ops(operators.scale(this.value, b)); }
		add(b: Ops): Ops		{ return new Ops(operators.add(this.value, b.value)); }
		sub(b: Ops): Ops		{ return new Ops(operators.sub(this.value, b.value)); }
		mul(b: Ops): Ops		{ return new Ops(operators.mul(this.value, b.value)); }
		div(b: Ops): Ops 		{ return new Ops(operators.div(this.value, b.value)); }
		mag():		number		{ return typeof this.value === 'number' ? Math.abs(this.value) : 0; }

		// scalar
		abs():		Ops		{ if (hasFree('abs')(operators)) return new Ops(operators.abs(this.value)); return this; };
		sign():		number	{ const z = operators.from(0); return operators.eq(this.value, z) ? 0 : operators.lt(this.value, z) ? -1 : 1; }
		eq(b: Ops):	boolean	{ return operators.eq(this.value, b.value); }
		lt(b: Ops):	boolean	{ return operators.lt(this.value, b.value); }

		//power
		sqrt(): 			Ops	{ return new Ops(operators.rpow(this.value, 1, 2)); }
		recip():			Ops	{ return new Ops(operators.ipow(this.value, -1)); }
		ipow(n: number):	Ops	{ return new Ops(operators.ipow(this.value, n)); }
		rpow(n: number, d: number):	Ops	{ return new Ops(operators.rpow(this.value, n, d)); }
		npow(n: number):	Ops	{ return new Ops(operators.pow(this.value, operators.from(n))); }

		//ext
//		divmod(b: Ops):		number | bigint;

		toString(): string { return String(this.value); }
		valueOf(): number { return Number(this.value); }	
	};
}


export interface ops<C extends ops<C, S>, S=number> {
	dup():				C;
	neg(): 				C;
	scale(b: S):		C;
	add(b: C): 			C;
	sub(b: C): 			C;
	mul(b: C):			C;
	div(b: C):			C;
	mag():				number | scalarExt<any>;
}

export interface scalar<C extends scalar<C, S>, S=number> extends ops<C, S> {
	from(n: number | bigint):	C;
	ipow(n: number):	C;
	abs():				C;
	sign():				number;
	eq(b: C):			boolean;
	lt(b: C):			boolean;
	valueOf():			number | bigint;
}

export interface scalarPow<C extends scalarPow<C>> extends scalar<C> {
	sqrt(): 			C;
	recip():			C;
	rpow(n: number, d: number):	C;
	npow(n: number):	C;
}

export interface scalarExt<C extends scalarExt<C>> extends scalarPow<C> {
	divmod(this: C, b: C):	number | bigint;
}

export type scalarRational<T extends scalar<T>> = scalar<T> & has<'divmod'> & has<'recip'>;
export type Immutable<T> = Omit<T, 'divmod'>;

export type has0<K extends keyof scalarExt<any>> = Pick<scalarExt<any>, K>;
export function has0<K extends keyof scalarExt<any>>(f: K) {
	return (x: object): x is has0<K> => f in x;
}

export type has<K extends keyof scalarExt<any>> = ops<any> & Pick<scalarExt<any>, K>;
export function has<K extends keyof scalarExt<any>>(f: K) {
	return (x: ops<any>): x is has<K> => f in x;
}

export type hasFree<K extends keyof scalarExt<any>> = {[P in K]: AsFreeFunction<scalarExt<any>, K> };
export function hasFree<K extends keyof scalarExt<any>>(f: K) {
	return (x: object): x is hasFree<K> => f in x;
}

export function isScalar(x: ops<any>): x is scalar<any> {
	return 'lt' in x;
}
export function isScalarRational(x: ops<any>): x is scalarRational<any> {
	return 'divmod' in x;
}
export function isScalarExt(x: ops<any>): x is scalarExt<any> {
	return 'rpow' in x && 'divmod' in x;
}

export function asScalarT<T extends scalarExt<T>>(from: T, x: number|T): T {
	if (typeof x === 'number')
		return from.from(x);
	if (x instanceof from.constructor)
		return x;
	return from.from(+x);
}


export function compare<T extends number|bigint|string>(a: T, b: T): number {
	return a < b ? -1 : a > b ? 1 : 0;
}

export function* lazySlice<T>(arr: T[], start?: number, end?: number): Generator<T> {
	const len = arr.length;
	start = start === undefined	? 0
		:	start < 0			? Math.max(len + start, 0)
		:	Math.min(start, len);

	end =	end === undefined	? len
		:	end < 0				? Math.max(len + start, 0)
		:	Math.min(end, len);

	for (let i = start; i < end; i++)
		yield arr[i];
}
