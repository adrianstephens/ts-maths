import { isNumber, isInstance, Operators, scalar, gcd, absB, signB, gcdB, divB, minB, lazySlice, has, rationalApprox, rationalApproxT, denominator } from "./core";
import rational from './rational';

//-----------------------------------------------------------------------------
// algebraic
//-----------------------------------------------------------------------------

export type algebraic = number | rational | radical | algebraicSum | algebraicProd

class radical {
	constructor(public base: algebraic, public root: number) {}
	eq(b: algebraic)	{
		return b instanceof radical && this.root === b.root && algebraicOps.eq(this.base, b.base);
	}
	neg()		{ return new algebraicProd([-1, this]); }
	valueOf()	{ return Number(this.base) ** (1 / this.root); }
	toString()	{ return this.root === 2 ? `sqrt(${this.base})` : `root(${this.base}, ${this.root})`; }
}
class algebraicSum {
	constructor(public terms: algebraic[]) {}
	eq(b: algebraic) {
		return b instanceof algebraicSum && this.terms.length === b.terms.length && this.terms.every((t, i) => algebraicOps.eq(t, b.terms[i]));
	}
	neg()		{ return new algebraicSum(this.terms.map(algebraicOps.neg)); }
	valueOf()	{ return this.terms.reduce((a: number, b) => a + Number(b), 0); }
	toString(): string { return `(${this.terms.map(t => t.toString()).join(' + ')})`; }
}
class algebraicProd {
	constructor(public factors: algebraic[]) {}
	eq(b: algebraic) {
		return b instanceof algebraicProd && this.factors.length === b.factors.length && this.factors.every((f, i) => algebraicOps.eq(f, b.factors[i]));
	}
	neg()		{ return combineFactors([-1, ...this.factors]); }
	valueOf() 	{ return this.factors.reduce((a: number, b) => a * Number(b), 1); }
	toString(): string { return `(${this.factors.map(t => t.toString()).join(' * ')})`; }
}

function makeRadical(n: algebraic, root: number): algebraic {
	if (isInstance(n, radical)) {
		root *= n.root;
		n = n.base;
	}

	if (n instanceof algebraicProd)
		return combineFactors(n.factors.map(f => makeRadical(f, root)));

	if (!isNumber(n))
		return new radical(n, root);

	if (n === 1)
		return 1;
	
	let coefficient = 1;
	let remaining = n;
	
	// Factor out perfect powers
	for (let i = 2; ; i++) {
		const factor = i ** root;
		if (factor > remaining)
			break;
		while (remaining % factor === 0) {
			coefficient *= i;
			remaining /= factor;
		}
	}
	
	return	remaining === 1 ? coefficient
		:	coefficient === 1 ? new radical(remaining, root)
		:	new algebraicProd([coefficient, new radical(remaining, root)]);
}

function combineTerms(terms: algebraic[]): algebraic {
	const termCounts = new Map<string, {factors: algebraic[], coef: rational}>();
	let	sum = rational(0);

	for (const term of terms) {
		if (isNumber(term)) {
			sum = sum.add(rational(term));
		} else if (isInstance(term, rational)) {
			sum = sum.add(term);
		} else {
			let rat = rational(1);
			let factors: algebraic[] = [term];

			if (isInstance(term, algebraicProd)) {
				factors = term.factors;
				const num1 = factors.find(f => isNumber(f));
				if (num1) {
					rat = rational(num1);
					factors = factors.filter(f => f !== num1);
				}
				const rat1 = factors.find(f => isInstance(f, rational));
				if (rat1) {
					rat = rat1;
					factors = factors.filter(f => !isInstance(f, rational));
				}
			}

			const key = JSON.stringify(factors);
			const existing = termCounts.get(key);
			if (existing)
				existing.coef = existing.coef.add(rat);
			else
				termCounts.set(key, {factors, coef: rat});
		}
	}

	// Add combined terms
	const combined: algebraic[] = [];
	for (const {factors, coef} of termCounts.values()) {
		if (coef.sign()) {
			if (!coef.eq(rational(1)))
				factors.unshift(coef.den === 1 ? coef.num : coef);
			combined.push(factors.length === 1 ? factors[0] : new algebraicProd(factors));
		}
	}

	if (combined.length === 0)
		return sum.den === 1 ? sum.num : sum;

	if (!sum.eq(rational(0)))
		combined.push(sum);

	return combined.length === 1 ? combined[0] : new algebraicSum(combined);
}

function combineFactors(factors: algebraic[]): algebraic {
	const combined: algebraic[] = [];
	const radicalByBase:	Record<string, {base: algebraic, power: rational}> = {};
	let product = rational(1);

	for (const factor of factors) {
		if (isNumber(factor)) {
			product = product.scale(factor);
		} else if (isInstance(factor, rational)) {
			product = product.mul(factor);
		} else if (isInstance(factor, radical)) {
			const existing = radicalByBase[JSON.stringify(factor.base)] ??= {base: factor.base, power: rational(0)};
			existing.power = existing.power.add(rational(1, factor.root));
		} else {
			combined.push(factor);
		}
	}

	if (product.sign() === 0)
		return 0;

	const radicalsByRoot:	string[][] = [];
	for (const [key, {power}] of Object.entries(radicalByBase))
		(radicalsByRoot[power.den] ??= []).push(key);

	// Combine radicals with same root: √a × √b = √(a×b)
	for (const i in radicalsByRoot) {
		const root	= +i;
		const exprs	= radicalsByRoot[root];
		if (root === 1) {
			const base = exprs.reduce((a: algebraic, b) => algebraicOps.mul(a, radicalByBase[b].base), 1);
			if (isNumber(base)) {
				product = product.scale(base);
			} else if (isInstance(base, rational)) {
				product = product.mul(base);
			}
			for (const key of exprs) {
				const expr = radicalByBase[key];
				expr.power = rational(0);
			}

		} else if (exprs.length > 1) {
			const base = exprs.reduce((a: algebraic, b) => algebraicOps.mul(a, radicalByBase[b].base), 1);
			combined.push(makeRadical(base, root));
			for (const key of exprs) {
				const expr = radicalByBase[key];
				expr.power = rational(expr.power.floor());
			}
		}
	}

	// Add combined radicals
	for (const {base, power} of Object.values(radicalByBase)) {
		if (power.sign() > 0)
			combined.push(algebraicOps.pow(base, power));
	}

	// Add combined numeric/rational factor
	if (combined.length === 0)
		return product.den === 1 ? product.num : product;

	if (!product.eq(rational(1)))
		combined.unshift(product.den === 1 ? product.num : product);

	return combined.length === 1 ? combined[0] : new algebraicProd(combined);
}


export const algebraicOps: Operators<algebraic> = {
	from(n: number) { return n; },
	func(_name: string, _args: algebraic[]) { return undefined; },
	variable(_name: string) { return undefined; },

	dup(a: algebraic) {
		return a;
	},
	neg(a: algebraic) {
		return isNumber(a) ? -a : a.neg();
	},
	add(a: algebraic, b: algebraic) {
		return combineTerms([...(a instanceof algebraicSum ? a.terms : [a]), ...(b instanceof algebraicSum ? b.terms : [b])]);
	},
	sub(a: algebraic, b: algebraic) {
		return this.add(a, this.neg(b));
	},
	mul(a: algebraic, b: algebraic) {
		// Expand (a+b)×(c+d) = ac + ad + bc + bd
		if (a instanceof algebraicSum || b instanceof algebraicSum) {
			const aTerms = a instanceof algebraicSum ? a.terms : [a];
			const bTerms = b instanceof algebraicSum ? b.terms : [b];
			const expandedTerms: algebraic[] = [];
			for (const aTerm of aTerms) {
				for (const bTerm of bTerms)
					expandedTerms.push(combineFactors([...(aTerm instanceof algebraicProd ? aTerm.factors : [aTerm]), ...(bTerm instanceof algebraicProd ? bTerm.factors : [bTerm])]));
			}
			return combineTerms(expandedTerms);
		}
		return combineFactors([...(a instanceof algebraicProd ? a.factors : [a]), ...(b instanceof algebraicProd ? b.factors : [b])]);
	},

	div(a: algebraic, b: algebraic) {
		return this.mul(a, this.ipow(b, -1));
	},
	ipow(a: algebraic, n: number) {
		if (isNumber(a))
			return n < 0 ? rational(1, a ** -n) : a ** n;
		if (isInstance(a, rational))
			return a.ipow(n);
		if (isInstance(a, radical))
			return this.pow(a.base, rational(n, a.root));
		throw new Error('unsupported type for ipow');
	},
	pow(a: algebraic, b: algebraic) {
		if (isNumber(b))
			return this.ipow(a, b);
		if (isInstance(b, rational)) {
			if (b.num !== 1)
				a = this.ipow(a, b.num);
			return b.den === 1 ? a : makeRadical(a, b.den);
		}
		throw new Error('unsupported type for pow');
	},

	eq(a: algebraic, b: algebraic): boolean {
		if (a === b)
			return true;
		if (isInstance(a, rational) && isInstance(b, rational))
			return a.eq(b);
		if (isInstance(a, radical))
			return a.eq(b);
		if (a instanceof algebraicSum)
			return a.eq(b);
		if (a instanceof algebraicProd)
			return a.eq(b);
		return false;
	},
	lt(a: algebraic, b: algebraic) { throw new Error('unsupported type for lt'); }

};

