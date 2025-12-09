import { isNumber, isInstance, Operators } from "./core";
import rational from "./rational";
import { radicalChars } from "./string";

type algebraic = number | rational | compound;

type root = { base: algebraic, root: number };
type term = { coeff: rational, roots: root[] };

class compound {
	constructor(public terms: term[] = []) {}

	eq(other: algebraic): boolean {
		return String(this) === String(other);
	}
	toString(): string {
		return this.terms.map(({coeff, roots}, i) => roots.length === 0
			? (i === 0 ? String(coeff) : (coeff.sign() > 0 ? ' + ' : ' - ') + String(coeff.abs()))
			: [
				(i === 0 ? '' : coeff.sign() > 0 ? ' + ' : ' - '),
				...(!coeff.abs().eq(rational(1)) ? [maybeParentheses(coeff.abs())] : []),
				...roots.map(r => radicalChars[r.root] ? `${radicalChars[r.root]}${maybeParentheses(r.base)}` : `${maybeParentheses(r.base)}^(1/${r.root})`)
			].join('')
		).join('') || '0';
	}
	valueOf() {
		return this.terms.reduce((a, {coeff, roots}) => a + 
			roots.reduce((a, {base, root}) => a * Number(base) ** (1 / root), Number(coeff))
		, 0);
	}
}

function root(base: algebraic, root: number) : root {
	return {base, root};
}
function term(coeff: number | rational, roots: root[] = []): term {
	return {coeff: asRational(coeff), roots};
}
function isZero(a: algebraic): boolean {
	return a === 0 || (isInstance(a, rational) && a.sign() === 0);
}
function asRational(x: number | rational) : rational {
	return isNumber(x) ? rational(x) : x;
}
function asTerms(a: algebraic): term[] {
	return a instanceof compound ? a.terms : [term(a)];
}

function maybeParentheses(x: algebraic) {
	const s = String(x);
	return  /[^0-9]/.exec(s) ? `(${s})` : s;
}

function simplifyRadical(base: number, n: number) {
	let remaining = base, extracted = 1;
	for (let i = 2, f = 2 ** n; f <= remaining; f = ++i ** n) {
		while (remaining % f === 0) {
			extracted *= i;
			remaining /= f;
		}
	}
	return [remaining, extracted];
}

// Try to denest compound square roots: √(a + b√c) = √x + √y
function simplifyQuad(base: compound): term[] | undefined {
	let [t1, t2] = base.terms;
	if (t1.roots.length)
		[t1, t2] = [t2, t1];
	if (t1.roots.length === 0 && t2.roots.length === 1) {
		const a = t1.coeff;
		const b = t2.coeff;
		const r = t2.roots[0];
		if (r.root === 2 && !(r.base instanceof compound)) {
			const disc = a.mul(a).sub(b.mul(b).mul(asRational(r.base)));
			if (disc.sign() >= 0) {
				const sqrtDisc = Math.sqrt(Number(disc));
				if (Math.floor(sqrtDisc) === sqrtDisc) {
					const sqrtDiscRat = rational(sqrtDisc);
					const x = a.add(sqrtDiscRat).div(rational(2));
					const y = a.sub(sqrtDiscRat).div(rational(2));
					if (x.sign() >= 0 && y.sign() >= 0)
						return [term(b.sign(), [root(x, 2)]), term(b.sign(), [root(y, 2)])];
				}
			}
		}
	}
}

function termsCrossMul(at: term[], bt: term[]): term[] {
	const result: term[] = [];
	for (const termA of at) {
		for (const termB of bt)
			result.push(...termMul(termA, termB));
	}
	return result;
}

// Normalize rational bases: (a/b)^(1/n) = a^(1/n) / b^(1/n)
function normalizeRoot(r: root): root[] {
	if (isInstance(r.base, rational) && r.base.den !== 1) {
		return [
			{base: r.base.num, root: r.root},
			{base: r.base.den, root: -r.root}
		];
	}
	return [r];
}

// multiply two terms
function termMul(termA: term, termB: term): term[] {
	let coeff = termA.coeff.mul(termB.coeff);
	const compounds: compound[] = [];
	const rootPowers: Record<string, {root: root, power: rational, count: number}> = {};
	
	// Accumulate powers: a^(1/n) × a^(1/m) = a^(1/n + 1/m)
	// Normalize rational bases first so (1/2)^(1/4) becomes 2^(1/4) / 2^(1/4)
	for (const r of [...termA.roots, ...termB.roots]) {
		for (const nr of normalizeRoot(r)) {
			const key = String(nr.base);
			const power = rational(1, nr.root);
			const existing = rootPowers[key];
			if (existing) {
				existing.power = existing.power.add(power);
				existing.count++;
			} else {
				rootPowers[key] = {root: nr, power, count: 1};
			}
		}
	}
	
	// Convert accumulated powers back to roots, only calling algebraic.pow when power changed
	const roots: root[] = [];
	for (const {root, power, count} of Object.values(rootPowers)) {
		if (count === 1) {
			// Unchanged - just keep the root
			roots.push(root);
		} else if (power.sign()) {
			// Power changed - need to simplify through algebraic.pow
			const t = algebraic.pow(root.base, power);
			if (t instanceof compound)
				compounds.push(t);
			else
				coeff = coeff.mul(asRational(t));
		}
	}
	
	let result = [term(coeff, roots)];
	for (const t of compounds)
		result = termsCrossMul(result, t.terms);
	return result;
}

// take nth root of a term: flatten nested roots: (c × a^(1/n))^(1/m) = c^(1/m) × a^(1/nm)
function termRoot(t: term, n: number) {
	const roots = t.roots.map(r => root(r.base, r.root * n));
	const flattened = simplifyTerms([{coeff: rational(1), roots}]);
	return algebraic.mul(algebraic.pow(t.coeff, rational(1, n)), flattened);
}

function simplifyTerm(t: term): term[] {
	let coeff = t.coeff;
	const compounds: compound[] = [];
	const roots: root[] = [];

	// Normalize rational bases first, then combine products under same root
	const rootGroups: algebraic[][] = [];
	for (const r of t.roots) {
		for (const nr of normalizeRoot(r))
			(rootGroups[nr.root] ??= []).push(nr.base);
	}
	
	for (const i in rootGroups) {
		const root	= +i;
		const bases = rootGroups[root];

		// Multiply all numeric/rational bases together
		let product = rational(1);
		for (const base of bases) {
			if (!(base instanceof compound))
				product = product.mul(asRational(base));
		}
		
		if (!product.eq(rational(1))) {
			if (root < 0)
				product = product.recip();
			const aroot = Math.abs(root);
			const [numRem, numExt] = simplifyRadical(product.num, aroot);
			const [denRem, denExt] = simplifyRadical(product.den, aroot);
			coeff = coeff.mul(rational(numExt, denExt));
			if (numRem !== 1 || denRem !== 1)
				roots.push({base: denRem === 1 ? numRem : rational(numRem, denRem), root: aroot});
		}

		for (const base of bases) {
			if (base instanceof compound) {
				if (root === 2 && base.terms.length === 2) {
					const result = simplifyQuad(base);
					if (result) {
						compounds.push(new compound(result));
						continue;
					}
				}
				roots.push({base, root});
			}
		}
	}

	// Multiply collected compound factors
	let result: term[] = [term(coeff, roots)];
	for (const c of compounds)
		result = termsCrossMul(result, c.terms);
	return result;
}

function simplifyTerms(terms: term[]): algebraic {
	const termMap: Record<string, term> = {};

	for (const t of terms) {
		if (t.coeff.sign()) {
			const ts	= simplifyTerm(t);
			for (const t of ts) {
				const key	= JSON.stringify(t.roots.sort());
				if (termMap[key])
					termMap[key].coeff = termMap[key].coeff.add(t.coeff);
				else
					termMap[key] = t;
			}
		}
	}

	// Remove zero terms and convert back to array
	const simplified = Array.from(Object.values(termMap)).filter(t => t.coeff.sign() !== 0);
	if (simplified.length === 0)
		return 0;

	// Look for pattern: x^(1/n) + c·x^(-1/n) which can denest => 2·(x^(1/2n))
	if (simplified.length === 2) {
		const [t1, t2] = simplified;
		if (t1.roots.length === 1 && t2.roots.length === 1) {
			const r1 = t1.roots[0], r2 = t2.roots[0];
			if (algebraic.eq(r1.base, r2.base) && r1.root === -r2.root && t1.coeff.eq(t2.coeff))
				return algebraic.mul(t1.coeff.scale(2), algebraic.pow(r1.base, rational(1, 2 * Math.abs(r1.root))));
		}
	}

	if (simplified.length === 1 && simplified[0].roots.length === 0) {
		const coeff = simplified[0].coeff;
		return coeff.den === 1 ? coeff.num : coeff;
	}

	return new compound(simplified);
}

const algebraic: Operators<algebraic> & {
	abs(a: algebraic): algebraic;
} = {
	from: (n: number) => rational.from(n),
	func: () => undefined,
	variable: () => undefined,

	dup: (a: algebraic) => a,
	neg: (a: algebraic) => isNumber(a) ? -a
		: isInstance(a, rational) ? a.neg()
		: new compound(a.terms.map(t => term(t.coeff.neg(), t.roots))),
	
	add: (a: algebraic, b: algebraic) => simplifyTerms([...asTerms(a), ...asTerms(b)]),
	mul: (a: algebraic, b: algebraic) => isZero(a) || isZero(b) ? 0 : simplifyTerms(termsCrossMul(asTerms(a), asTerms(b))),
	sub: (a: algebraic, b: algebraic) => algebraic.add(a, algebraic.neg(b)),
	div: (a: algebraic, b: algebraic) => algebraic.mul(a, algebraic.pow(b, rational(-1))),

	ipow: (a: algebraic, n: number) => {
		if (n === 0)
			return 1;
		if (isZero(a))
			return 0;
		if (n < 0)
			return algebraic.pow(algebraic.ipow(a, -n), rational(-1));
		
		let result = n & 1 ? a : undefined;
		for (n >>= 1; n; n >>= 1) {
			a = algebraic.mul(a, a);
			if (n & 1)
				result = result ? algebraic.mul(result, a) : a;
		}
		return result!;
	},
	rpow: (a: algebraic, n: number, d: number) => {
		return algebraic.pow(a, rational(n, d));
	},
	pow: (a: algebraic, b: algebraic) => {
		if (isNumber(b))
			return algebraic.ipow(a, b);

		if (isInstance(b, rational)) {
			if (isZero(a))
				return 0;

			const	base = Math.abs(b.num) === 1 ? a : algebraic.ipow(a, Math.abs(b.num));
			const	root = b.num < 0 ? -b.den : b.den;
			return	root === 1 ? base
				:	base instanceof compound && base.terms.length === 1 ? termRoot(base.terms[0], root)
				:	simplifyTerms([term(1, [{base, root}])]);
		}
		throw new Error('unsupported exponent');
	},

	eq: (a: algebraic, b: algebraic) => String(a) === String(b),
	lt: (a: algebraic, b: algebraic) => Number(a) < Number(b),
	abs: (a: algebraic) => isNumber(a) ? Math.abs(a)
		: isInstance(a, rational) ? a.abs()
		: new compound(a.terms.map(t => term(t.coeff.abs(), t.roots))),

};

export default algebraic;
