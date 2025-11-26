import { symbolic, MatchOptions, Bindings, term, factor, mulFactors, addTerms, factorAsSymbolic } from './symbolic';

export type Rule = {
	name:		string;
	match:		(node: symbolic, opts?: MatchOptions) => Bindings | null;
	replace:	(bs: Bindings) => symbolic;
	guard?:		(bs: Bindings) => boolean;
};


export function scoreFactory(depthPenalty = 0.2, mulW = 0.5, addW = 1, constW = 0.1) {
    return (n: symbolic, bestScore?: number) => {
        let total = 0;
		let depth = 1;
		bestScore ??= Infinity;

        n.visit({
			noRemake: true,
            pre: (x: symbolic) => {
				if (total > bestScore)
					return undefined;
                ++depth;
                return x;
            },
            post: (x: symbolic) => {
                --depth;
                const w = x.is('mul') ? (x.factors.length - 1) * mulW
               		: x.is('add') ? (x.terms.length - 1) * addW
               		: x.is('const') ? constW
               		: 0.5;
                total += w * (1 + depthPenalty * depth);
                return x;
            }
        });

        return total;
    };
}

export function applyRules(node: symbolic, rules: Rule[]): symbolic {
	const cache		= new Map<string, symbolic>();

	const visitor = {
		post: (node: symbolic) => {
			const cached = cache.get(node.id);
			if (cached)
				return cached;

			for (const r of rules) {
				const bs = r.match(node);
				if (bs && (!r.guard || r.guard(bs))) {
					const replaced		= r.replace(bs);
					const simplified	= replaced.visit(visitor);
					cache.set(node.id, simplified);
					return simplified;
				}
			}
			cache.set(node.id, node);
			return node;
		}
	};

	return node.visit(visitor);
}

//-----------------------------------------------------------------------------
// factoring
//-----------------------------------------------------------------------------

// collect factor counts and min powers across terms
function getFactors(terms: readonly Readonly<term>[]) {
	const factors: Record<string, factor & { terms: Set<term> }> = {};

	function addFactor(item: symbolic, pow: number, t: term) {
		const info = factors[item.id] ??= { item: item, pow: pow, terms: new Set<term>() };
		info.terms.add(t);
		info.pow = Math.min(info.pow, pow);
	}
	for (const t of terms) {
		if (t.item.is('mul')) {
			for (const f of t.item.factors)
				addFactor(f.item, f.pow, t);
		} else {
			addFactor(t.item, 1, t);
		}
	}
	return Object.values(factors);
}

function commonFactors(terms: readonly Readonly<term>[], scorer: (sym: symbolic) => number): readonly term[] {
	const factors = getFactors(terms);
	const singles = factors.filter(i => i.terms.size > 1);
	const pairs: {a: factor, b: factor, terms: Set<term>}[] = [];

	singles.sort((a, b) => b.terms.size - a.terms.size);
	singles.length = Math.min(singles.length, 16);

	for (const a of singles) {
		for (const b of singles) {
			if (a === b)
				break;
			const terms = a.terms.intersection(b.terms);
			if (terms.size > 1)
				pairs.push({a, b, terms});
		}
	}

	pairs.sort((a, b) => b.terms.size - a.terms.size);
	pairs.length = Math.min(pairs.length, 16);


	// prefer candidates that cover more terms and that simplify their inner sum under trig rules
	const scoreCandidate = (terms: Set<term>, inner: symbolic) => terms.size * 10 - scorer(inner);
	
	function remove1Factor(t: term, a: factor) {
		if (t.item.is('mul')) {
			const newFactors = t.item.factors.map(f =>
				f.item === a.item ? factor(f.item, f.pow - a.pow)
				: f
			).filter(ff => ff.pow !== 0);
			return term(mulFactors(t.item.num, ...newFactors), t.coef);
		}
		return t.item === a.item ? term(one, t.coef) : t;
	}

	function remove2Factors(t: term, a: factor, b: factor) {
		if (t.item.is('mul')) {
			const newFactors = t.item.factors.map(f =>
				f.item === a.item ? factor(f.item, f.pow - a.pow)
			:   f.item === b.item ? factor(f.item, f.pow - b.pow)
			:   f
			).filter(f => f.pow !== 0);
			return term(mulFactors(t.item.num, ...newFactors), t.coef);
		}
		return t;
	}

	const factored: term[] = [];
	let remaining = new Set<term>(terms);

	while (remaining.size > 1) {
		// find best candidate
		type Candidate = { mul: symbolic; terms: Set<term>; inner: symbolic; score: number };
		let best: Candidate | undefined;

		// singles
		for (const f of singles) {
			const terms = f.terms.intersection(remaining);
			if (terms.size > 1)  {
				const inner = addTerms(0, ...Array.from(terms).map(t => remove1Factor(t, f)));
				const score = scoreCandidate(terms, inner);
				if (!best || score > best.score)
					best = { mul: factorAsSymbolic(f), terms, inner, score };
			}
		}

		// pairs
		for (const p of pairs) {
			const terms = p.terms.intersection(remaining);
			if (terms.size > 1)  {
				const inner = addTerms(0, ...Array.from(terms).map(t => remove2Factors(t, p.a, p.b)));
				const score = scoreCandidate(terms, inner);
				if (!best || score > best.score)
					best = { mul: factorAsSymbolic(p.a).mul(factorAsSymbolic(p.b)), terms, inner, score };
			}
		}

		if (!best)
			break;

		factored.push(term(best.inner.mul(best.mul)));
		remaining = remaining.difference(best.terms);
	}

	return [...factored, ...remaining];
}

//-----------------------------------------------------------------------------
// transformation rules
//-----------------------------------------------------------------------------

function replaceRest(bs: Bindings, node: symbolic): symbolic {
	const addrest = bs['_addrest'];
	if (addrest)
		node = node.add(addrest);
	const mulrest = bs['_mulrest'];
	if (mulrest)
		node = node.mul(mulrest);
	return node;
}

function PatternRule(name: string, pattern: symbolic, replace: (bs: Bindings) => symbolic, guard?: (bs: Bindings) => boolean): Rule {
	return {
		name,
		match: (node, opts) => pattern.match(node, {}, opts),
		replace: bs => replaceRest(bs, replace(bs)),
		guard,
	};
}

const A		= symbolic.bind('A');
const B		= symbolic.bind('B');
const C		= symbolic.bind('C');

const one	= symbolic.one;
const zero	= symbolic.zero;
const pi	= symbolic.pi;

function isConst(e: symbolic) {
	return e.is('const');
}

export const generalRules: Rule[] = [
	{
		name: 'mul-distribute',
		match(node: symbolic) {
			if (node.is('mul') && (node.factors.length > 1 || (node.factors[0].pow > 1 && Number.isInteger(node.factors[0].pow)))) {
				const result = node.expand({ depth: 1, maxPow: 4 });
				if (result !== node)
					return {result};
			}
			return null;
		},
		replace(bs: Bindings): symbolic {
			return bs.result;
		}
	},
	{
		name: 'collect-like-terms',
		match(node: symbolic) {
			// Only factor expressions where terms share multiplicative factors
			// Skip simple additions like A+B that have no common structure
			if (!(node.is('add')) || node.terms.length < 2)
				return null;
			
			// Check if any terms share factors (otherwise factoring makes no sense)
			const hasMul = node.terms.some(t => t.item.is('mul'));
			if (!hasMul)
				return null;
				
			const scorer = scoreFactory();
			const terms = commonFactors(node.terms, scorer);
			const result = addTerms(node.num, ...terms);

			if (result !== node)
				return { result };
			return null;
		},
		replace(bs: Bindings) {
			return bs.result;
		}
	},
	/*
	PatternRule('factor-common',
		// pattern: A*B + A*C  ->  A * (B + C)
		A.mul(B).add(A.mul(C)),
		bs => bs.A.mul(bs.B.add(bs.C))
	),*/
	
	// quadratics:

	// Perfect square trinomial: A^2 + 2AB + B^2 -> (A+B)^2
	PatternRule('perfect-square-sum',
		A.pow(2).add(A.mul(B).scale(2)).add(B.pow(2)),
		bs => bs.A.add(bs.B).pow(2)
	),
	// Perfect square trinomial: A^2 - 2AB + B^2 -> (A-B)^2
	PatternRule('perfect-square-diff',
		A.pow(2).sub(A.mul(B).scale(2)).add(B.pow(2)),
		bs => bs.A.sub(bs.B).pow(2)
	),
	// Difference of squares: a² - b² → (a-b)(a+b)
	PatternRule('factor-diff-squares',
		A.pow(2).sub(B.pow(2)),
		bs => bs.A.sub(bs.B).mul(bs.A.add(bs.B))
	),
	
	// cubics:

	// Perfect square trinomial (3 vars): A^2 + B^2 + C^2 + 2AB + 2AC + 2BC -> (A+B+C)^2
	PatternRule('perfect-square-sum-3',
		A.pow(2).add(B.pow(2)).add(C.pow(2)).add(A.mul(B).scale(2)).add(A.mul(C).scale(2)).add(B.mul(C).scale(2)),
		bs => bs.A.add(bs.B).add(bs.C).pow(2)
	),
	// Perfect square trinomial (3 vars): A^2 + B^2 + C^2 - 2AB - 2AC - 2BC -> (A-B-C)^2
	PatternRule('perfect-square-diff-3',
		A.pow(2).add(B.pow(2)).add(C.pow(2)).sub(A.mul(B).scale(2)).sub(A.mul(C).scale(2)).sub(B.mul(C).scale(2)),
		bs => bs.A.sub(bs.B).sub(bs.C).pow(2)
	),

	// Symmetric cubic: (A+B+C)^3 - 3(A+B+C)(AB+AC+BC) + 3ABC -> A^3+B^3+C^3
	//PatternRule('symmetric-cubic-expand',
	//	A.add(B).add(C).pow(3).sub(A.add(B).add(C).mul(A.mul(B).add(A.mul(C)).add(B.mul(C))).scale(3)).add(A.mul(B).mul(C).scale(3)),
	//	bs => bs.A.pow(3).add(bs.B.pow(3)).add(bs.C.pow(3))
	//),
	// Symmetric cubic contraction: A^3+B^3+C^3 -> (A+B+C)^3 - 3(A+B+C)(AB+AC+BC) + 3ABC
	PatternRule('symmetric-cubic-contract',
		A.pow(3).add(B.pow(3)).add(C.pow(3)),
		bs => bs.A.add(bs.B).add(bs.C).pow(3).sub(bs.A.add(bs.B).add(bs.C).mul(bs.A.mul(bs.B).add(bs.A.mul(bs.C)).add(bs.B.mul(bs.C))).scale(3)).add(bs.A.mul(bs.B).mul(bs.C).scale(3))
	),
	// AB+AC+BC contraction: (A+B)C + AB -> AB + AC + BC
	//PatternRule('abacbc-contract',
	//	A.add(B).mul(C).add(A.mul(B)),
	//	bs => bs.A.mul(bs.B).add(bs.A.mul(bs.C)).add(bs.B.mul(bs.C))
	//),
	// AB+AC+BC expansion: AB + AC + BC -> (A+B)C + AB
	PatternRule('abacbc-expand',
		A.mul(B).add(A.mul(C)).add(B.mul(C)),
		bs => bs.A.add(bs.B).mul(bs.C).add(bs.A.mul(bs.B))
	),
	
	// Cube contraction: A^3 + 3A^2B + 3AB^2 + B^3 -> (A+B)^3
	PatternRule('contract-cube-sum',
		A.pow(3).add(A.pow(2).mul(B).scale(3)).add(A.mul(B.pow(2)).scale(3)).add(B.pow(3)),
		bs => bs.A.add(bs.B).pow(3)
	),
	// Cube contraction: A^3 - 3A^2B + 3AB^2 - B^3 -> (A-B)^3
	PatternRule('contract-cube-diff',
		A.pow(3).sub(A.pow(2).mul(B).scale(3)).add(A.mul(B.pow(2)).scale(3)).sub(B.pow(3)),
		bs => bs.A.sub(bs.B).pow(3)
	)
];

export const trigRules: Rule[] = [
	// sin(A+B) = sin(A)cos(B) + cos(A)sin(B)
	PatternRule('sin-sum',
		symbolic.sin(A.add(B)),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	// sin(A-B) = sin(A)cos(B) - cos(A)sin(B)
	PatternRule('sin-diff',
		symbolic.sin(A.sub(B)),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	// sin(x/2) = sqrt((1-cos(x))/2)
	PatternRule('sin-half-angle',
		symbolic.sin(A.scale(0.5)),
		bs => symbolic.sqrt(one.sub(symbolic.cos(bs.A)).scale(0.5))
	),
	// sin(2x) = 2 * sin(x) * cos(x)
	PatternRule('sin-double-angle',
		symbolic.sin(A.scale(2)),
		bs => symbolic.from(2).mul(symbolic.sin(bs.A).mul(symbolic.cos(bs.A)))
	),


	// cos(A+B) = cos(A)cos(B) - sin(A)sin(B)
	PatternRule('cos-sum',
		symbolic.cos(A.add(B)),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	// cos(A-B) = cos(A)cos(B) + sin(A)sin(B)
	PatternRule('cos-diff',
		symbolic.cos(A.sub(B)),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	// cos(x/2) = sqrt((1+cos(x))/2)
	PatternRule('cos-half-angle',
		symbolic.cos(A.scale(0.5)),
		bs => symbolic.sqrt(one.add(symbolic.cos(bs.A)).scale(0.5))
	),
	// cos(2x) = cos^2(x) - sin^2(x)
	PatternRule('cos-double-angle',
		symbolic.cos(A.scale(2)),
		bs => symbolic.cos(bs.A).pow(2).sub(symbolic.sin(bs.A).pow(2))
	),



	// tan(A+B) = (tan(A) + tan(B)) / (1 - tan(A)tan(B))
	PatternRule('tan-sum',
		symbolic.tan(A.add(B)),
		bs => symbolic.tan(bs.A).add(symbolic.tan(bs.B)).div(one.sub(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	// tan(A-B) = (tan(A) - tan(B)) / (1 + tan(A)tan(B))
	PatternRule('tan-diff',
		symbolic.tan(A.sub(B)),
		bs => symbolic.tan(bs.A).sub(symbolic.tan(bs.B)).div(one.add(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	// tan(x/2) = sin(x) / (1 + cos(x))
	PatternRule('tan-half-angle',
		symbolic.tan(A.scale(0.5)),
		bs => symbolic.sin(bs.A).scale(0.5).div(symbolic.cos(bs.A).scale(0.5))
	),

	// Pythagorean identities
	PatternRule('sin-cos-pythag',
		symbolic.sin(A).pow(2).add(symbolic.cos(A).pow(2)),
		_ => one
	),
	PatternRule('tan-secant-pythag',
		symbolic.tan(A).pow(2).add(one),
		bs => symbolic.cos(bs.A).pow(-2)
	),
	PatternRule('cot-cosecant-pythag',
		one.add(symbolic.tan(A).pow(-2)),
		bs => symbolic.sin(bs.A).pow(-2)
	),
	/*
	PatternRule('sin-cos-product',
		symbolic.sin(A).mul(symbolic.cos(B)),
		bs => symbolic.sin(bs.A.add(bs.B)).mul(symbolic.cos(bs.A.sub(bs.B))).scale(0.5)
	),
	*/
];

export const atan2Rules: Rule[] = [
	// atan2 basic properties
	PatternRule('atan2-to-atan',
		symbolic.atan2(B, A),
		bs => symbolic.atan(bs.B.div(bs.A)),
		bs => !isConst(bs.A) || bs.A.value > 0  // only when x > 0
	),

	// atan2 symmetry
	PatternRule('atan2-negate-both',
		symbolic.atan2(B.neg(), A.neg()),
		bs => symbolic.atan2(bs.B, bs.A)
	),

	// atan2 scale invariance
	PatternRule('atan2-scale-invariance',
		symbolic.atan2(B.mul(C), A.mul(C)),
		bs => symbolic.atan2(bs.B, bs.A),
		bs => !isConst(bs.C) || bs.C.value > 0  // only for positive C
	),

	// atan2 of zero
	PatternRule('atan2-zero-y',
		symbolic.atan2(zero, A),
		_ => zero
	),

	// atan2 special angles
	PatternRule('atan2-x-x',
		symbolic.atan2(A, A),
		_ => pi.scale(1/4)
	),
	PatternRule('atan2-x-neg-x',
		symbolic.atan2(A, A.neg()),
		_ => pi.scale(3/4)
	),
];

export const invTrigRules: Rule[] = [
	// sin(A)cos(B) + cos(A)sin(B) -> sin(A+B)
	PatternRule('sin-sum-compress',
		symbolic.sin(A).mul(symbolic.cos(B)).add(symbolic.cos(A).mul(symbolic.sin(B))),
		bs => symbolic.sin(bs.A.add(bs.B)),
	),
	// sin(x) * cos(x) -> 1/2 * sin(2x)
	PatternRule('double-angle-sin-compress',
		symbolic.sin(A).mul(symbolic.cos(A)),
		bs => symbolic.sin(bs.A.scale(2)).scale(0.5)
	),

	// cos^2(x) - sin^2(x) -> cos(2x)
	PatternRule('double-angle-cos-compress',
		symbolic.cos(A).pow(2).sub(symbolic.sin(A).pow(2)),
		bs => symbolic.cos(bs.A.scale(2))
	),

	/*
	PatternRule( 'sin-cos-product',
		symbolic.sin(A.add(B)).mul(symbolic.cos(A.sub(B))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B))
	),
	*/
];

/*
import {polynomialT} from "./polynomial";


export function resultantParametric<T extends scalar0<T>>(ops: Operators<T>,p: polynomialT<symbolic>, q: polynomialT<symbolic>, varName: string, fixedEnv: Record<string, T> = {}, maxDegree = -1) {
	const coeffAt = (c: symbolic, t: T) => c.evaluateT(ops, { ...fixedEnv, [varName]: t });
	const v = symbolic.variable(varName);

	const pd = p.degree();
	const qd = q.degree();
//	if (pd < 0 || qd < 0)
//		return new polynomialT<U>([]);

	// choose number of samples: default to pd*qd + 1
	//const degGuess = maxDegree > -1 ? maxDegree : Math.max(0, pd * qd);
	//const samples = degGuess + 1;
	const pts: [T, T][] = [];
	for (const x of samples) {
		const a = p.c.map(c => coeffAt(c, x));
		const b = q.c.map(c => coeffAt(c, x));
		const M = sylvesterMatrix(a, b, zero);
		const { swaps } = LUDecomposeBareissPivotT(M, true);
		const r = swaps & 1 ? M[M.length - 1][M.length - 1].neg() : M[M.length - 1][M.length - 1];
		pts.push([x, r]);
	}

	return interpolateT(pts, zero.from(1)); // coefficients numeric low->high
//	return resultantEvalInterp(p, q, v, coeffAt, maxDegree);
}
*/
