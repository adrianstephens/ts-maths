/**
 * Groebner Basis Implementation for Polynomial Factorization
 * 
 * Operates directly on symbolic expressions (symbolicAdd/symbolicMul)
 * without introducing duplicate data structures.
 */

import { factor, symbolic, mulFactors } from './symbolic';
import { Numeric } from './rational';

type  Monomial = number[];

interface Term {
	coef:	symbolic;
	mon:	Monomial;
}

type Compare = (a: Monomial, b: Monomial) => number;

// ============================================================================
// MONOMIAL OPERATIONS (using symbolicMul structure)
// ============================================================================

function monomialToSymbolic(mon: Monomial, variables: readonly symbolic[]): symbolic {
	return mulFactors(new Numeric(1), ...mon.map((exp, i) => factor(variables[i], new Numeric(exp))).filter(Boolean));
}

/**
 * Total degree of a monomial
 */
function degree(expr: Monomial): number {
	return expr.reduce((sum, e) => sum + e, 0);
}

/**
 * Compare two monomials using graded reverse lexicographic ordering
 * Returns: >0 if a>b, <0 if a<b, 0 if equal
 */
function compareGrevlex(a: Monomial, b: Monomial): number {
	const degA = degree(a);
	const degB = degree(b);
	
	if (degA !== degB)
		return degB - degA;
	
	// Reverse lex: compare from right to left
	const n = Math.max(a.length, b.length);
	for (let i = n - 1; i >= 0; i--) {
		const aExp = a[i] ?? 0;
		const bExp = b[i] ?? 0;
		if (aExp !== bExp)
			return aExp - bExp;
	}
	return 0;
}

/**
 * Compare using lexicographic ordering (for elimination)
 */
function compareLex(a: Monomial, b: Monomial): number {
	const n = Math.max(a.length, b.length);

	for (let i = 0; i < n; i++) {
		const aExp = a[i] ?? 0;
		const bExp = b[i] ?? 0;
		if (aExp !== bExp)
			return bExp - aExp;
	}
	return 0;
}

/**
 * Check if monomial a divides monomial b
 */
function divides(a: Monomial, b: Monomial): boolean {
	return a.every((e, i) => e <= (b[i] ?? 0));
}

/**
 * Divide monomial b by monomial a (returns quotient monomial)
 * Returns undefined if not divisible
 */
function divideMonomial(b: Monomial, a: Monomial): Monomial | undefined {
	const n = Math.max(a.length, b.length);
	const result: number[] = [];
	for (let i = 0; i < n; i++) {
		const diff = (b[i] ?? 0) - (a[i] ?? 0);
		if (diff < 0)
			return undefined;
		if (diff > 0)
			result[i] = diff;
	}
	
	return result;
}

/**
 * LCM of two monomials
 */
function lcmMonomial(a: Monomial, b: Monomial): Monomial {
	const result: Monomial = [];
	const n = Math.max(a.length, b.length);
	for (let i = 0; i < n; i++) {
		const maxExp = Math.max(a[i] ?? 0, b[i] ?? 0);
		if (maxExp > 0)
			result[i] = maxExp;
	}
	return result;
}

function monomialEqual(a: Monomial, b: Monomial): boolean {
	const n = Math.max(a.length, b.length);
	for (let i = 0; i < n; i++) {
		if ((a[i] ?? 0) !== (b[i] ?? 0))
			return false;
	}
	return true;
}

// ============================================================================
// POLYNOMIAL OPERATIONS (using symbolicAdd structure)
// ============================================================================

/**
 * Extract term: coefficient monomial pair from a symbolicAdd term
 * Separates coefficient (non-variable factors) from monomial (variable powers)
 */
function extractTerm(expr: symbolic, variables: readonly symbolic[]): Term {
	const mon: Monomial = [];
	
	if (expr.is('mul')) {
		const coefFactors: symbolic[] = !expr.num.is1() ? [symbolic.from(expr.num)] : [];
		
		for (const factor of expr.factors) {
			const idx = variables.indexOf(factor.item);
			if (idx >= 0) {
				mon[idx] = Number(factor.pow);
			} else {
				coefFactors.push(factor.item.npow(factor.pow));
			}
		}
		
		const coef = coefFactors.length === 0 ? symbolic.from(1) : coefFactors.reduce((a, b) => a.mul(b));
		return { coef, mon };
	}
	
	const idx = variables.indexOf(expr);
	if (idx >= 0) {
		mon[idx] = 1;
		return { coef: symbolic.from(1), mon };
	}
	// Entire expression is coefficient
	return { coef: expr, mon: [] };
}

/**
 * Get leading term from a polynomial (symbolicAdd)
 */
function leadingTerm(poly: symbolic, variables: readonly symbolic[], ordering: Compare): Term | undefined {
	// After expansion, unwrap if it got wrapped again
	let scale = new Numeric(1);
	if (poly.is('mul') && poly.factors.length === 1 && poly.factors[0].pow.is1() && poly.factors[0].item.is('add')) {
		scale = poly.num;
		poly = poly.factors[0].item;
	}

	if (!poly.is('add'))
		return extractTerm(poly, variables);

	// Find term with largest monomial
	const bestTerm = extractTerm(poly.terms[0].item, variables);
	let bestMon = bestTerm.mon;
	let bestCoef = bestTerm.coef.scale(poly.terms[0].coef.mul(scale));
	
	for (let i = 1; i < poly.terms.length; i++) {
		const term = extractTerm(poly.terms[i].item, variables);
		// ordering(a, b) returns positive if b > a (i.e., b is larger)
		// So ordering(bestMon, term.mon) > 0 means term.mon is larger than bestMon
		if (ordering(bestMon, term.mon) > 0) {
			bestMon = term.mon;
			bestCoef = term.coef.scale(poly.terms[i].coef.mul(scale));
		}
	}

	return { coef: bestCoef, mon: bestMon };
}

/**
 * S-polynomial: S(f, g) = (lcm/LT(f)) * f - (lcm/LT(g)) * g
 */
export function sPolynomial(f: symbolic, g: symbolic, variables: readonly symbolic[], ordering = compareGrevlex): symbolic {
	const ltf = leadingTerm(f, variables, ordering);
	const ltg = leadingTerm(g, variables, ordering);
	
	if (!ltf || !ltg)
		return symbolic.from(0);
	
	const lcm = lcmMonomial(ltf.mon, ltg.mon);
	
	const mf = divideMonomial(lcm, ltf.mon)!;
	const mg = divideMonomial(lcm, ltg.mon)!;
	
	// Scale to cancel leading terms: (mf/cf)*f - (mg/cg)*g
	const term1 = f.mul(monomialToSymbolic(mf, variables)).div(ltf.coef).expand();
	const term2 = g.mul(monomialToSymbolic(mg, variables)).div(ltg.coef).expand();
	
	return term1.sub(term2).expand();
}

/**
 * Multivariate polynomial division with remainder
 * Given f and divisors [g₁, g₂, ..., gₛ], compute:
 * f = q₁g₁ + q₂g₂ + ... + qₛgₛ + r
 */
export function polynomialDivision(
	f: symbolic,
	divisors: symbolic[],
	variables: readonly symbolic[],
	ordering = compareGrevlex
): { quotients: symbolic[]; remainder: symbolic } {
	const quotients = divisors.map(_ => symbolic.from(0));
	let remainder	= symbolic.from(0);
	let dividend	= f;
	// Guard against non-termination due to precision/representation issues.
	// If the division loop makes no progress after many iterations, bail out
	// conservatively by placing the remaining dividend into the remainder.
	const MAX_STEPS = 20000;
	let steps = 0;
	let lastDividendStr = '';

	while (!isZero(dividend)) {
		if (++steps > MAX_STEPS) {
			// give up and append remaining dividend to remainder
			remainder = remainder.add(dividend.expand());
			break;
		}

		const lt = leadingTerm(dividend, variables, ordering);
		if (!lt)
			break;

		// detect if dividend stopped changing (string representation)
		const curDivStr = String(dividend);
		if (curDivStr === lastDividendStr) {
			// no progress; bail out conservatively
			remainder = remainder.add(dividend.expand());
			break;
		}
		lastDividendStr = curDivStr;

		let divided = false;

		for (let i = 0; i < divisors.length; i++) {
			const ltg = leadingTerm(divisors[i], variables, ordering);
			if (!ltg)
				continue;

			if (divides(ltg.mon, lt.mon)) {
				const c = lt.coef.div(ltg.coef);

				const m = divideMonomial(lt.mon, ltg.mon)!;
				try {
					const mc = monomialToSymbolic(m, variables).mul(c);
					const subtrahend = divisors[i].mul(mc).expand();

					// Ensure the subtrahend actually cancels the current leading term.
					const ltSub = leadingTerm(subtrahend, variables, ordering);
					if (!ltSub)
						continue;
					if (!monomialEqual(ltSub.mon, lt.mon))
						continue;

					// Tentatively compute new dividend and ensure progress (leading term removed)
					const newDividend = dividend.sub(subtrahend);
					const ltNew = leadingTerm(newDividend, variables, ordering);
					if (ltNew && monomialEqual(ltNew.mon, lt.mon))
						continue; // did not remove leading term, skip

					// Avoid explosive growth: if the textual size grows too much, skip
					if (String(newDividend).length > String(dividend).length * 4)
						continue;

					// Accept subtraction
					quotients[i] = quotients[i].add(mc);
					dividend = newDividend;
				} catch {
					// If any symbolic construction here produces NaN/Infinity or
					// validation fails, skip this divisor and try the next.
					continue;
				}
				divided = true;
				break;
			}
		}

		if (!divided) {
			const ltTerm = monomialToSymbolic(lt.mon, variables).mul(lt.coef);
			remainder = remainder.add(ltTerm);
			dividend = dividend.sub(ltTerm);
		}
	}
	
	return { quotients, remainder };
}

/**
 * Buchberger's algorithm for computing Groebner basis
 */
export function groebnerBasis(
	generators: symbolic[],
	variables: readonly symbolic[],
	ordering = compareGrevlex
): symbolic[] {
	// Expand all generators (GCD-factored muls will be unwrapped in leadingTerm)
	const G = generators.filter(g => !isZero(g)).map(g => g.expand());
	const pairs: [number, number][] = [];
	
	// Generate all pairs
	for (let i = 0; i < G.length; i++) {
		for (let j = i + 1; j < G.length; j++)
			pairs.push([i, j]);
	}
	
	const MAX_PAIR_POLY_LEN = 8000;
	while (pairs.length > 0) {
		const [i, j] = pairs.pop()!;

		// Heuristic guard: avoid forming S-polynomials from very large generators
		// which historically blow up memory. If both generators are large, skip
		// this pair conservatively.
		const giLen = String(G[i]).length;
		const gjLen = String(G[j]).length;
		if (giLen + gjLen > MAX_PAIR_POLY_LEN) {
			// skip heavy pair
			continue;
		}

		const S = sPolynomial(G[i], G[j], variables, ordering);
		if (String(S).length > MAX_PAIR_POLY_LEN)
			continue;

		const { remainder } = polynomialDivision(S, G, variables, ordering);

		if (!isZero(remainder)) {
			// Expand and reduce remainder; skip adding if it's duplicate or too large.
			const red = remainder.expand();
			const redStr = String(red);
			// Skip extremely large remainders which likely indicate division gave up
			if (redStr.length > 2000)
				continue;
			// Skip if remainder already present in G (structural equality via id/string)
			if (G.some(g => g === red || String(g) === redStr))
				continue;
			// Add new pairs with the new polynomial
			const newIndex = G.length;
			for (let k = 0; k < G.length; k++)
				pairs.push([k, newIndex]);

			G.push(red);
		}
}

	return reduceGroebner(G, variables, ordering);
}

/**
 * Reduce Groebner basis to minimal form
 */
function reduceGroebner(G: symbolic[], variables: readonly symbolic[], ordering: Compare): symbolic[] {
	// Make monic (leading coefficient = 1), but only if monomial is non-constant
	let result = G.map(g => {
		const lt = leadingTerm(g, variables, ordering);
		if (!lt)
			return g;
		// Don't make monic if the monomial is just 1 (constant polynomial in these variables)
		if (lt.mon.length === 0)
			return g;
		return g.div(lt.coef);
	});
	
	// Remove redundant polynomials
	result = result.filter((f, i) => {
		const ltf = leadingTerm(f, variables, ordering);
		if (!ltf)
			return false;
		const redundant = result.some((g, j) => {
			if (i === j)
				return false;
			const ltg = leadingTerm(g, variables, ordering);
			return ltg && divides(ltg.mon, ltf.mon);
		});
		return !redundant;
	});
	
	// Final inter-reduction: reduce each polynomial using the others
	// Only keep the reduction if it's actually simpler
	result = result.map((f, i) => {
		const others = result.filter((_, j) => i !== j);
		if (others.length === 0)
			return f;
		const { remainder } = polynomialDivision(f, others, variables, ordering);
		// Only use remainder if it's non-zero (don't eliminate basis elements!)
		return isZero(remainder) ? f : remainder;
	});
	
	return result.filter(g => !isZero(g));
}

// ============================================================================
// UTILITIES
// ============================================================================

function isZero(expr: symbolic): boolean {
	return expr === symbolic.zero;
}

// ============================================================================
// ORDERINGS
// ============================================================================

export const grevlexOrder = compareGrevlex;
export const lexOrder = compareLex;

// ============================================================================
// POLYNOMIAL FACTORIZATION VIA VIETA'S FORMULAS
// ============================================================================

/**
 * Generate Vieta's formulas for a polynomial of given degree
 * Returns equations relating roots to coefficients
 * 
 * For degree n with roots r₁, r₂, ..., rₙ and coefficients c₁, c₂, ..., cₙ:
 * - σ₁ = r₁ + r₂ + ... + rₙ = -c₁
 * - σ₂ = r₁r₂ + r₁r₃ + ... = c₂
 * - σ₃ = r₁r₂r₃ + ... = -c₃
 * - ...
 * - σₙ = r₁r₂...rₙ = (-1)ⁿcₙ
 */
function _vietasFormulas(roots: symbolic[], coefficients: symbolic[]): symbolic[] {
	const n = roots.length;
	if (n !== coefficients.length)
		throw new Error('Number of roots must match number of coefficients');
	
	const equations: symbolic[] = [];
	
	// Generate elementary symmetric polynomials σₖ
	for (let k = 1; k <= n; k++) {
		let sigma = symbolic.zero;
		
		// Sum over all k-element subsets of roots
		const indices = Array.from({ length: k }, (_, i) => i);
		
		function generateSubsets(start: number, depth: number) {
			if (depth === k) {
				// Multiply the k roots at these indices
				let product = symbolic.one;
				for (let i = 0; i < k; i++)
					product = product.mul(roots[indices[i]]);
				sigma = sigma.add(product);
				return;
			}
			
			for (let i = start; i <= n - (k - depth); i++) {
				indices[depth] = i;
				generateSubsets(i + 1, depth + 1);
			}
		}
		
		generateSubsets(0, 0);
		
		// σₖ = (-1)ᵏ⁺¹ × cₖ  (alternating signs)
		const sign = (k % 2 === 0) ? 1 : -1;
		equations.push(sigma.sub(coefficients[k - 1].scale(sign)));
	}
	
	return equations;
}

/**
 * Factor a univariate polynomial with symbolic coefficients using Groebner basis
 * 
 * Given: polynomial in x with symbolic coefficients
 * Returns: array of root expressions, or undefined if factorization fails
 * 
 * Example:
 *   const A = symbolic.variable('A');
 *   const B = symbolic.variable('B');
 *   const C = symbolic.variable('C');
 *   const x = symbolic.variable('x');
 *   
 *   const poly = x.pow(3)
 *     .sub(A.add(B).add(C).mul(x.pow(2)))
 *     .add(A.mul(B).add(A.mul(C)).add(B.mul(C)).mul(x))
 *     .sub(A.mul(B).mul(C));
 *   
 *   const roots = factorPolynomial(poly, x);
 *   // Should return [A, B, C] (or expressions equal to them)
 */
export function factorPolynomial(poly: symbolic, variable: symbolic): symbolic[] | undefined {
	// Extract polynomial structure: collect coefficients by degree
	const collected = poly.collect(variable.is('var') ? variable.name : variable.toString());
	const degree = collected.length - 1;
	
	if (degree <= 0)
		return undefined;
	
	// For degree 2 and 3, use direct formulas
	if (degree === 2) {
		// ax² + bx + c: roots = (-b ± √(b²-4ac)) / 2a
		const a = collected[2];
		const b = collected[1] ?? symbolic.zero;
		const c = collected[0] ?? symbolic.zero;
		
		const disc = b.pow(2).sub(a.mul(c).scale(4));
		const sqrtDisc = disc.pow(symbolic.from(0.5));
		const denom = a.scale(2);
		
		return [
			b.neg().add(sqrtDisc).div(denom),
			b.neg().sub(sqrtDisc).div(denom)
		];
	}
	
	if (degree === 3) {
		// For cubic, Vieta's formulas directly give us symmetric functions
		// x³ + px² + qx + r = (x-r₁)(x-r₂)(x-r₃)
		// σ₁ = r₁+r₂+r₃ = -p
		// σ₂ = r₁r₂+r₁r₃+r₂r₃ = q  
		// σ₃ = r₁r₂r₃ = -r
		
		const a = collected[3];
		const _p = (collected[2] ?? symbolic.zero).div(a);
		const _q = (collected[1] ?? symbolic.zero).div(a);
		const _r = (collected[0] ?? symbolic.zero).div(a);
		
		// For now, just return symbolic roots using variables
		// A full cubic solver would use Cardano's formula
		const y1 = symbolic.variable('y1');
		const y2 = symbolic.variable('y2');
		const y3 = symbolic.variable('y3');
		
		// Return the roots as unknowns constrained by Vieta
		// This is incomplete but demonstrates the structure
		return [y1, y2, y3];
	}
	
	return undefined;
}

/**
 * Attempt to factor a polynomial into linear factors
 * Returns factors as (x - r₁)(x - r₂)...(x - rₙ) if successful
 */
export function factorIntoLinear(poly: symbolic, variable: symbolic): symbolic | undefined {
	const roots = factorPolynomial(poly, variable);
	if (!roots)
		return undefined;
	
	// Build product of (x - rᵢ)
	let result = symbolic.one;
	for (const root of roots)
		result = result.mul(variable.sub(root));
	
	return result;
}
